# This is the server logic of a Shiny web application for calculating the
# Australia and New Zealand metal guideline values. It follows a similar format
# to the Fish IBI Calculator (https://mfenz.shinyapps.io/fish-ibi-calculator/)
# You can run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# https://shiny.posit.co/


#library(formattable)
library(reactable)
library(shiny)
library(shinyjs)
library(DT)  ## load DT after shiny so that the DT functions are used
library(tidyverse)
source("check_data.R")
source("calc_GVs.R")
source("plot_SSDs.R")

server <- function(input, output, session) {
  
     #  showModal(modalDialog(
     # #   title = "Important message",
     #    "This app has not been optimised for use on mobile devices",
     #    easyClose = TRUE
     #  ))
  
  # Disable arrow tabs ------------------------------------------------------------------------------------
  
  shinyjs::disable(selector = '.navbar-nav a[data-value="arrow1"')
  shinyjs::disable(selector = '.navbar-nav a[data-value="arrow2"')
  shinyjs::disable(selector = '.navbar-nav a[data-value="arrow3"')
  
  # Initially disable check data action button -------------------------------------------------------------
  
  shinyjs::disable("check_btn")
  
  # Initialize output variables for data and GVs
  
  df <- NULL                                                                     # original data
  df_checked <- NULL                                                             # checked and processed data
 
  GVs <- NULL                                                                    # list of outputs returned from calc_GVs function
  results <- NULL                                                                # GV results dataframe
  plots <- NULL                                                                  # list of SSD plots
  
  # Load dataset when file uploaded ------------------------------------------------------------------------
  
  observe({
    
    # If no file uploaded, return NULL
    
    inFile = input$target_upload
    if (is.null(inFile)){
      return (NULL)
    }
    
    # Otherwise, load the file and render data table
    
    fpath = inFile$datapath
    
    msg = tryCatch({
      df <<- utils::read.csv(fpath, header=TRUE, stringsAsFactors=FALSE, fileEncoding="UTF-8")
    },error=function(e) e, warning=function(w) w)
    
    if (is(msg, "warning")){
      df <<- utils::read.csv(fpath, header=TRUE, stringsAsFactors=FALSE, fileEncoding="WINDOWS-1252")
    }
    
    output$data = renderReactable({
      reactable(df, resizable=TRUE, showPageSizeOptions=TRUE, showPagination=TRUE,
                bordered=TRUE, wrap=FALSE,
                defaultColDef=colDef(format=colFormat(digits=1),
                                     headerStyle=list(background="#bacdda")))
    })
    
    # Update navbarPanel
    
    updateNavbarPage(session, "tabs", selected="select-page")
    
  }) # end observeEvent for upload data
  
  
  # Load example dataset when "Use demo table" action button pressed ----------------------------------------
  
  observeEvent(input$sample_btn, {
    
    df <<- NULL
    df <<- utils::read.csv("data/exampleinputdata.csv", header=TRUE, stringsAsFactors=FALSE)
    
    output$data = renderReactable({
      reactable(df, resizable=TRUE, showPageSizeOptions=TRUE, showPagination=TRUE,
                bordered=TRUE, wrap=FALSE,
                defaultColDef=colDef(format=colFormat(digits=1),
                                     headerStyle=list(background="#bacdda")))
    })
    
    updateNavbarPage(session, "tabs", selected="select-page")
    
  })
  
  # Enable / disable check data button based on metal selection
  
  observe({
    if (!is.null(input$metals)) {
      shinyjs::enable("check_btn")
    
    } else {
      shinyjs::disable("check_btn")
    }
    
  }) # end observeEvent when loading demo data
  
  # Check data when "check data" action button pressed---------------------------
  
  observeEvent(input$check_btn, {
    
    # Get selected options
    
    GV_options = list("metals"=input$metals, "calc_biof"=input$calc_biof, "rcr" = input$rcr)
    
    # Call function to check the data
    
    x = check_data(df, GV_options)
    issues = x$issue_df                                                          # issues dataframe
    cols_in = x$cols_in                                                          # required columns that are in the data
    df_checked <<- x$df_checked                                                  # data, may include new column with Hardness calculated from Ca and Mg if applicable
    
    Nerrs = nrow(issues[which(issues$type=="error"),])                           # number of issues identified as errors
    Nwarn = nrow(issues[which(issues$type=="warning"),])                         # number of issues identified as warnings
    
    # Enable or disable calculate GV button based on whether any issues have
    # been identified
    
    if (Nerrs > 0) {
      shinyjs::disable("GV_btn")
    } else {
      shinyjs::enable("GV_btn")
    }
    
    # Render icon and text based on whether any issues have been identified
    
    output$issuesIcon <- renderText({
      if (Nerrs > 0) {
        as.character(icon("exclamation-triangle", style="font-size:100px; color:#ad0205"))
        
      } else if (Nwarn > 0) {
        as.character(icon("exclamation-triangle", style="font-size:100px; color:#e86b0c"))
        
      } else {
        as.character(icon("check-square", style="font-size:100px; color:#70b94a"))
      }
    
    })
    
    output$issuesText1 <- renderText({
      if (nrow(issues) > 0){
        sprintf("%i issues were found", nrow(issues))
      } else{
        sprintf("No issues were found")
      }
    })
    
    output$issuesText2 <- renderText({
      sprintf("%i errors and %i warnings", Nerrs, Nwarn)
    })
    
    # Display issue table
    
    issue_summary <- data.frame(Issue = issues[order(issues$message),"message"])  #order errors first
    
    issue_summary <- issue_summary |>
      dplyr::group_by(Issue) |>
      dplyr::summarise(N = n()) 
      
    output$issue_table = renderReactable({
      if (nrow(issues) > 0) {
        reactable(data.frame(Issue=paste0(issue_summary$Issue, " (", issue_summary$N, ")")),
                  compact=TRUE)
      }
    })
    
    # Display data in table
    
    msg = ""                                                                     # warning or error message to display
    
    # Create a lookup copy of df_checked for reference with table formatting
    # If any issues were found, replace the corresponding cell in the lookup
    # dataframe with the issue type (i.e., error/warning)
    
    lookup = df
    
    #lookup = df_checked
    if (nrow(issues) > 0) {
      for (i in c(1:nrow(issues))) {
        ir = issues[i,"row"]                                                     # index of issue row in data
        ic = cols_in[issues[i,"col"]]                                            # name of issue column in data
        
        if (ir > 0) {
          lookup[ir,ic] = issues[i,"type"]
        }
      }
    }
    
    styleFn <- function(value, index, name) {
      
      color <- "white"
      
      # Set background color to blue if the column is in the list of required
      # columns that are in the dataset based on the options selected in
      # page 2 of the ui
      
      if (name %in% cols_in) {
        color <- "#eef4f6"
      }
      
      # Set background color to red for errors and orange for warnings
      
      if (!is.na(lookup[index,name])) {
        if (lookup[index,name] == "error") {
          list(fontWeight = "bold", color <- "#fecdcd")
         } else if (lookup[index,name] == "warning") {
           fontWeight = "bold"
           color <- "#ffebcc"

        }
      }
      
      list(background=color)
    }
    
    colDefList <- list(
      reactable::colDef(style=styleFn,
                        format=colFormat(digits=1),
                        headerStyle=list(background="#bacdda"))
    )
    
    colDefList <- rep(colDefList, ncol(df))
    names(colDefList) <- names(df)
    
    output$data_checked = renderReactable({
      reactable(df, resizable=TRUE, showPageSizeOptions=TRUE,
                showPagination=TRUE, bordered=TRUE, wrap=FALSE,
                columns=colDefList)
    })
    
    output$issueMessage <- renderText({
      sprintf("%s", msg)
    })
    
    updateNavbarPage(session, "tabs", selected="check-page")
    
  }) # end observeEvent for check data
  
  # Calculate GVs when "Calculate GVs" action button pressed  -----------------------------------------------
  
  observeEvent(input$GV_btn, {
    
    show_modal_spinner(spin = "hollow-dots", color ="#FF931E") # show the modal window
    
    # NOTE: NEED A PROGRESS BAR OR SOME OTHER INDICATOR
    
    updateNavbarPage(session, "tabs", selected="GV-page")
    
    # Get selected options
    
    GV_options = list("metals"=input$metals,
                      "calc_biof"=input$calc_biof,
                      "pcs"=input$pcs,
                      "rcr"=input$rcr,
                      "country"=input$country)
    
    # Call function to calculate GVs
    
    GVs <<- calc_GVs(df_checked, GV_options)
    results <<- GVs$results
    
    # Overwrite original data in results so that issues removed as part of
    # checking (e.g. non-numeric values) are preserved when the results are
    # displayed and/or downloaded
    
    results[,match(names(df),names(results))] <<- df
    
   # plots <<- GVs$plots
    remove_modal_spinner() # remove it when done
    
    # Update ui
    
    output$resultsHeading = renderUI({
      if (!is.null(GVs)){
        h2("Guideline values calculated successfully")
      }
    })
     
    output$resultsText = renderUI({
      for (i in c(1:nrow(GVs$summary))) {
        GVs$summary[i,"message"] = paste(GVs$summary[i,"metal"],
                                         " DGVs were calculated for ",
                                         GVs$summary[i,"nGVs"],
                                         " data rows (",
                                         GVs$summary[i,"nExcluded"],
                                         " data row/rows were excluded)<br>", sep="")
      }
      HTML(paste(GVs$summary[,"message"], collapse=""))
      
    })
    
    styleFn <- function(value, index, name) {
      
      # Set default font colour to black
      
      color <- "black"
      fontWeight <- "normal"
      
      # Set font colour to red if the column is a hazard quotient and the
      # value is greater than 1
      
      if (!is.na(value)) {
        if (grepl("HQ",name) & value > 1) {
          color <- "#e00000"
          fontWeight <- "bold"
        }
      }
      
      list(color=color, fontWeight=fontWeight)
    }
    
    colDefList <- list(
      reactable::colDef(style=styleFn,
                        format=colFormat(digits=1),
                        headerStyle=list(background="#bacdda"))
    )
    
    colDefList <- rep(colDefList, ncol(results))
    names(colDefList) <- names(results)
    
    output$GVs = renderReactable({
      reactable(results, resizable=TRUE, showPageSizeOptions=TRUE,
                showPagination=TRUE, bordered=TRUE, wrap=FALSE,
                columns=colDefList)
    })
    
  })
  
  ## Page 4----------------------------
  # Summary of results 
  #no.CuGVs <- length(results$CuPC95)
  
  output$results_summary <- renderText({
     paste("Guideline values calculated for ", input$metals)
   })
   
  
  # Button to download data

  output$downloadGVs <- downloadHandler(
    
    filename = function() {
      paste("MyDGVs-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(GVs$results, file, row.names=FALSE)
    }
  )

  # Button that makes SSD plots - commented out as combining with download button for now
  
  # observeEvent(input$SSDplots, { 
  #   #add_busy_spinner(spin = "hollow-dots", color ="#FF931E") # this one doesn't show up for some reason
  #   show_modal_spinner(spin = "hollow-dots", color ="#FF931E")
  #   # Get selected options
  #   
  #   GV_options = list("metals"=input$metals)
  #   
  #   # Call function to create the plots
  #   
  #   CompletedPlots <<- plot_SSDs(df_checked, GV_options)
  #   plots <<- CompletedPlots$plots
  #   remove_modal_spinner() # remove it when plots created
  #   
  #   # Then need to make the download button visible
  #   # runjs("$('#downloadData')[0].click();") # DOWNLOAD BUTTON
  # 
  #   })
  
  # Button to download SSD plots
  
  output$downloadssds <- downloadHandler(
    
    filename = function() {
      paste0("MySSDs-", Sys.Date(), ".zip")
    },
    content = function(file) {
      
      show_modal_spinner(spin = "hollow-dots", color ="#FF931E")
      
      # Call function to create the plots
      
      GV_options = list("metals"=input$metals)
      CompletedPlots <<- plot_SSDs(df_checked, GV_options)
      plots <<- CompletedPlots$plots
      
      remove_modal_spinner()                                   # remove spinner when plots created
      
      # Create temporary directory and save plots to file
      
      temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
      dir.create(temp_directory)
      
      pnames = paste0(names(plots), ".png")
      for (i in seq_along(plots)) {
        ggsave(file.path(temp_directory, pnames[i]), plots[[i]], "png")
      }
      
      # Zip file and save to user specified location
      
      zip::zip(
        zipfile = file,
        files = dir(temp_directory),
        root = temp_directory
      )
      
      unlink(temp_directory, recursive = TRUE)
    },
    contentType = "application/zip"
  )


}