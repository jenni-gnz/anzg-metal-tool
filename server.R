# This is the server logic of a Shiny web application for calculating the
# Australia and New Zealand metal guideline values. It follows a similar format
# to the Fish IBI Calculator (https://mfenz.shinyapps.io/fish-ibi-calculator/)
# You can run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# https://shiny.posit.co/


library(formattable)
library(reactable)
library(shiny)
library(shinyjs)
library(DT)  ## load DT after shiny so that the DT functions are used

source("check_data.R")
source("calc_GVs.R")


server <- function(input, output, session) {
  
      showModal(modalDialog(
     #   title = "Important message",
        "This app has not been optimised for use on mobile devices",
        easyClose = TRUE
      ))
  
  # Disable arrow tabs ------------------------------------------------------------------------------------
  
  shinyjs::disable(selector = '.navbar-nav a[data-value="arrow1"')
  shinyjs::disable(selector = '.navbar-nav a[data-value="arrow2"')
  shinyjs::disable(selector = '.navbar-nav a[data-value="arrow3"')
  
  # Initially disable check data action button -------------------------------------------------------------
  
  shinyjs::disable("check_btn")
  
  # Initialize dataframes for data and GVs
  
  df = NULL
  results = NULL
  
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
      df <<- read.csv(fpath, header=TRUE, stringsAsFactors=FALSE, fileEncoding="UTF-8")
    },error=function(e) e, warning=function(w) w)
    
    if (is(msg, "warning")){
      df <<- read.csv(fpath, header=TRUE, stringsAsFactors=FALSE, fileEncoding="WINDOWS-1252")
    }
    
    output$data = renderReactable({
      reactable(df, resizable=TRUE, showPageSizeOptions=TRUE, showPagination=TRUE,
                bordered=TRUE,
                defaultColDef=colDef(format=colFormat(digits=1),
                                     headerStyle=list(background="#bacdda")))
    })
    
    # Update navbarPanel
    
    updateNavbarPage(session, "tabs", selected="select-page")
    
  }) # end observeEvent for upload data
  
  
  # Load example dataset when "Use demo table" action button pressed ----------------------------------------
  
  observeEvent(input$sample_btn, {
    
    df <<- NULL
    df <<- read.csv("data/exampleinputdata.csv", header=TRUE, stringsAsFactors=FALSE)
    
    output$data = renderReactable({
      reactable(df, resizable=TRUE, showPageSizeOptions=TRUE, showPagination=TRUE,
                bordered=TRUE,
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
    
  }) # end obsereEvent when loading demo data ---------------------------------------------------------------
  
  # Check data when "check data" action button pressed
  
  observeEvent(input$check_btn, {
    
    # Get selected options
    
    GV_options = list("metals"=input$metals, "calc_biof"=input$calc_biof)
    
    # Call function to check the data
    
    x = check_data(df, GV_options)
    issues = x$issue_df                                                          # issues dataframe
    cols_in = x$cols_in                                                          # required columns that are in the data
    
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
    
    output$issue_table = renderReactable({
      if (nrow(issues) > 0) {
        reactable(data.frame(Issue=issues[order(issues$message),"message"]),
                  compact=TRUE)
      }
    })
    
    # Display data in table
    
    df_checked = df
    msg = ""                                                                     # warning or error message to display
    
    # Create a lookup copy of df_checked for reference with table formatting
    # If any issues were found, replace the corresponding cell in the lookup
    # dataframe with the issue type (i.e., error/warning)
    
    lookup = df_checked
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
          color <- "#fecdcd"
        } else if (lookup[index,name] == "warning") {
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
    
    colDefList <- rep(colDefList, ncol(df_checked))
    names(colDefList) <- names(df_checked)
    
    output$data_checked = renderReactable({
      reactable(df_checked, resizable=TRUE, showPageSizeOptions=TRUE,
                showPagination=TRUE, bordered=TRUE,
                columns=colDefList)
    })
    
    output$issueMessage <- renderText({
      sprintf("%s", msg)
    })
    
    
    # NEED TO DISABLE CALCULATE BUTTON UNLESS DATA OK...
    
    updateNavbarPage(session, "tabs", selected="check-page")
    
  }) # end observeEvent for check data
  
  # Calculate GVs when "Calculate GVs" action button pressed  -----------------------------------------------
  
  observeEvent(input$GV_btn, {
    
    # NOTE: NEED A PROGRESS BAR OR SOME OTHER INDICATOR
    
    updateNavbarPage(session, "tabs", selected="GV-page")
    
    # Get selected options
    
    if (is.null(input$rcr)) {
      rcr_option = FALSE
      
    } else if (input$rcr=="Yes") {
      rcr_option = TRUE
    }
    
    GV_options = list("metals"=input$metals,
                      "calc_biof"=input$calc_biof,
                      "pcs"=input$pcs,
                      "rcr"=rcr_option)
    
    GVs <<- calc_GVs(df, GV_options)
    
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
    
    cols = names(results[grepl("PC", names(results)) | grepl("Bio", names(results)) | grepl("HQ", names(results))])
    hq_cols = names(results[grepl("HQ", names(results))])
    
    output$GVs = renderReactable({
      reactable(GVs$results, resizable=TRUE, showPageSizeOptions=TRUE,
                showPagination=TRUE, bordered=TRUE, wrap=FALSE,
                defaultColDef=colDef(format=colFormat(digits=1),
                                     headerStyle=list(background="#bacdda")))
    })
    
    
    
    # output$GVs = renderDT({
    #   datatable(results) %>% formatRound(cols, 1) %>% 
    #      formatStyle(hq_cols, color = styleInterval(1, c('black', 'red')),
    #                                                                     fontWeight = 'bold')
    # }, options=list(scrollX=TRUE, bFilter=0))  #, scrollY = TRUE

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
      # https://shiny.posit.co/r/articles/build/download/
    }
  )

###Might use this?---
#   output$mytable = DT::renderDataTable({
#     results
#   })

  # Button to download ssds
#  mydata <- list(cars,pressure,airquality)
#  nplots <- length(mydata)
# 
#   observeEvent(input$downloadssds, {
#     lapply(1:nplots, function(i){
#       ggsave(paste0("yplot",i,".png"), plot(mydata[[i]]))
#     })
#   }, ignoreInit = TRUE)

  
output$downloadssds <- downloadHandler(
    
     filename = function() {
       paste(input$metal, input$row, Sys.Date(), ".png", sep="")
     },
     content = function(file) {
       png(file, width = 980, height = 400, units = "px", pointsize = 12,
       bg = "white", res = NA)
       ## Need to replace this dummy plot with a call to the ssd plot function
       x =  seq(1,10, by=1)
       y = seq(2,20, by= 2)
       top6.plot <- plot(x ~ y)
print(top6.plot)
dev.off()

### end replacement here?
},
contentType = 'image/png'
)

  
  
  
  
  
}