# This is the user-interface definition of a Shiny web application for
# calculating the Australia and New Zealand metal guideline values. It follows
# a similar format to the Fish IBI Calculator (https://mfenz.shinyapps.io/fish-ibi-calculator/)
# You can run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# https://shiny.posit.co/



library(fontawesome)
library(r2symbols)
library(shiny)
library(shinyjs)
library(DT)  ## load after shiny
library(bslib)
library(bsicons)
library(reactable)
library(shinythemes)
library(markdown)

fileInputOnlyButton <- function(..., label="") {
  
  temp <- fileInput(..., label=label)
  temp$children[[1]] <- NULL ## Cut away the label
  ## Cut away the input field (after label is cut, this is position 1 now)
  temp$children[[1]]$children[[2]] <- NULL
  ## Remove input group classes (makes button flat on one side)
  temp$children[[1]]$attribs$class <- NULL
  temp$children[[1]]$children[[1]]$attribs$class <- NULL
  temp
  
}


ui <- fluidPage(
  #theme = shinytheme("flatly"),
  
  useShinyjs(),

  tags$header(id="header",
              tags$table(style="width:100%; background-color:#154c79;",
                         tags$tr(tags$td(style="width:33%"
                                         ),
                                 tags$td(style="width:33%; height:120px", align="center",
                                         h1(id="main-title", style="color:white;",
                                            "ANZG metal DGV tool")
                                         ),
                                 tags$td(style="width:33%"
                                         )
                                 )
                         )
              ),
  
  tags$style(".btn-file {background-color:#376894; border-color:#376894; width:280px}",
             ".fa-arrow-up {color:white}",
             ".fa-arrow-right {font-size:30px}",
             HTML("#data table {width: 800px;}
                             #data td:first-child {
                               width: 10%;
                             }"
                  ),
             ),
  
  page_navbar(id="tabs",
    bg = "#bacdda",
    
    nav_spacer(),
    nav_spacer(),
    nav_spacer(),
    
    # 1. Intro and file upload --------------------------------------------------------------------------
    nav_panel(value="upload-page",
      title = h4(id="page1-title", "1. Upload your file"),
      br(),
      fluidRow(id="welcome-panel",
               column(width=8, align="left", h2(id="welcome-title", "Welcome to the ANZG metal DGV tool"),
                      br(),
                      p(withMathJax(includeMarkdown("text/page-1-description.md")))
                      ),
               column(width=4, align="left",
                      fluidRow(id="upload-panel",
                               column(width=6,
                                      fileInputOnlyButton("target_upload",
                                                          buttonLabel=
                                                            
                                                            div(class="button-inner", h4("Upload CSV file", style="color:white"), icon("arrow-up")),
                                                          
                                                          #buttonLabel=div(class="button-inner", span(id="button-inner-text", h4("Upload CSV file", style="color:white")), icon("arrow-up")),
                                                          accept=c("text/csv", "text/comma-separated-values", ".csv"), width='100%'
                                                          )
                                      ),
                               column(width=6,
                                      actionButton("sample_btn", div(class='button-inner', h4("Use demo table"), icon("forward")),
                                                                     width="280px",
                                                   style="color:white; background-color:#86a3bb; border-color:#86a3bb")
                                      )
                              ),
                      
                      fluidRow(id="upload-info",
                               helpText(includeMarkdown("text/page-1-instructions.md")))
                      )
               ),
      hr(),
      fluidRow(id="about-title",
               # h4("About this calculator"),
               # br(),
               # p(includeMarkdown("text/page-1-about.md")),
               tabsetPanel(
                 tabPanel(h4("Instructions for use"),
                          br(),
                          p(includeMarkdown("text/page-1-about.md"),
                            tags$style(' #tab {margin-bottom:-30px;}')),
                          div(style = "margin-top: -100px"),
                          tags$img(src='reqd_table.png', align = "left", style = "width: 600px"),
                          #    tags$style(' #tab {margin-top:-200px; }')),
                          p(includeMarkdown("text/page-1-about-2.md")),
                          ),
                 tabPanel(h4("About this calculator"),
                          p(includeMarkdown("text/page-1-about-3.md")),
                          br())
                 )
               )
    ),  ## end page 1 nav_panel  --------------------------------  
    nav_spacer(),
    
    nav_panel(value="arrow1",
      title=tags$img(src="icons/arrow.svg")
    ),
    
    nav_spacer(),
    
    # 2. Select options ------------------------------------------------------------------------
    nav_panel(value="select-page",
      title = h4(id="page2-title", "2. Select options"),
      br(),
      fluidRow(id="options-panel",
                 column(width=10, align="left", h2("Please review your data (displayed below) and select options"),
                                      #em("Add some more text here?"),
                        fluidRow(em("Choose the options for your analysis:"),
                                 column(width=5, align = "left",
                                  #     br(),
                                      
                                                    #style="background-color:#f0f1f1;",
                                             checkboxGroupInput(inputId="metals", width="100%",
                                                                label="Which metals do you want to generate guideline values for?",
                                                                choices=c("Copper" = "Cu",
                                                                          "Nickel" = "Ni",
                                                                          "Zinc" = "Zn")
                                             ),
                                             br(), br(), br(),
                                             
                                  checkboxInput("calc_biof", # "",
                                                value = FALSE,
                                                label = HTML('<p 
                    style="position:  relative; top: -36px; left: 30px;">
                  Do you want to calculate bioavailable metals?
                   </p>')
                                                )
                                  # tooltip(checkboxGroupInput(inputId="calc_biof",
                                  #                            width = "100%",
                                  #                            label = "Do you want to calculate bioavailable metals?",
                                  #                            bsicons::bs_icon("info-circle"),
                                  #                            choices = c("Yes"="Yes")
                                  #                            ),
                                  #
                                 # "These can be compared to a fixed guideline value. \nYou'll need to supply metal concentrations",
                                  #placement = "right"
                                  #)
                        ),
                        column(width=5, align = "left",
                               
                                checkboxGroupInput(inputId="pcs", width="100%",
                                                   label="What levels of species protection do you want to include?",
                                                   choices=c("99% protection" = "PC99",
                                                             "95% protection" = "PC95",
                                                             "90% protection" = "PC90",
                                                             "80% protection" = "PC80"),
                                                   selected = "PC95"),
                               #             
                               br(), 
                               
                               ## Uncomment the option below when we know why its breaking
                               ## This displays correctly but then the tool breaks!!
                               
                               # tooltip(checkboxGroupInput(inputId="rcr",
                               #                    width = "100%",
                               #                    label = "Do you want to calculate a Hazard Quotient (HQ)?",
                               #                    choices = c("Yes"="Yes")
                               #                    ),
                               # 
                               # "These indicate where risks to aquatic ecosystems are possible. \nYou'll need to supply metal concentrations",
                               # placement = "right"
                               #    )
                               
                               checkboxGroupInput(inputId="rcr",
                                                                      width = "100%",
                                                                      label = "Do you want to calculate a Hazard Quotient (HQ)?",
                                                                      choices = c("Yes"="Yes")
                                                                      )
                               
                        )
                        ) ## end top fluid row
                 ), ##end width 10 column
               column(width=2, align="right",
                          #             actionButton("check_btn", h4("Check data"), width="280px",
                           #                         style="color:white; background-color:#376894; border-color:#376894"),
                        actionButton("check_btn",  div(class='button-inner', h4("Check input data"), icon("forward")),
                                                     width="280px", style="color:white; background-color:#376894; border-color:#376894"),
                                      br(), br(),
                      helpText("The next step checks your uploaded data against your selections"),
                                                                   ),
                    
                      hr(),
               fluidRow(id="show-data",

                      h4("Your uploaded data are displayed below:"),
                      helpText("Please note that if your data are missing, you’ll need to edit your original table and re-upload."),
                      helpText("Check your original data to make sure all data values are numeric, and do not include * or commas."),
                      br(), br(), br(),
                      fluidRow(id="options-data",
                               column(width=12, align="center",
                                      reactableOutput("data")
                                      )
                               )
                      )
               )
      ),  #end of page 2 nav_panel -----------------------
   
    nav_spacer(),
    
    nav_panel(value="arrow2",
      title=tags$img(src="icons/arrow.svg")
    ),
    
    nav_spacer(),
    
    # 3. Check data page -----------------------------------------------------------------------------
    nav_panel(value="check-page",
              title = h4(id="page3-title", "3. Check data"),
              br(),
             fluidRow(id="check-panel",
             column(width=7,
                    fluidRow(id="check-panel",
                             tags$table(style="width:100%",
                                        tags$tr(tags$td(rowspan=3, style="width:15%; text-align:center", uiOutput("issuesIcon")),
                                                tags$td(style="width:45%; font-size:22pt", uiOutput("issuesText1")),
                                                tags$td(rowspan=3, style="width:40%")
                                                ),
                                        tags$tr(tags$td(style="width:45%", uiOutput("issuesText2"))),
                                        tags$tr(tags$td(style="width:45%", p("(See table below)")))
                                        ),
                             ),
                    
                    ),
                              
             column(width=5, align="right",
                    actionButton("GV_btn", width="280px",
                                  div(class='button-inner', h4("Run the calculator"), icon("forward")),
                                                   style="color:white; background-color:#376894; border-color:#376894"),
                       ),
             ),
             fluidRow(column(width=7, align="left",
                             reactableOutput("issue_table"),
                             )),
             fluidRow(column(width=12, align="left",
                             p(includeMarkdown("text/page-3-instructions.md")))),
            
             fluidRow(id="checked-data",
                      column(width=12, align="left",
                             uiOutput("issueMessage"),
                             br(),
                             reactableOutput("data_checked")
                      )
                      ),
    ), # end of page 3 nav_panel ------------------
    
    nav_spacer(),
    
    nav_panel(value="arrow3",
      title=tags$img(src="icons/arrow.svg")
    ),
    
    nav_spacer(),
    
    # 4. Display & download GVs page-----------------------------------------------------------------
    nav_panel(value="GV-page",
              title=h4(id="page4-title", "4. View & download results"),
              br(),
              fluidRow(id="results-panel",
                       column(width=7, align="left",
                              h2("Guideline values calculated successfully"),
                              br(),
                              uiOutput("resultsText")
                              ),
                       column(width=5, align="right",
                              downloadButton("downloadGVs", h4("Download results"), width="280px",
                                             style="color:white; background-color:#376894; border-color:#376894"),
                              br(), br(),
                              column(width=5, helpText("This exports a comma-separated (csv) file including your input data and results")),
                              br(),
                              downloadButton("downloadssds", "Download SSDs", icon = icon("images"), width="280px",
                                             style="color:white; background-color:#376894; border-color:#376894"),
                              br(), br(),
                              helpText("Note this button currently does nothing")
                              
                              )
                       ),
              
              fluidRow(id="results-table",
                       column(width=12,
                              br(), br(),
                              reactableOutput("GVs"))
                       )
              
        ), # end of page 4 nav_panel

    nav_spacer(),
    ###Page 5 -----------------------------------------------------
    #  nav_panel(value="user-page",
    #           title=h4(id="page4-title", "User guide"),
    #           br(),
    #           fluidRow(id="user-guide",
    #                    h2(id="user-title", "User guide to the ANZG metal DGV tool"),
    #                           br(),
    #                           p(withMathJax(includeMarkdown("text/user-guide.md")))
    #                    )
    # ), # end page 5 nav_panel
    nav_panel(value="user-page",
                         title=h4(id="page4-title", "User guide"),
              page_sidebar(
      h2("User guide to the ANZG metal DGV tool"),
      
      sidebar = sidebar(
        bg = "#bacdda",
        accordion(
          accordion_panel(
            "User guide",
        #    color_by
          ),
          accordion_panel(
            "Worked example",
            "More sections go here"
          )
        )
      ),
      
      accordion(
        open = FALSE,
        accordion_panel(
          "User guide",
          p(withMathJax(includeMarkdown("text/user-guide.md")))
        ),
        accordion_panel(
          "Example",
          p(withMathJax(includeMarkdown("text/worked-example.md")))  
        ),

      )
    )
    ),
    ## Links ------------------------------------------------------------
    # nav_menu(
    #      title = h4("User guide & links"),
    #      align = "right",
    #      nav_item(tags$a("Why use this tool?", href = "https://www.waterquality.gov.au/anz-guidelines/about")),
    #      nav_item(tags$a("Worked example", href = "https://www.waterquality.gov.au/anz-guidelines/about")),
    #      nav_item(tags$a("FAQ", href = "https://www.waterquality.gov.au/anz-guidelines/about")),
    #      nav_item(tags$a("ANZG website", href = "https://www.waterquality.gov.au/anz-guidelines/about"))
    # ),
    
    nav_spacer(),
    nav_spacer(),
    
   ), # end of page_navbar
  
  # Define footer ------------------------------------------------------------------------------------------
  span(
    br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
    tags$footer(id='footer',
                tags$table(style="width:100%; background-color:#154c79;",
                           tags$tr(tags$td(style="width:33%"),
                                   tags$td(style="width:33%; height:80px", align="center", p(style="color:white", "Footer with space for logos etc")),
                                   tags$td(style="width:33%")
                                   )
                           )
                )
    ) # end of footer definition
  
) # end of ui function

