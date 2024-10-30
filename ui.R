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
library(shinycssloaders)
library(DT)  ## load after shiny
library(bslib)
library(bsicons)
library(reactable)
library(shinythemes)
library(markdown)
library(tippy)
library(shinybusy)

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
options(spinner.color="#FF931E", spinner.color.background="#ffffff", spinner.size=2)

ui <- fluidPage(
  #theme = shinytheme("flatly"),
  
  useShinyjs(),

  tags$header(id="header",
              tags$table(style="width:100%; background-color:#154c79;",
                         tags$tr(tags$td(style="width:33%"
                                         ),
                                 tags$td(style="width:33%; height:120px", align="center",
                                         h1(id="main-title", style="color:white;",
                                            "ANZG metal freshwater guideline value calculator")
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
<<<<<<< HEAD
      title = h5(id="page1-title", "1. Upload your file"),
     # br(),
      fluidRow(id="welcome-panel",
               column(width=8, style="padding-right:55px", align="left", h2(id="welcome-title", 
                                                "Welcome to the ANZG metal bioavailability-adjusted guideline values (BAGVs) calculator"),
=======
      title = h4(id="page1-title", "1. Upload your file"),
      br(),
      fluidRow(id="welcome-panel",
               column(width=8, align="left", h2(id="welcome-title", 
                                                "Welcome to the ANZG bioavailability-based metals default guideline values (DGVs) tool"),
>>>>>>> d0c163b7deb6556cc95d69c332fa7854ef1751bd
                      br(),
                      span("This tool estimates potential risks to freshwater aquatic 
                      environments posed by ", strong("copper"), " and ", strong("nickel"), " after considering bioavailability",
                      tooltip(bs_icon("info-circle"), 
                              "the concentration an organism in the water column “experiences”, accounting for metal speciation and competition",
                              placement = "bottom"),".",
                      br(),  br(),
                      "This tool has been developed to assist in implementation of the ", 
                      tags$a("ANZG",  href="https://www.waterquality.gov.au/anz-guidelines", target="_blank"),
                      "toxicant guideline values (GVs) within a tiered risk assessment framework.",
                      tooltip(bs_icon("info-circle"), "add a link", placement = "bottom"),
                      br(), br(),
                      strong("Use the tool when your dissolved metal concentrations ", em("exceed"), 
                      "the tier 1 DGVs"), 
                      popover(bs_icon("info-circle"), title = "Tier 1 DGVs", #tableOutput("DGVs")
                              includeMarkdown("text/tier1-dgvs.md"),
                              options = list(container = "body")
                              ),
                      br(), 
                      p(includeMarkdown("text/page-1-description.md")), 
                      "If you  include metal concentrations, the tool can also 
                      estimate the bioavailable fraction of the metal",
                      tooltip(bs_icon("info-circle"), 
                              "used to compare to Tier 1 DGVs", 
                              placement = "bottom"),
                      ", and calculate a hazard quotient",
                      tooltip(bs_icon("info-circle"), 
                              "an indication of risk to aquatic organisms, metal concentration divided by BAGV", 
                              placement = "bottom"), 
                      br(),
                    "Upload a comma-separated table (csv)", a(" (template provided if needed) ",target="_blank",href="template.csv"), 
                    "with your water chemistry data or test the 
                    calculator with the example dataset provided.", br(), br(), 
                    "For more instructions see ", a("the user guide with worked examples",target="_blank",href="myfile.pdf"),
                    "and the ", strong("FAQs"), " below.", br(),
                    "Details on the GVs, the tiered approach to implementation" ,
                    " and a description of the science underpinning the tool's development is provided in ",
                    a("this document.",target="_blank",href="myfile.pdf"))
                      
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
                          tags$img(src='reqd_table2.png', align = "left", style = "width: 600px"),
                          #    tags$style(' #tab {margin-top:-200px; }')),
                          p(includeMarkdown("text/page-1-about-2.md")),
                          ),
                 tabPanel(h4("Frequently asked questions"),
                          p(includeMarkdown("text/faqs.md")),
                          br()),
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
<<<<<<< HEAD
      title = h5(id="page2-title", "2. Select options"),
    #  br(),
=======
      title = h4(id="page2-title", "2. Select options"),
      br(),
>>>>>>> d0c163b7deb6556cc95d69c332fa7854ef1751bd
      fluidRow(id="options-panel",
                 column(width=10, align="left", h2("Review your data (displayed below) and select options for your analysis"),
                                                         fluidRow(#em("Choose the options for your analysis:"),
                                 column(width=5, align = "left",
                                  #     br(),
                                      
                                                    #style="background-color:#f0f1f1;",
                                                            checkboxGroupInput(inputId="metals", width="80%",
                                                                label=span("Which metals do you want to generate BAGVs for?",
                                                                           popover(bs_icon("info-circle"),
                                                                                   "Zinc will be available in the future")),
                                                                choices=c("Copper" = "Cu",
                                                                          "Nickel" = "Ni")
                                                                          #"Zinc" = "Zn")
                                             ), 
                                           #  br(),#  br(),
                                  checkboxGroupInput(inputId="pcs", width="80%",
                                                             label=span("What levels of species protection do you want to include?",
                                                                        popover(bs_icon("info-circle"), 
                                                                      "95% protection is the default for slightly-moderately disturbed sites. \nSee ANZG website for more details")),        
                                                             choices=c("99% protection" = "PC99",
                                                                       "95% protection" = "PC95",
                                                                       "90% protection" = "PC90",
                                                                       "80% protection" = "PC80"),
                                                             selected = "PC95"
                                         
                                  ),
                                  br(),
                                  checkboxInput("rcr",
                                                value = FALSE,
                                                label = span(HTML('<p 
                                                                  style="position:  relative; top: -36px; left: 30px;"> 
                                                                  Do you want to calculate Hazard Quotients (HQs)? '
                                                                  ), 
                                                            popover(bs_icon("info-circle"),
                                                                    "These indicate where risks to aquatic ecosystems are possible. \nYou'll need to supply metal concentrations",
                               )
                                      )
                                         )),
                        column(width=5, align = "left",
                               br(),
                                checkboxInput("calc_biof",
                                                     value = FALSE,
                                                     label = span(HTML('<p 
                                                             style="position:  relative; top: -36px; left: 30px;">
                                                           Do you want to estimate the bioavailable metal concentration?'
                                                           ),
                                                          popover(bs_icon("info-circle"),
                                                          "These can be compared to the tier 1 DGV. 
                                        \nYou'll need to supply metal concentrations 
                                        \nand select the country where you are applying these"))        
                                                                  
                                                     ),

                               radioButtons("country", label = span("Select your country for application:", 
                                                                    popover(bs_icon("info-circle"),
                                         "Country of application is required for comparing bioavailable metals to tier 1 DGVs \n 
                                         Only one country can be selected at a time")),                           
                                                    choices = c("Australia" = "aus", "New Zealand" = "nz")),
                                       
                               br(), 
                               
                               
                      )
                        ) ## end top fluid row
                 ), ##end width 10 column
               column(width=2, align="right",
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
<<<<<<< HEAD
              title = h5(id="page3-title", "3. Check data"),
      #        br(),
=======
              title = h4(id="page3-title", "3. Check data"),
              br(),
>>>>>>> d0c163b7deb6556cc95d69c332fa7854ef1751bd
             fluidRow(id="check-panel",
             column(width=7,
                    fluidRow(id="check-panel",
                             tags$table(style="width:100%",
<<<<<<< HEAD
                                        tags$tr(tags$td(rowspan=3, style="width:15%; text-align:center", 
                                                        uiOutput("issuesIcon")),
=======
                                        tags$tr(tags$td(style="width:45%", p("Note: for large data sets this may take some time to display"))),

                                        tags$tr(tags$td(rowspan=3, style="width:15%; text-align:center", uiOutput("issuesIcon")),
>>>>>>> d0c163b7deb6556cc95d69c332fa7854ef1751bd
                                                tags$td(style="width:45%; font-size:22pt", uiOutput("issuesText1")),
                                                tags$td(rowspan=3, style="width:40%")
                                                ),
                                        tags$tr(tags$td(style="width:45%", 
                                                        tooltip(uiOutput("issuesText2"),
                                                                "If there are errors, check for typos & capitals in heading names"))), 
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
                             withSpinner(reactableOutput("issue_table"), type =5)
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
             add_busy_spinner(spin = "fading-circle", color = "#FF931E")
    ), # end of page 3 nav_panel ------------------
    
    nav_spacer(),
    
    nav_panel(value="arrow3",
      title=tags$img(src="icons/arrow.svg")
    ),
    
    nav_spacer(),
    
    # 4. Display & download GVs page-----------------------------------------------------------------
    nav_panel(value="GV-page",
<<<<<<< HEAD
              title=h5(id="page4-title", "4. View & download results"),
       #       br(),
=======
              title=h4(id="page4-title", "4. View & download results"),
              br(),
>>>>>>> d0c163b7deb6556cc95d69c332fa7854ef1751bd
              fluidRow(id="results-panel",
                       column(width=7, align="left",
                              h2("Guideline values calculated successfully"),
                              br(),
                              uiOutput("resultsText")
                              ),
                       column(width=5, align="right",
                              downloadButton("downloadGVs", h4("Download results"), width="280px",
                                             style="color:white; background-color:#376894; border-color:#376894"),
                              br(), #br(),
                              column(width=5, helpText("This exports a comma-separated (csv) file including your input data and results")),
                              br(),
                              
                              # actionButton("SSDplots", width="280px",
                              #              div(class='button-inner', h4("Generate & download SSD plots")),
                              #              style="color:white; background-color:#376894; border-color:#376894"),
                              #br(),
                              # conditionalPanel(
                              #   condition = "message" == 'complete',
                              #   #"false", # always hide the download button
                              downloadButton("downloadssds", "Download SSDs", icon = icon("images"), width="280px",
                                              style="color:white; background-color:#376894; border-color:#376894"),
                              #),
                              # useShinyjs(),
                              # 
                              #   downloadButton("downloadPlots")
                              # ),
                              # actionButton("button", "MakePlots"),
                              # #gt_output(outputId = "table"),
                              #br(), br(),
                              column(width=5, helpText("Generate species sensitivity distribution (SSD) plots & download in a zip file",
                                                       br(), "This takes ~1-2 mins for 10 rows of data")),
                              #helpText("This exports a zip file containing species sensitivity distribution (SSD) plots",
                              #         br(), 
                              #         "Expect this to take around 1-2 minutes for every 10 rows of data")
                              
                              )
                       ),
              
              fluidRow(id="results-table",
                       column(width=12,
                              br(), br(),
                              withSpinner(reactableOutput("GVs"), type =5),
                              helpText("The columns are resizeable - hover over border in the header row & drag to resize"))
                       )
              
        ), # end of page 4 nav_panel

    nav_spacer(),
<<<<<<< HEAD
    #5. User guide and documents-----------------------------------------------------
nav_panel(value="user-page",
          title=h4(id="page4-title", "User guide"),
=======
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
>>>>>>> d0c163b7deb6556cc95d69c332fa7854ef1751bd

          h2("User guide to the ANZG metal GV tool"),
          p("This app",strong("calculates chronic default guideline values for copper, nickel and zinc"),
          " as derived for Australian and New Zealand guidelines for marine and fresh water."),
          em("Hint: Find and click the info icons throughout the app to find more information on a particular input."),
            
          accordion(
            open = FALSE,
            accordion_panel(
              "1. Upload your file",
              p(withMathJax(includeMarkdown("text/user-guide1.md")))
            ),
            accordion_panel(
              "2. Select options",
              p(withMathJax(includeMarkdown("text/user-guide2.md")))
            ),
            accordion_panel(
              "3. Check data",
              p(withMathJax(includeMarkdown("text/user-guide3.md")))
            ),
            accordion_panel(
              "4. View & download results",
              p(withMathJax(includeMarkdown("text/user-guide4.md")))
            ),
            # accordion_panel(
            #   "Worked example",
            #   p(withMathJax(includeMarkdown("text/worked-example.md")))
            # )
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
    
    nav_spacer()

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

