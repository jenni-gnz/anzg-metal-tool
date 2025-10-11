#######################################################################
#              ********  Cu, Zn & Ni DGV adjuster  *********    
#######################################################################
# Created: May 2024
# Authors: Jenni Gadd, Sharleen Yalden

# Edited to work with R Shiny tool Oct 2024

plot_SSDs <- function(df, options, updateProgress=NULL){
  
  #####################################################################
  ## Libraries and data files needed
  library(tidyverse)                   ## Required
  library(ssdtools)                    ## Required
  library(latex2exp)                   ## Required for greek numbers
  options(scipen = 8)                  ## To avoid scientific numbers
  
  # Read coefficients and species data ################################
  
  metals = options$metals
  pcs = options$pcs
  
  # pcs_all = c("PC99", "PC95", "PC90", "PC80")
  # pcs_vals_all = c(1, 5, 10, 20)
  # 
  # pcs_vals = pcs_vals_all[pcs_all %in% pcs]
  
  # Zinc
  
  if ("Zn" %in% metals){
    load(file="data/ZnMLRcoeffs.Rdata")
    load(file="data/ZnSSD.Rdata")
  }
  
  # Nickel
  
  if ("Ni" %in% metals){
    load(file="data/NiSSD.Rdata")
    load(file="data/NiMLR.coeffs.Rdata")
  }
 
  plots = list()                                                                 # list to hold SSD plots 
  message = list()                                                               # list to hold message that says its completed
  
  # Calculate total number of rows for progress updates
  
  Nmetals = length(metals)                                                       # number of metals to generate SSD plots for (excluding copper)
  if ("Cu" %in% metals) {
    Nmetals = Nmetals - 1
  }
  
  Nrows = nrow(df)                                                               # number of data rows
  NrowsTotal = Nmetals*Nrows                                                     # number of data rows to calculate for all metals
  df <- df |> dplyr::mutate(myrow=row_number())                                  # used to update the progress bar
  N = 0                                                                          # running indicator for progress bar
  
  ######################################################################
   # Nickel, with MLR adjustment
  
  DoNiSSDPlots <- function(input, sens=NiSSD.df, tMLR=NiMLR.coeffs){
    
    if (is.function(updateProgress)) {
      text <- paste0("Nickel, row ", input$myrow, "/", Nrows)
      updateProgress(value=N, detail=text)
      N <<- N + 1/NrowsTotal
    }
    
    # Check input data. If data is missing do nothing
    
    if (is.na(input$DOC) | is.na(input$pH) | is.na(input$Calcium) |is.na(input$Magnesium)) {
      
    ## dont plot, move on
      
    } else {
      
      if (input$DOC<0.5 |input$DOC>17 |  input$pH<6.0 |input$pH>8.0 |
                input$Calcium<3.7 | input$Calcium>88 | input$Magnesium<3  | input$Magnesium>72) {
        NiNote <- "TMF(s) outside applicable model range"

      } else {
        NiNote <- "TMFs in applicable range, DGV suitable"
      
      myDOC <- input$DOC
      mypH  <- input$pH
      myCa  <- input$Calcium
      myMg  <- input$Magnesium
      
      tMLR[is.na(tMLR)] <- 0                                        # Zero out coefficients that are NA - will mean that these parts of the general full
      
      # Equation below do not contribute to the formula
      sens <- merge(sens,tMLR,by.x="Model.used",by.y="MLR.model")
      
      # Apply generic equation form
      sens$Conc <- exp(sens$Sensitivity + sens$DOC*log(myDOC) + sens$Ca*log(myCa) + sens$Mg*log(myMg) +
                         sens$pH*mypH + sens$DOC.pH*log(myDOC)*mypH + sens$Mg.pH*log(myMg)*mypH)
      
      # Fit ssd functions and extract protection values
      res <- try(ssd_fit_bcanz(sens), silent = FALSE)
      
      if(isTRUE(class(res)=="try-error")) {                             # if data cannot be fitted, NA is recorded
         ### Don't save a figure?
      
      } else {

        print(paste0("Plotting Ni row ", input$myrow))
        
        Nissd.pred <- predict(res, ci = TRUE)
        
        BAGV_95 <- ssd_hc(res, percent = c(5))
        
        fig_Ni <- ssd_plot(sens, Nissd.pred, ribbon = TRUE,
                           label = "PlotLabel",
                          color = "Model.used") +
          scale_x_continuous(TeX("Dissolved nickel ($\\mu g$/L)"),
                             breaks = c(1, 10, 100, 1000, 10000)) +
        
          ggtitle(paste("Row: ", input$myrow)) +
          labs(subtitle = "Nickel species sensitivity distribution",
               caption = bquote("SSD produced"~ .(format(Sys.Date(), "%d-%m-%Y"))* ", DOC:"~ .(round(myDOC,1))~
                                                  "mg/L, pH:"~ .(round(mypH,1)) * 
                                                  ", Calcium:"~ .(round(myCa,1))~"mg/L, Magnesium:"~.(round(myMg,1))~ "mg/L,
                                                  Nickel BAGV"[95]~.(round(BAGV_95$est,1))~mu*"g/L"),
               colour = "Model used") +
          theme_bw() +
          theme(legend.position.inside = c(0.2, 0.8),
                legend.background = element_rect(color = "black", linewidth = 0.1),
                legend.text=element_text(size=8),
                legend.key.size = unit(0.5, 'cm'),
                plot.caption.position = "plot",
                plot.caption = element_text(hjust = 0)
                )

          p_name <- paste0("Ni_SSD_", input$myrow)
          temp <- c(names(plots), p_name)
          
          plots <<- append(plots, list(fig_Ni))
          names(plots) <<- temp
   
          }
      }
    }
  }
  
  ######################################################################
  # Zinc, with MLR adjustment
  
  DoZnSSDPlots <- function(input, sens=ZnSSD.df, tMLR=ZnMLR.coeffs){
    
    if (is.function(updateProgress)) {
      text <- paste0("Zinc, row ", input$myrow, "/", Nrows)
      updateProgress(value=N, detail=text)
      N <<- N + 1/NrowsTotal
    }
    
    # Check input data. If data is missing do nothing
    
    if (is.na(input$DOC) | is.na(input$pH) | is.na(input$Hardness)) {
      
      ## don't plot, move on
      
    } else {
      
      if (input$DOC<0.5 |input$DOC>15 |  input$pH<6.7 |input$pH>8.1 |
          input$Hardness<26 | input$Hardness>370) {
        ZnNote <- "TMF(s) outside applicable model range"
        
      } else {
        ZnNote <- "TMFs in applicable range, DGV suitable"
        
        myDOC <- max(input$DOC, 0.5)                                      # Adds a floor for zinc
        mypH  <- input$pH
        myH  <- input$Hardness
        
        tMLR[is.na(tMLR)] <- 0                                        # Zero out coefficients that are NA - will mean that these parts of the general full
        
        # Equation below do not contribute to the formula
        sens <- merge(sens,tMLR,by.x="Model.used",by.y="type")
        
        # Apply generic equation form
        sens$Conc <- exp(sens$Sensitivity + sens$DOC*log(myDOC) + sens$H*log(myH) +
                           sens$pH*mypH + sens$DOC.pH*log(myDOC)*mypH)
        
        # Fit ssd functions and extract protection values
        res <- try(ssd_fit_bcanz(sens), silent = FALSE)
        
        if(isTRUE(class(res)=="try-error")) {                             # if data cannot be fitted, NA is recorded
          ### Don't save a figure?
          
        } else {
          
          print(paste0("Plotting Zn row ", input$myrow))
          
          Znssd.pred <- predict(res, ci = TRUE)
          
          BAGV_95 <- ssd_hc(res, percent = c(5))
          
          fig_Zn <- ssd_plot(sens, Znssd.pred, ribbon = TRUE,
                             label = "PlotLabel",
                             color = "Model.used") +
            scale_x_continuous(TeX("Dissolved zinc ($\\mu g$/L)"),
                               breaks = c(1, 10, 100, 1000, 10000)) +
            ggtitle(paste("Row: ", input$myrow)) +
            labs(subtitle = "Zinc species sensitivity distribution",
                 caption = bquote("SSD produced"~ .(format(Sys.Date(), "%d-%m-%Y"))*", DOC:"~ .(round(myDOC,1))~
                                    "mg/L, pH:"~ .(round(mypH,1)) * 
                                    ", Hardness:"~ .(round(mypH,1))*" mg/L CaCO"[3]*
                                    ", Zinc BAGV"[95]~.(round(BAGV_95$est,1))~mu*"g/L"),
                 colour = "Model used") +
            theme_bw() +
            theme(legend.position.inside = c(0.2, 0.8),
                  legend.background = element_rect(color = "black", linewidth = 0.1),
                  legend.text=element_text(size=8),
                  legend.key.size = unit(0.5, 'cm'),
                  plot.caption.position = "plot",
                  plot.caption = element_text(hjust = 0)
            )
          
          p_name <- paste0("Zn_SSD_", input$myrow)
          temp <- c(names(plots), p_name)
          
          plots <<- append(plots, list(fig_Zn))
          names(plots) <<- temp
          
        }
      }
    }
  }
  
  
  
 ### Function to get all GVs------------------------ 

  GetAllSSDPlots <- function(myTMF.df) {
    myTMF.df <- myTMF.df |> dplyr::mutate(myrow = row_number())  #Used in the plotting title
    
    # Specify the columns to convert (if present)
    columns_to_convert <- c("DOC","pH", "Hardness", "Calcium", "Magnesium", "Copper", "Nickel", "Zinc")  # columns we use
    
    # Convert columns to numeric only if they are present in the df (should avoid crashing)
    myTMF.df[intersect(columns_to_convert, names(myTMF.df))] <- lapply(myTMF.df[intersect(columns_to_convert, 
                                                                                          names(myTMF.df))], as.numeric)
    # Convert data to NA if they were zero (only if cols are present in the df to avoid crashing)
    myTMF.df[intersect(columns_to_convert, names(myTMF.df))] <- lapply(myTMF.df[intersect(columns_to_convert, 
                                                                                          names(myTMF.df))], 
                                                                       function(x) ifelse(x == 0, NA, x))
    
   
    if ("Cu" %in% metals) {
      
      ## do nothing?
      ##Cu.output <- ddply(myTMF.df,.(myrow), function(x) GetCuGuidelines(input=x, Cucol=Cucol))
    }
    
    if ("Ni" %in% metals) {
      
      Ni.output <- myTMF.df |>
        dplyr::group_split(myrow) |>
        purrr::map(DoNiSSDPlots)
     # message <- "Complete"
    }
    
    if ("Zn" %in% metals) {
      
      Zn.output <- myTMF.df |>
        dplyr::group_split(myrow) |>
        purrr::map(DoZnSSDPlots) 
    }
    
    return(message)
    
  }
  
  AllPlots <- GetAllSSDPlots(myTMF.df=df)
  
  if (is.function(updateProgress)) {
    text <- "Complete!"
    updateProgress(value=N, detail=text)
    N <<- 1
  }
  
  return (list("plots"=plots))
  
}  
