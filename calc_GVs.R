#######################################################################
#######################################################################
#
#              ********  Cu, Zn & Ni DGV adjuster  *********    
#
#######################################################################
#######################################################################
# Created: May 2024
# Authors: Jenni Gadd, Caroline Fraser (LWP), Sharleen Yalden

# Edited to work with R Shiny tool Aug 2024

calc_GVs <- function(df, options){
  
  #####################################################################
  ## Libraries and data files needed
  
 # library(plyr)                        ## Required
  library(tidyverse)                   ## Required
  library(ssdtools)                    ## Required
  
  # Read coefficients and species data ################################
  
  metals = options$metals
  calc_biof = options$calc_biof
  pcs = options$pcs
  rcr = options$rcr
  country = options$country
  
  # include PC95 if not selected in options so that bioavailable fraction can
  # be calculated
  
  pcs_calc = pcs
  if (calc_biof) pcs_calc = unique(c(pcs,"PC95"))
  
  pcs_all = c("PC99", "PC95", "PC90", "PC80")
  pcs_vals_all = c(1, 5, 10, 20)
  
  pcs_vals = pcs_vals_all[pcs_all %in% pcs_calc]
  
  ### For each metal, there are 4 values for each country. 
  ### Depending on the PCs selected, we need to provide these values with the returned table
  ### Something like below?? 
  
  CuDGVvals_all <- data.frame("nz" = c("PC99" = 0.4, "PC95" = 0.753, "PC90" = 0.9, "PC80" = 1.3),
                              "aus" = c("PC99" = 0.4, "PC95" = 0.753, "PC90" = 0.9, "PC80" = 1.3))
  
  NiDGVvals_all <- data.frame("aus" = c("PC99" = 0.6, "PC95" = 3.4, "PC90" = 6.9, "PC80" = 14),
                              "nz" = c("PC99" = 0.4, "PC95" = 2.3, "PC90" = 4.8, "PC80" = 10))
  
  CuDGV_vals <- setNames(CuDGVvals_all[,country], row.names(CuDGVvals_all))
  NiDGV_vals <- setNames(NiDGVvals_all[,country], row.names(NiDGVvals_all))
  
  
  # cuDGV_vals = CuDGVvals_all[pcs_all %in% pcs_calc]    
  # DGV_cu_nz <- 0.6
  # DGV_cu_aus <- 0.8
  DGV_ni_nz <- 2.3
  DGV_ni_aus <- 3.1
  
  # DGV_cu <- DGV_cu_aus
  DGV_ni <- DGV_ni_aus
  # 
  if (country == "nz") {
  #   DGV_cu <- DGV_cu_nz
   DGV_ni <- DGV_ni_nz
  }
  
  # Zinc
  
  # if ("Zn" %in% metals){
  #   load(file="data/ZnMLRcoeffs.Rdata")
  #   load(file="data/ZnSSD.Rdata")
  # }
  
  # Nickel
  
  if ("Ni" %in% metals){
    load(file="data/NiSSD.Rdata")
    load(file="data/NiMLR.coeffs.Rdata")
  }
  
  # Initialize output info
  
  summary = data.frame(matrix(nrow=0, ncol=3))                                   # dataframe summarising number of GVs calculated for each metal and the number of rows excluded
  names(summary) = c("metal","nGVs","nExcluded")
  print(summary)
  plots = list()                                                                 # list to hold SSD plots
  
  ######################################################################
  ## Functions to calculate GVs
  
  # Copper, just an equation
  
  GetCuGuidelines <- function(input){
    
    # Check input data. If data are missing, do nothing, but keep the row.
    # Otherwise, write a note if any of the observations are out of the fitting
    # bounds
    
    GV <- data.frame(matrix(nrow=1, ncol=length(pcs_calc)))
    GV_labels = paste("Cu", pcs_calc, sep="")
    names(GV) = GV_labels
    
    # GV <- data.frame(matrix(nrow=1, ncol=length(pcs)+1))
    # GV_labels = paste("Cu", pcs, sep="")
    # names(GV) = c(GV_labels, "CuNote")
    
    # Initialize flag for whether or not to calculate GVs
    
    do_calcs <- FALSE
    
    # Check whether or not to calculate GVs based on availability and range
    # of input variables
    
    if(is.na(input$DOC)) {
      
      CuNote <- "DOC missing"
      GV[1,GV_labels] = NA
      
    } else if ("pH" %in% names(input) & "Hardness" %in% names(input)) {
      
      # If both pH and hardness columns are in the input dataset
      
      if (is.na(input$pH) | is.na(input$Hardness)) {
        CuNote <- "pH and/or hardness not provided, DGV may or may not be applicable"
        do_calcs <- TRUE
        
      } else if (input$DOC>30 | input$pH<6 | input$pH>8.5 | input$Hardness<2 | input$Hardness>340) {
        
        CuNote <- case_when(input$DOC>30 ~ "DOC above upper applicability limit",
                            input$pH<6 ~ "pH below lower applicability limit",
                            input$pH>8.5 ~ "pH above upper applicability limit",
                            input$Hardness<2 ~ "Hardness below lower applicability limit",
                            input$Hardness >340 ~ "Hardness above upper applicability limit")
        
        # CuNote <- case_when(DOC > 30 ~ "DOC above upper applicability limit",
        #                     pH <6 ~ "pH below lower applicability limit", 
        #                     pH >8.5 ~ "pH above upper applicability limit",
        #                     Hardness <2 ~ "Hardness below lower applicability limit", 
        #                     Hardness >340 ~ "Hardness above upper applicability limit")  
        ## this wont work as intended if two things out of range. 
        ## need to make a new note everytime and then use paste to join?  
        
        GV[1,GV_labels] = NA
        #GV[1,"CuNote"] = CuNote
        
      } else {
        CuNote <- "TMF data in range, GV suitable"
        do_calcs <- TRUE
      }
        
    } else {
      
      # Either pH or hardness columns are missing from the input dataset, can
      # still do calcs but note that GV values may or may not be applicable
      
      CuNote <- "pH and/or hardness not provided, DGV may or may not be applicable"
      do_calcs <- TRUE
      
    }
    
    if (do_calcs) {
      
      for (p in pcs_calc) {
        
        DGV_cu <- CuDGV_vals[p]
        GV[1,paste0("Cu",p)] <- ifelse(DGV_cu*(input$DOC/0.5)^1.00 <1,
                                       round(max(DGV_cu, DGV_cu*(input$DOC/0.5)^1.00),1),
                                       signif(max(DGV_cu, DGV_cu*(input$DOC/0.5)^1.00),2))
      }
      
      # if ("PC99" %in% pcs_calc) GV$CuPC99 <- ifelse(0.20*(input$DOC/0.5)^1.00 <1,
      #                                               round(max(0.2, 0.20*(input$DOC/0.5)^1.00),1),
      #                                               signif(max(0.2, 0.20*(input$DOC/0.5)^1.00),2))
      # 
      # if ("PC95" %in% pcs_calc) GV$CuPC95 <- ifelse(DGV_cu*(input$DOC/0.5)^1.00 <1,
      #                                               round(max(DGV_cu, DGV_cu*(input$DOC/0.5)^1.00),1),
      #                                               signif(max(DGV_cu, DGV_cu*(input$DOC/0.5)^1.00),2))
      # 
      # if ("PC90" %in% pcs_calc) GV$CuPC90 <- signif(max(0.73, 0.73*(input$DOC/0.5)^1.00),2)
      # if ("PC80" %in% pcs_calc) GV$CuPC80 <- signif(max(1.3, 1.3*(input$DOC/0.5)^1.00),2)
      
    }
    
    
    
    
    
    # if (input$DOC>30 | input$pH<6 |input$pH > 8.5 | input$Hardness<2 | input$Hardness>340){
    #    CuNote <- case_when(DOC > 30 ~ "DOC above upper applicability limit",
    #                           pH <6 ~ "pH below lower applicability limit", 
    #                           pH >8.5 ~ "pH above upper applicability limit",
    #                           Hardness <2 ~ "Hardness below lower applicability limit", 
    #                           Hardness >340 ~ "Hardness above upper applicability limit")  
    #    ## this wont work as intended if two things out of range. 
    #    ## need to make a new note everytime and then use paste to join?  
    #       
    #   GV[1,GV_labels] = NA
    #   #GV[1,"CuNote"] = CuNote
    #   
    # } else {
    #   if("pH" %in% names(input) & "Hardness" %in% names(input)) {
    #     if(is.na(input$pH) | is.na(input$Hardness)){
    #       CuNote <- "pH and/or hardness not provided, DGV may or may not be applicable"
    # 
    #    } else {
    #       CuNote <- "TMF data in range, GV suitable"   
    #     }
    # 
    #   # Apply equation form -  
    #   ## SHARLEEN -------------------****************
    #     ## USE THE cu-DGVs here so its easier to update in one place only 
    #     ## ***********************************
    #   if ("PC99" %in% pcs_calc) GV$CuPC99 <- ifelse(0.20*(input$DOC/0.5)^1.00 <1,
    #                                                 round(max(0.2, 0.20*(input$DOC/0.5)^1.00),1),
    #                                                 signif(max(0.2, 0.20*(input$DOC/0.5)^1.00),2))
    #   
    #   if ("PC95" %in% pcs_calc) GV$CuPC95 <- ifelse(DGV_cu*(input$DOC/0.5)^1.00 <1,
    #                                                 round(max(DGV_cu, DGV_cu*(input$DOC/0.5)^1.00),1),
    #                                                 signif(max(DGV_cu, DGV_cu*(input$DOC/0.5)^1.00),2))
    # 
    #   if ("PC90" %in% pcs_calc) GV$CuPC90 <- signif(max(0.73, 0.73*(input$DOC/0.5)^1.00),2)
    #   if ("PC80" %in% pcs_calc) GV$CuPC80 <- signif(max(1.3, 1.3*(input$DOC/0.5)^1.00),2)
      
    # }
    myoutput <- cbind(input, CuNote, GV)
    
    if (calc_biof) {
      
      DGV_cu <- CuDGV_vals["PC95"]
      myoutput <- myoutput |>
        dplyr::mutate(CuBioF = case_when(is.na(CuPC95) ~ NA,
                                         DGV_cu/CuPC95 > 1 ~ 1,
                                         TRUE ~ DGV_cu/CuPC95),                         # Bioavailable fraction
                      CuBio = case_when(is.na(CuPC95) ~ NA,
                                        is.na(CuBioF) ~ NA,
                                        !is.numeric(Copper) ~ NA,
                                        is.numeric(Copper) ~ signif(Copper*CuBioF,2)),     # Bioavailable Cu
                      #CuDGV = DGV_cu
                      )
    }
    
    myoutput[,paste0("DGV_Cu", names(CuDGV_vals))] = CuDGV_vals
    
    # Hazard quotient
    
    if (rcr) {
      for (p in pcs) {
        x = gsub("PC","",p)
        col = paste0("Cu",p)
        myoutput <- myoutput |>
          dplyr::mutate("CuHQ{x}" := ifelse((is.na(.data[[col]])|is.na(Copper)), NA, signif(Copper/.data[[col]],2)))
      }
    }
    
    return(myoutput)
    
    }
 
  
  # Zinc, with MLR adjustment
  
  GetZnGuidelines <- function(input, sens=ZnSSD.df, tMLR=ZnMLR.coeffs){
    
    # Check input data. If data is missing do nothing, but keep the row.
    # Otherwise, write a note if any of the observations are out of the fitting
    # bounds
    
    GV <- data.frame(matrix(nrow=1, ncol=length(pcs_calc)+1))
    GV_labels = paste("Zn", pcs_calc, sep="")
    names(GV) = c(GV_labels, "ZnNote")
    
    if(is.na(input$DOC) | is.na(input$pH) | is.na(input$Hardness)) {
      
      ZnNote <- "TMFs missing"
      
      GV[1,GV_labels] = NA
      GV[1,"ZnNote"] = ZnNote
      
    } else {
      
      if(input$DOC>40 | input$pH>8.5 | input$Hardness>529){
        ZnNote <- "TMF(s) outside applicable model range"
        
      } else if (input$DOC>15 | input$pH >8.1 | input$Hardness> 370){
        ZnNote <- "TMF(s) outside applicable model range"
        
      } else if (input$DOC<0.3 | input$pH<5.6 | input$Hardness<5) {
        ZnNote <- "TMF(s) outside applicable model range"
        
      } else if (input$DOC<0.5 | input$pH<6.7| input$Hardness<26) {
        ZnNote <- "TMF(s) outside applicable model range"
        
      } else {
        ZnNote <- "TMFs in applicable range, DGV suitable"
      }
      
      # myDOC <- min(max(input$DOC, 0.5), 15)                 # Use this if we want to crop the TMF data to the MLR range
      # myH   <- min(max(input$H, 20), 440)
      # mypH  <- min(max(input$pH, 6.2), 8.3)
      
      myDOC <- input$DOC                                      # Use this if we want to calculate anyway & decide on issues later
      myH   <- input$Hardness
      mypH  <- input$pH
      
      tMLR[is.na(tMLR)] <- 0                                  # Zero out coefficients that are NA - will mean that these parts of the general full
                                                              # equation below do not contribute to the formula
      sens <- merge(sens,tMLR,by.x="Model used",by.y="type")
      
      # Apply generic equation form
      sens$Conc <- exp(sens$Sensitivity + sens$DOC*log(myDOC) + sens$H*log(myH) +
                         sens$pH*mypH + sens$DOC.pH*log(myDOC)*mypH)
      
      # Note: column must be called Conc for ssdtools
      
      # Fit ssd function and extract protection values
      res <- try(ssd_fit_bcanz(sens), silent=FALSE)
      
      if (isTRUE(class(res)=="try-error")) {                              # if data cannot be fitted, NA is recorded
        GV[GV_labels] = NA
        
      } else {
        GV_temp = as.data.frame(t(ssd_hc(res, percent=pcs_vals, ci=FALSE, nboot=10)[,3]))
        GV_temp <- GV_temp |>
          dplyr::mutate(across(is.numeric, ~case_when(.x <1 ~ round(.x, digits=1),
                                               TRUE ~ signif(.x, 2))))
        
        rownames(GV_temp) <- NULL
        #GV = as.data.frame(t(ssd_hc(res, percent=c(1, 5, 10, 20), ci=FALSE, nboot=10)[,3])) 
        #rownames(GV) <- NULL
      }
      
      GV <- cbind(GV_temp, ZnNote)
    }
    
    names(GV) <- c(GV_labels, "ZnNote")
    myoutput <- cbind(input, GV)
    
    if (calc_biof) {
    #if (calc_biof & ("PC95" %in% pcs)) {
      
      myoutput <- myoutput |>
        dplyr::mutate(ZnBioF = case_when(is.na(ZnPC95) ~ NA,                       # Calc Bioavailable fraction
                                         4.1/ZnPC95 > 1 ~ 1,                       # Make 1 if > 1
                                         TRUE ~ 4.1/ZnPC95),
                      ZnBio = case_when(is.na(ZnPC95) ~ NA,
                                        is.na(ZnBioF) ~ NA,
                                        !is.numeric(Zinc) ~ NA,
                                        is.numeric(Zinc) ~ signif(Zinc*ZnBioF,2)   # Bioavailable Zn
                                        )
                      )
    }
    
    # Hazard quotient
    
    if (rcr) {
      for (p in pcs) {
        x = gsub("PC","",p)
        col = paste0("Zn",p)
        myoutput <- myoutput |>
          dplyr::mutate("ZnHQ{x}" := ifelse((is.na(.data[[col]])|is.na(Zinc)), NA, signif(Zinc/.data[[col]],2)))
      }
    }
    
    # if (rcr & ("PC95" %in% pcs)) {
    #   myoutput <- myoutput |>
    #     dplyr::mutate(Zn_HQ = ifelse((is.na(ZnPC95)|is.na(Zinc)), NA, signif(Zinc/ZnPC95,2))     # RCR Zn
    #     )
    # }
    
    return (myoutput)
   
  }
  
  # Nickel, with MLR adjustment
  
  GetNiGuidelines <- function(input, sens=NiSSD.df, tMLR=NiMLR.coeffs){
    
    # Check input data. If data is missing do nothing, but keep the row.
    # Otherwise, write a note if any of the observations are out of the fitting
    # bounds
    
    GV <- data.frame(matrix(nrow=1, ncol=length(pcs_calc)+1))
    GV_labels = paste("Ni", pcs_calc, sep="")
    names(GV) = c(GV_labels, "NiNote")
    
    if (is.na(input$DOC) | is.na(input$pH) | is.na(input$Ca) |is.na(input$Mg)) {
      
      NiNote <- "TMFs missing"
      
      GV[1,GV_labels] = NA
      GV[1,"NiNote"] = NiNote
      
    } else {
      
      if (input$DOC<0.3 | input$pH<5.5| input$Ca<0.1 | input$Mg<0.2) {
        NiNote <- "TMF(s) outside applicable model range"
        
      } else if (input$DOC<0.7 | input$pH<6.9 | input$Ca<3.7 | input$Mg<3) {
        NiNote <- "TMF(s) outside applicable model range"
        
      } else if (input$DOC>26 | input$pH>8.5 | input$Ca>144 | input$Mg>286) {
        NiNote <- "TMF(s) outside applicable model range"
        
      } else if (input$DOC>7.1 | input$pH>8.0 | input$Ca>72 | input$Mg>21) {
        NiNote <- "TMF(s) outside applicable model range"
        
      } else {
        NiNote <- "TMFs in applicable range, DGV suitable"
      }
      
      myDOC <- input$DOC
     # myH   <- input$Hardness
      mypH  <- input$pH
      myCa  <- input$Ca
      myMg  <- input$Mg
      
      tMLR[is.na(tMLR)] <- 0                                        # Zero out coefficients that are NA - will mean that these parts of the general full
      
      # Equation below do not contribute to the formula
      sens <- merge(sens,tMLR,by.x="Model.used",by.y="MLR.model")
      
      # Apply generic equation form
      sens$Conc <- exp(sens$Sensitivity + sens$DOC*log(myDOC) + sens$Ca*log(myCa) + sens$Mg*log(myMg) +
                         sens$pH*mypH + sens$DOC.pH*log(myDOC)*mypH + sens$Mg.pH*log(myMg)*mypH)
      
      # Fit ssd functions and extract protection values
      res <- try(ssd_fit_bcanz(sens), silent = FALSE)
      
      if(isTRUE(class(res)=="try-error")) {                             # if data cannot be fitted, NA is recorded
        GV[GV_labels] = NA
        #GV <- data.frame(NiPC99=NA, NiPC95=NA, NiPC90=NA, NiPC80=NA) 
      
      } else {
        
        GV_temp = as.data.frame(t(ssd_hc(res, percent=pcs_vals, ci=FALSE, nboot=100)[,3])) 
        GV_temp <- GV_temp |>
          dplyr::mutate(across(is.numeric, ~case_when(.x <1 ~ round(.x, digits=1),
                                               TRUE ~ signif(.x, 2))))
          rownames(GV_temp) <- NULL
          
         # ## Real ssd plot
         #  Nissd.pred <- predict(res, ci = TRUE)
         # 
         #  fig_Ni <- ssd_plot(sens, Nissd.pred, ribbon = TRUE,
         #                     label = "PlotLabel",
         #                     color = "Model.used") +
         #    ggtitle(paste("Row: ", input$row)) +
         #    labs(subtitle = "Nickel species sensitivity distribution",
         #         caption = paste("SSD for DOC=", round(myDOC,1), " pH=", round(mypH,1),
         #                         " Ca=", round(myCa,1), " Mg=", round(myMg,1))) +
         #    theme_bw() +
         #    theme(legend.position.inside = c(0.2, 0.8),
         #          legend.background = element_rect(color = "black", linewidth = 0.1),
         #          legend.text=element_text(size=8),
         #          legend.key.size = unit(0.5, 'cm'),
         #          plot.caption.position = "plot",
         #          plot.caption = element_text(hjust = 0)
         #    )
         # 
         #  p_name <- paste0("Ni_SSD_", input$row)
         #  temp <- c(names(plots), p_name)
         # 
         #  plots <<- append(plots, list(fig_Ni))
         #  names(plots) <<- temp

          # # Make dummy plot and update list of plots to return
          # fig_Ni <- ggplot(input, aes(DOC, Hardness)) + geom_point()
          #
          # p_name <- paste0("Ni_SSD_", input$row)
          # temp <- c(names(plots), p_name)
          #
          # plots <<- append(plots, list(fig_Ni))
          # names(plots) <<- temp
          #
      }
      
      GV <- cbind(GV_temp, NiNote) 
      
    }
    
    names(GV) <- c(GV_labels, "NiNote")
    myoutput <- cbind(input, GV)
    
    if (calc_biof) {
      
      myoutput <- myoutput |>
        dplyr::mutate(NiBioF = case_when(is.na(NiPC95) ~ NA,                             # Calc bioavailable fraction
                                         DGV_ni/NiPC95 > 1 ~ 1,                        # Make 1 if > 1
                                         TRUE ~ DGV_ni/NiPC95),
                      NiBio = case_when(is.na(NiPC95) ~ NA,
                                        is.na(NiBioF) ~ NA,
                                        !is.numeric(Nickel) ~ NA,
                                        is.numeric(Nickel) ~ signif(Nickel*NiBioF,2)),   # Bioavailable Ni
                      NiDGV = DGV_ni
                      )
    }
    
    # Hazard quotient
    
    if (rcr) {
      for (p in pcs) {
        x = gsub("PC","",p)
        col = paste0("Ni",p)
        myoutput <- myoutput |>
          dplyr::mutate("NiHQ{x}" := ifelse((is.na(.data[[col]])|is.na(Nickel)), NA, signif(Nickel/.data[[col]],2)))
      }
    }
    
    # if (rcr & ("PC95" %in% pcs)) {
    #   myoutput <- myoutput |>
    #     dplyr::mutate(Ni_HQ = ifelse((is.na(NiPC95)|is.na(Nickel)), NA, signif(Nickel/NiPC95,2))     # RCR Ni
    #     )
    # }
    
    return (myoutput)
    
  }
  
  GetAllGVs <- function(myTMF.df) {
    
    myTMF.df <- myTMF.df |> dplyr::mutate(row = row_number(), .before = everything()) # Used only for users to reorder the table display
    
    # Specify the columns to convert (if present)
    columns_to_convert <- c("DOC","pH", "Hardness", "Ca", "Mg", "Copper", "Nickel", "Zinc")  # columns we use
    
    # Convert columns to numeric only if they are present in the df (should avoid crashing)
     myTMF.df[intersect(columns_to_convert, names(myTMF.df))] <- lapply(myTMF.df[intersect(columns_to_convert, 
                                                                                           names(myTMF.df))], as.numeric)
     # Convert data to NA if they were zero (only if cols are present in the df to avoid crashing)
     myTMF.df[intersect(columns_to_convert, names(myTMF.df))] <- lapply(myTMF.df[intersect(columns_to_convert, 
                                                                                           names(myTMF.df))], 
                                                                        function(x) ifelse(x == 0, NA, x))
     ## Add a bit in here to calculate Hardness 
    Alloutput = myTMF.df
    
    if ("Cu" %in% metals) {
      Cu.output <- myTMF.df |>
        dplyr::group_split(row) |>
        purrr::map(GetCuGuidelines) |>
        purrr::list_rbind()
      
    #  Cu.output <- ddply(myTMF.df,.(row), function(x) GetCuGuidelines(input=x))
      GV_labels = paste("Cu", pcs, sep="")
      #if (calc_biof) GV_labels = c(GV_labels, "CuBio", "CuDGV")
      if (calc_biof) GV_labels = c(GV_labels, "CuBio")
      GV_labels = c(GV_labels, paste0("DGV_Cu",names(CuDGV_vals)))
      if (rcr) GV_labels = c(GV_labels, paste0("Cu",gsub("PC","HQ",pcs)))
      Alloutput <- cbind(Alloutput, Cu.output %>% dplyr::select(all_of(c(GV_labels, "CuNote"))))
      
      i = nrow(summary)
      summary[i+1,"metal"] <<- "Copper"
      summary[i+1,"nExcluded"] <<- nrow(Cu.output[which(Cu.output$CuNote=="DOC missing"),])
      summary[i+1,"nGVs"] <<- nrow(Cu.output) - summary[i+1,"nExcluded"]
    }
    
    # if ("Zn" %in% metals) {
    #   
    #   Zn.output <- myTMF.df |>
    #     dplyr::group_split(row) |>
    #     purrr::map(GetZnGuidelines) |>
    #     purrr::list_rbind()
    #   #     Zn.output <- ddply(myTMF.df,.(row), function(x) GetZnGuidelines(input=x))
    #   GV_labels = paste("Zn", pcs, sep="")
    #   if (calc_biof) GV_labels = c(GV_labels, "ZnBio")
    #   if (rcr) GV_labels = c(GV_labels, paste0("Zn",gsub("PC","HQ",pcs)))
    #   Alloutput <- cbind(Alloutput, Zn.output %>% dplyr::select(all_of(c(GV_labels, "ZnNote"))))
    #   
    #   i = nrow(summary)
    #   summary[i+1,"metal"] <<- "Zinc"
    #   summary[i+1,"nExcluded"] <<- nrow(Zn.output[which(Zn.output$ZnNote=="TMFs missing"),])
    #   summary[i+1,"nGVs"] <<- nrow(Zn.output) - summary[i+1,"nExcluded"]
    # }
    
    if ("Ni" %in% metals) {
      
      Ni.output <- myTMF.df |>
        dplyr::group_split(row) |>
        purrr::map(GetNiGuidelines) |>
        purrr::list_rbind()
    #  Ni.output <- ddply(myTMF.df,.(row), function(x) GetNiGuidelines(input=x))
      GV_labels = paste("Ni", pcs, sep="")
      if (calc_biof) GV_labels = c(GV_labels, "NiBio", "NiDGV")
      if (rcr) GV_labels = c(GV_labels, paste0("Ni",gsub("PC","HQ",pcs)))
      Alloutput <- cbind(Alloutput, Ni.output %>% dplyr::select(all_of(c(GV_labels, "NiNote"))))
      
      i = nrow(summary)
      summary[i+1,"metal"] <<- "Nickel"
      summary[i+1,"nExcluded"] <<- nrow(Ni.output[which(Ni.output$NiNote=="TMFs missing"),])
      summary[i+1,"nGVs"] <<- nrow(Ni.output) - summary[i+1,"nExcluded"]
      #Alloutput <- Alloutput |> select(-row)
    }
    
    return(Alloutput)
    
  }
  
  AllMetals <- GetAllGVs(myTMF.df=df)
  
  print(AllMetals)
  print(summary)
  
  return (list("results"=AllMetals, "summary"=summary, "plots"=plots))

}  
 
  
