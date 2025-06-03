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

  CuDGVvals_all <- data.frame("nz" = c("PC99" = 0.439, "PC95" = 0.732, "PC90" = 0.896, "PC80" = 1.28),
                              "aus" = c("PC99" = 0.439, "PC95" = 0.732, "PC90" = 0.896, "PC80" = 1.28))
  
  NiDGVvals_all <- data.frame("aus" = c("PC99" = 0.66, "PC95" = 3.4, "PC90" = 6.9, "PC80" = 14),
                              "nz" = c("PC99" = 0.39, "PC95" = 2.3, "PC90" = 5.0, "PC80" = 11))
  
  
  #-----------------------------------------
  # THESE WILL NEED TO BE UPDATED, USING NI VALUES FOR NOW...
  
  ZnDGVvals_all <- data.frame("aus" = c("PC99" = 0.66, "PC95" = 3.4, "PC90" = 6.9, "PC80" = 14),
                              "nz" = c("PC99" = 0.39, "PC95" = 2.3, "PC90" = 5.0, "PC80" = 11))
  #------------------------------------------
  
  
  CuDGV_vals <- setNames(CuDGVvals_all[,country], row.names(CuDGVvals_all))
  NiDGV_vals <- setNames(NiDGVvals_all[,country], row.names(NiDGVvals_all))
  ZnDGV_vals <- setNames(ZnDGVvals_all[,country], row.names(ZnDGVvals_all))
  
  
  # Load Nickel SSD and MLR data if applicable
  
  if ("Ni" %in% metals){
    load(file="data/NiSSD.Rdata")
    load(file="data/NiMLR.coeffs.Rdata")
  }
  
  # Load Zinc SSD and MLR data if applicable
  
  if ("Zn" %in% metals){
    load(file="data/ZnMLRcoeffs.Rdata")
    load(file="data/ZnSSD.Rdata")
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
        CuNote <- "pH and/or hardness not provided, PC may or may not be applicable"
        do_calcs <- TRUE
        
      } else if (input$DOC>30 | input$pH<6 | input$pH>8.5 | input$Hardness<2 | input$Hardness>340) {
        
        DOCnote <-case_when(input$DOC>30 ~ "DOC above upper applicability limit",
                            TRUE ~ NA)
        pHnote <-case_when(input$pH<6 ~ "pH below lower applicability limit",
                            input$pH>8.5 ~ "pH above upper applicability limit",
                           TRUE ~ NA)
        Hnote <-case_when(input$Hardness<2 ~ "Hardness below lower applicability limit",
                            input$Hardness >340 ~ "Hardness above upper applicability limit",
                          TRUE ~ NA)
        
        CuNote <- paste(na.omit(c(DOCnote,pHnote, Hnote)), collapse = ", ")
        
        GV[1,GV_labels] = NA
        #GV[1,"CuNote"] = CuNote
        
      } else {
        CuNote <- "TMF data in range, GV suitable"
        do_calcs <- TRUE
      }
        
    } else {
      
      # Either pH or hardness columns are missing from the input dataset, can
      # still do calcs but note that GV values may or may not be applicable
      
      CuNote <- "pH and/or hardness not provided, PC may or may not be applicable"
      do_calcs <- TRUE
      
    }
    
    if (do_calcs) {
      
      for (p in pcs_calc) {
        
        DGV_cu <- CuDGV_vals[p]
        GV[1,paste0("Cu",p)] <- ifelse(DGV_cu*(input$DOC/0.5)^1.00 <1,
                                       round(max(DGV_cu, DGV_cu*(input$DOC/0.5)^1.00),1),
                                       signif(max(DGV_cu, DGV_cu*(input$DOC/0.5)^1.00),2))
      }
     
    }
    
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
      CuDGV_vals_sub = CuDGV_vals[pcs_all %in% pcs]
      myoutput[,paste0("DGV_Cu", names(CuDGV_vals_sub))] = signif(CuDGV_vals_sub,2)
      
    }
 
    
    # Hazard quotient
    
    if (rcr) {
      for (p in pcs) {
        x = gsub("PC","",p)
        col = paste0("Cu",p)
        myoutput <- myoutput |>
          dplyr::mutate("Cu_HQ{x}" := ifelse((is.na(.data[[col]])|is.na(Copper)), NA, signif(Copper/.data[[col]],2)))
      }
    }
    
    return(myoutput)
    
    }
 
  # Nickel, with MLR adjustment
  
  GetNiGuidelines <- function(input, sens=NiSSD.df, tMLR=NiMLR.coeffs){
    
    # Check input data. If data is missing do nothing, but keep the row.
    # Otherwise, write a note if any of the observations are out of the fitting
    # bounds
    
    GV <- data.frame(matrix(nrow=1, ncol=length(pcs_calc)+1))
    GV_labels = paste("Ni", pcs_calc, sep="")
    names(GV) = c(GV_labels, "NiNote")
    
    if (is.na(input$DOC) | is.na(input$pH) | is.na(input$Calcium) |is.na(input$Magnesium)) {
      
      NiNote <- "TMFs missing"
      
      GV[1,GV_labels] = NA
      GV[1,"NiNote"] = NiNote
      
      
    } else if (input$DOC<0.5 | input$DOC>17 | input$pH<6 | input$pH>8 |
               input$Calcium<3.7 | input$Calcium>88 | input$Magnesium<3 | input$Magnesium>72) {
      
      DOCnote <-case_when(input$DOC<0.5 ~ "DOC below lower applicability limit",
                          input$DOC>17 ~ "DOC above upper applicability limit",
                          TRUE ~ NA)
      pHnote <-case_when(input$pH<6 ~ "pH below lower applicability limit",
                         input$pH>8 ~ "pH above upper applicability limit",
                         TRUE ~ NA)
      Canote <-case_when(input$Calcium<3.7 ~ "Calcium below lower applicability limit",
                         input$Calcium >88 ~ "Calcium above upper applicability limit",
                         TRUE ~ NA)
      Mgnote <-case_when(input$Magnesium<3 ~ "Magnesium below lower applicability limit",
                         input$Magnesium >72 ~ "Magnesium above upper applicability limit",
                         TRUE ~ NA)
      
      NiNote <- paste(na.omit(c(DOCnote,pHnote, Canote, Mgnote)), collapse = ", ")
      
      GV[1,GV_labels] = NA
      GV[1,"NiNote"] = NiNote
      
    } else {
      
      NiNote <- "TMF data in range, GV suitable"
      
      myDOC <- input$DOC
      mypH  <- input$pH
      myCa  <- input$Calcium
      myMg  <- input$Magnesium
      
      # Zero out coefficients that are NA - will mean that these parts of the general full
      # equation below do not contribute to the formula
      
      tMLR[is.na(tMLR)] <- 0                                        
      
      sens <- merge(sens,tMLR,by.x="Model.used",by.y="MLR.model")
      
      # Apply generic equation form
      sens$Conc <- exp(sens$Sensitivity + sens$DOC*log(myDOC) + sens$Ca*log(myCa) + sens$Mg*log(myMg) +
                         sens$pH*mypH + sens$DOC.pH*log(myDOC)*mypH + sens$Mg.pH*log(myMg)*mypH)
      
      # Fit ssd functions and extract protection values
      res <- try(ssd_fit_bcanz(sens), silent = FALSE)
      
      if(isTRUE(class(res)=="try-error")) {                             # if data cannot be fitted, NA is recorded
        GV[GV_labels] = NA
        
      } else {
        
        GV_temp = as.data.frame(t(ssd_hc(res, percent=pcs_vals, ci=FALSE, nboot=100)[,3])) 
        GV_temp <- GV_temp |>
          dplyr::mutate(across(is.numeric, ~case_when(.x <1 ~ round(.x, digits=1),
                                                      TRUE ~ signif(.x, 2))))
        rownames(GV_temp) <- NULL
      }
      
      GV <- cbind(GV_temp, NiNote)
    }
    
    names(GV) <- c(GV_labels, "NiNote")
    myoutput <- cbind(input, GV)
    
    
    if (calc_biof) {
      DGV_ni <- NiDGV_vals["PC95"]
      myoutput <- myoutput |>
        dplyr::mutate(NiBioF = case_when(is.na(NiPC95) ~ NA,                             # Calc bioavailable fraction
                                         DGV_ni/NiPC95 > 1 ~ 1,                        # Make 1 if > 1
                                         TRUE ~ DGV_ni/NiPC95),
                      NiBio = case_when(is.na(NiPC95) ~ NA,
                                        is.na(NiBioF) ~ NA,
                                        !is.numeric(Nickel) ~ NA,
                                        is.numeric(Nickel) ~ signif(Nickel*NiBioF,2))   # Bioavailable Ni
                      
        )
    }
    
    # Hazard quotient
    
    if (rcr) {
      for (p in pcs) {
        x = gsub("PC","",p)
        col = paste0("Ni",p)
        myoutput <- myoutput |>
          dplyr::mutate("Ni_HQ{x}" := ifelse((is.na(.data[[col]])|is.na(Nickel)), NA, signif(Nickel/.data[[col]],2)))
      }
    }
    
    return (myoutput)
    
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
      
    } else if (input$DOC<0.5 | input$DOC>15 | input$pH<6.7 | input$pH>8.1 |
               input$Hardness<26 | input$Hardness>370) {
      
      DOCnote <- case_when(input$DOC<0.5 ~ "DOC below lower applicability limit",
                           input$DOC>15 ~ "DOC above upper applicability limit",
                           TRUE ~ NA)
      pHnote <- case_when(input$pH<6.7 ~ "pH below lower applicability limit",
                          input$pH>8.1 ~ "pH above upper applicability limit",
                          TRUE ~ NA)
      Hnote <- case_when(input$Hardness<26 ~ "Hardness below lower applicability limit",
                         input$Hardness>370 ~ "Hardness above upper applicability limit",
                         TRUE ~ NA)
      
      ZnNote <- paste(na.omit(c(DOCnote,pHnote, Hnote)), collapse = ", ")
      
      GV[1,GV_labels] = NA
      GV[1,"ZnNote"] = ZnNote
      
    } else {
      
      # if(input$DOC>40 | input$pH>8.5 | input$Hardness>529){
      #   ZnNote <- "TMF(s) outside applicable model range"
      #   
      # } else if (input$DOC>15 | input$pH >8.1 | input$Hardness> 370){
      #   ZnNote <- "TMF(s) outside applicable model range"
      #   
      # } else if (input$DOC<0.3 | input$pH<5.6 | input$Hardness<5) {
      #   ZnNote <- "TMF(s) outside applicable model range"
      #   
      # } else if (input$DOC<0.5 | input$pH<6.7| input$Hardness<26) {
      #   ZnNote <- "TMF(s) outside applicable model range"
      #   
      # } else {
      #   ZnNote <- "TMFs in applicable range, DGV suitable"
      # }
      
      # myDOC <- min(max(input$DOC, 0.5), 15)                 # Use this if we want to crop the TMF data to the MLR range
      # myH   <- min(max(input$H, 20), 440)
      # mypH  <- min(max(input$pH, 6.2), 8.3)
      
      ZnNote <- "TMF data in range, GV suitable"
      
      myDOC <- input$DOC                                      # Use this if we want to calculate anyway & decide on issues later
      myH   <- input$Hardness
      mypH  <- input$pH
      
      # Zero out coefficients that are NA - will mean that these parts of the
      # general full equation below do not contribute to the formula
      
      tMLR[is.na(tMLR)] <- 0  
      
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
        
      }
      
      GV <- cbind(GV_temp, ZnNote)
    }
    
    names(GV) <- c(GV_labels, "ZnNote")
    myoutput <- cbind(input, GV)
    
    if (calc_biof) {
    
      DGV_zn <- ZnDGV_vals["PC95"]
      
      
      myoutput <- myoutput |>
        dplyr::mutate(ZnBioF = case_when(is.na(ZnPC95) ~ NA,                       # Calc Bioavailable fraction
                                         DGV_zn/ZnPC95 > 1 ~ 1,                    # Make 1 if > 1
                                         TRUE ~ DGV_zn/ZnPC95),
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
    
    return (myoutput)
   
  }
  
  
  
  GetAllGVs <- function(myTMF.df) {
    
    myTMF.df <- myTMF.df |> dplyr::mutate(row = row_number(), .before = everything()) # Used only for users to reorder the table display
    
    # Specify the columns to convert (if present)
    columns_to_convert <- c("DOC","pH", "Hardness", "Calcium", "Magnesium", "Copper", "Nickel", "Zinc")  # columns we use
    
    # Convert columns to numeric only if they are present in the df (should avoid crashing)
    myTMF.df[intersect(columns_to_convert, names(myTMF.df))] <- lapply(myTMF.df[intersect(columns_to_convert, 
                                                                                           names(myTMF.df))], as.numeric)
    
    # Convert data to NA if they were zero (only if cols are present in the df to avoid crashing)
    myTMF.df[intersect(columns_to_convert, names(myTMF.df))] <- lapply(myTMF.df[intersect(columns_to_convert, 
                                                                                           names(myTMF.df))], 
                                                                        function(x) ifelse(x == 0, NA, x))
   
    Alloutput = myTMF.df
    
    
    if ("Cu" %in% metals) {
      Cu.output <- myTMF.df |>
        dplyr::group_split(row) |>
        purrr::map(GetCuGuidelines) |>
        purrr::list_rbind()
      
      GV_labels = paste("Cu", pcs, sep="")
      
      if (calc_biof) GV_labels = c(GV_labels, "CuBio")
      
      if (rcr) GV_labels = c(GV_labels, paste0("Cu",gsub("PC","_HQ",pcs)))
      
      Alloutput <- cbind(Alloutput, Cu.output %>% dplyr::select(all_of(c(GV_labels, "CuNote"))))
      names(Alloutput) <- gsub("PC", "_BAGV", names(Alloutput))
      names(Alloutput) <- gsub("CuBio", "Cu_bioavailable", names(Alloutput))
      
      CuDGV_vals_sub = as.data.frame(t(CuDGV_vals[pcs]))
      CuDGV_vals_sub <- CuDGV_vals_sub |> mutate(across(is.numeric, signif, digits=2))
      names(CuDGV_vals_sub) <- paste0("Cu_DGV",names(CuDGV_vals_sub))
      names(CuDGV_vals_sub) <- gsub("PC", "", names(CuDGV_vals_sub))
      
      Alloutput <- cbind(Alloutput,CuDGV_vals_sub)
      Alloutput <- Alloutput |> relocate(CuNote, .after = last_col())
      
      i = nrow(summary)
      summary[i+1,"metal"] <<- "Copper"
      summary[i+1,"nExcluded"] <<- nrow(Cu.output[which(Cu.output$CuNote=="DOC missing"),])
      summary[i+1,"nGVs"] <<- nrow(Cu.output[which(Cu.output$CuNote=="TMF data in range, GV suitable"),])
      summary[i+1,"nOutofRange"] <<- nrow(Cu.output) - summary[i+1,"nExcluded"] - summary[i+1,"nGVs"]
      
    }
    
    if ("Ni" %in% metals) {
      
      Ni.output <- myTMF.df |>
        dplyr::group_split(row) |>
        purrr::map(GetNiGuidelines) |>
        purrr::list_rbind()
       
      GV_labels = paste("Ni", pcs, sep="")
      
      if (calc_biof) GV_labels = c(GV_labels, "NiBio")
      
      if (rcr) GV_labels = c(GV_labels, paste0("Ni",gsub("PC","_HQ",pcs)))
      
      Alloutput <- cbind(Alloutput, Ni.output %>% dplyr::select(all_of(c(GV_labels, "NiNote"))))
      names(Alloutput) <- gsub("PC", "_BAGV", names(Alloutput))
      names(Alloutput) <- gsub("NiBio", "Ni_bioavailable", names(Alloutput))
      
      NiDGV_vals_sub = as.data.frame(t(NiDGV_vals[pcs]))
      NiDGV_vals_sub <- NiDGV_vals_sub |> mutate(across(is.numeric, signif, digits=2))
      names(NiDGV_vals_sub) <- paste0("Ni_DGV",names(NiDGV_vals_sub))
      names(NiDGV_vals_sub) <- gsub("PC", "", names(NiDGV_vals_sub))
      
      Alloutput <- cbind(Alloutput,NiDGV_vals_sub)
      Alloutput <- Alloutput |> relocate(NiNote, .after = last_col())
      
      i = nrow(summary)
      summary[i+1,"metal"] <<- "Nickel"
      summary[i+1,"nExcluded"] <<- nrow(Ni.output[which(Ni.output$NiNote=="TMFs missing"),])
      summary[i+1,"nGVs"] <<- nrow(Ni.output[which(Ni.output$NiNote=="TMF data in range, GV suitable"),])
      summary[i+1,"nOutofRange"] <<- nrow(Ni.output) - summary[i+1,"nExcluded"] - summary[i+1,"nGVs"]
      
      
      #Alloutput <- Alloutput |> select(-row)
    }
    
    if ("Zn" %in% metals) {
      
      Zn.output <- myTMF.df |>
        dplyr::group_split(row) |>
        purrr::map(GetZnGuidelines) |>
        purrr::list_rbind()
      
      GV_labels = paste("Zn", pcs, sep="")
      
      if (calc_biof) GV_labels = c(GV_labels, "ZnBio")
      
      if (rcr) GV_labels = c(GV_labels, paste0("Zn",gsub("PC","HQ",pcs)))
      
      Alloutput <- cbind(Alloutput, Zn.output %>% dplyr::select(all_of(c(GV_labels, "ZnNote"))))
      names(Alloutput) <- gsub("PC", "_BAGV", names(Alloutput))
      names(Alloutput) <- gsub("ZnBio", "Zn_bioavailable", names(Alloutput))
      
      ZnDGV_vals_sub = as.data.frame(t(ZnDGV_vals[pcs]))
      ZnDGV_vals_sub <- ZnDGV_vals_sub |> mutate(across(is.numeric, signif, digits=2))
      names(ZnDGV_vals_sub) <- paste0("Zn_DGV",names(ZnDGV_vals_sub))
      names(ZnDGV_vals_sub) <- gsub("PC", "", names(ZnDGV_vals_sub))
      
      Alloutput <- cbind(Alloutput,ZnDGV_vals_sub)
      Alloutput <- Alloutput |> relocate(ZnNote, .after = last_col())
      
      i = nrow(summary)
      summary[i+1,"metal"] <<- "Zinc"
      summary[i+1,"nExcluded"] <<- nrow(Zn.output[which(Zn.output$ZnNote=="TMFs missing"),])
      summary[i+1,"nGVs"] <<- nrow(Zn.output[which(Zn.output$ZnNote=="TMF data in range, GV suitable"),])
      summary[i+1,"nOutofRange"] <<- nrow(Zn.output) - summary[i+1,"nExcluded"] - summary[i+1,"nGVs"]
      
    }
    
    return(Alloutput)
    
  }
  
  AllMetals <- GetAllGVs(myTMF.df=df)
  
  print(AllMetals)
  print(summary)
  
  return (list("results"=AllMetals, "summary"=summary, "plots"=plots))

}  
 
  
