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
  
  library(plyr)                        ## Required
  library(tidyverse)                   ## Required
  library(ssdtools)                    ## Required
  
  # Read coefficients and species data ################################
  
  metals = options$metals
  calc_biof = options$calc_biof
  pcs = options$pcs
  rcr = options$rcr
  
  pcs_all = c("PC99", "PC95", "PC90", "PC80")
  pcs_vals_all = c(1, 5, 10, 20)
  
  pcs_vals = pcs_vals_all[pcs_all %in% pcs]
  
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
  
  # Initialize output info
  
  summary = data.frame(matrix(nrow=0, ncol=3))                                   # dataframe summarising number of GVs calculated for each metal and the number of rows excluded
  names(summary) = c("metal","nGVs","nExcluded")
  
  print(summary)
  
  ######################################################################
  ## Functions to calculate GVs
  
  # Copper, just an equation
  
  GetCuGuidelines <- function(input){
    
    # Check input data. If data is missing do nothing, but keep the row.
    # Otherwise, write a note if any of the observations are out of the fitting
    # bounds
    
    GV <- data.frame(matrix(nrow=1, ncol=length(pcs)))
    GV_labels = paste("Cu", pcs, sep="")
    names(GV) = GV_labels
    
    #GV <- data.frame(matrix(nrow=1, ncol=length(pcs)+1))
    #GV_labels = paste("Cu", pcs, sep="")
    #names(GV) = c(GV_labels, "CuNote")
    
    if(is.na(input$DOC | !is.numeric(input$DOC)) #| is.na(input$pH) #| is.na(input$Hardness)
       ) {
      
      CuNote <- "DOC missing"
      
      GV[1,GV_labels] = NA
      #GV[1,"CuNote"] = CuNote
      
    } else {
      
        if(is.na(input$pH) | is.na(input$Hardness)){
        CuNote <- "pH and/or hardness not provided, DGV may or may not be applicable"
            
       } else if (input$DOC>30 | input$pH<6 |input$pH > 8.5 | input$Hardness<2 | input$Hardness>200){
        CuNote <- "pH, hardness or DOC outside DGV range"
        
      } else {
        CuNote <- "TMF data in range, GV suitable"   
      }
   
    
    # Apply equation form
    
    if ("PC99" %in% pcs) GV$CuPC99 <- ifelse(0.20*(input$DOC/0.5)^0.977 <1, 
                                             round(max(0.2, 0.20*(input$DOC/0.5)^0.977),1),
      signif(max(0.2, 0.20*(input$DOC/0.5)^0.977),2))
    if ("PC95" %in% pcs) GV$CuPC95 <- ifelse(0.47*(input$DOC/0.5)^0.977 <1, 
                                             round(max(0.47, 0.47*(input$DOC/0.5)^0.977),1),
                                             signif(max(0.47, 0.47*(input$DOC/0.5)^0.977),2))

    if ("PC90" %in% pcs) GV$CuPC90 <- signif(max(0.73, 0.73*(input$DOC/0.5)^0.977),2)
    if ("PC80" %in% pcs) GV$CuPC80 <- signif(max(1.3, 1.3*(input$DOC/0.5)^0.977),2)
    
    myoutput <- cbind(input, CuNote, GV)

    if (calc_biof & ("PC95" %in% pcs)) {
      
      myoutput <- myoutput |>
        mutate(CuBioF = case_when(is.na(CuPC95) ~ NA,
                                  0.47/CuPC95 > 1 ~ 1,
                                  TRUE ~ 0.47/CuPC95),     # Bioavailable fraction
               CuBio = case_when(is.na(CuPC95) ~ NA,
                                 is.na(CuBioF) ~ NA,
                                 !is.numeric(Copper) ~ NA,
                                 is.numeric(Copper) ~ signif(Copper*CuBioF,2)
                                 )                # Bioavailable Cu
       )
    }
    
    if (rcr & ("PC95" %in% pcs)) {
      myoutput <- myoutput |>
        mutate(Cu_HQ = ifelse((is.na(CuPC95)|is.na(Copper)), NA, signif(Copper/CuPC95,2))     # RCR Cu
        )
    }
    return(myoutput)
    
    }
  }
  
  # Zinc, with MLR adjustment
  
  GetZnGuidelines <- function(sens=ZnSSD.df, tMLR=ZnMLR.coeffs, input){
    
    # Check input data. If data is missing do nothing, but keep the row.
    # Otherwise, write a note if any of the observations are out of the fitting
    # bounds
    
    GV <- data.frame(matrix(nrow=1, ncol=length(pcs)+1))
    GV_labels = paste("Zn", pcs, sep="")
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
        print(GV_temp)
        GV_temp <- GV_temp |>
          mutate(across(is.numeric, ~case_when(.x <1 ~ round(.x, digits=1),
                                               TRUE ~ signif(.x, 2))))
        
        rownames(GV_temp) <- NULL
        #GV = as.data.frame(t(ssd_hc(res, percent=c(1, 5, 10, 20), ci=FALSE, nboot=10)[,3])) 
        #rownames(GV) <- NULL
      }
      
      GV <- cbind(GV_temp, ZnNote)
    }
    
    names(GV) <- c(GV_labels, "ZnNote")
    myoutput <- cbind(input, GV)
    
    if (calc_biof & ("PC95" %in% pcs)) {
      
      myoutput <- myoutput |>
        mutate(ZnBioF = case_when(is.na(ZnPC95) ~ NA,  # Calc Bioavailable fraction
                                  4.1/ZnPC95 > 1 ~ 1,  # Make 1 if > 1
                                  TRUE ~ 4.1/ZnPC95),
      ZnBio = case_when(is.na(ZnPC95) ~ NA,
                        is.na(ZnBioF) ~ NA,
                        !is.numeric(Zinc) ~ NA,
                        is.numeric(Zinc) ~ signif(Zinc*ZnBioF,2)                # Bioavailable Zn
      )
      )
    }
    
    if (rcr & ("PC95" %in% pcs)) {

      myoutput <- myoutput |>
        mutate(Zn_HQ = ifelse((is.na(ZnPC95)|is.na(Zinc)), NA, signif(Zinc/ZnPC95,2))     # RCR Zn
        )
      
    }
    
    return (myoutput)
   
  }
  
  # Nickel, with MLR adjustment
  
  GetNiGuidelines <- function(sens=NiSSD.df, tMLR=NiMLR.coeffs, input){
    
    # Check input data. If data is missing do nothing, but keep the row.
    # Otherwise, write a note if any of the observations are out of the fitting
    # bounds
    
    GV <- data.frame(matrix(nrow=1, ncol=length(pcs)+1))
    GV_labels = paste("Ni", pcs, sep="")
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
      myH   <- input$Hardness
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
          mutate(across(is.numeric, ~case_when(.x <1 ~ round(.x, digits=1),
                                               TRUE ~ signif(.x, 2))))
          rownames(GV_temp) <- NULL
      }
      
      GV <- cbind(GV_temp, NiNote) 
      
    }
    
    names(GV) <- c(GV_labels, "NiNote")
    myoutput <- cbind(input, GV)
    

    if (calc_biof & ("PC95" %in% pcs)) {
      
      myoutput <- myoutput |>
        mutate(NiBioF = case_when(is.na(NiPC95) ~ NA,  # Calc bioavailable fraction
                                  1.1/NiPC95 > 1 ~ 1,  # Make 1 if > 1
                                  TRUE ~ 1.1/NiPC95),  
               NiBio = case_when(is.na(NiPC95) ~ NA,
                                 is.na(NiBioF) ~ NA,
                                 !is.numeric(Nickel) ~ NA,
                                 is.numeric(Nickel) ~ signif(Nickel*NiBioF,2)
               )
               )               # Bioavailable Ni
                
      
    }
    
    if (rcr & ("PC95" %in% pcs)) {
      
      myoutput <- myoutput |>
        mutate(Ni_HQ = ifelse((is.na(NiPC95)|is.na(Nickel)), NA, signif(Nickel/NiPC95,2))     # RCR Ni
        )
    }
    return (myoutput)
    
  }
  
  GetAllGVs <- function(myTMF.df) {
     myTMF.df <- myTMF.df |> mutate(row = row_number(),
                                    Copper = as.numeric(Copper),
              Nickel = as.numeric(Nickel),
              Zinc = as.numeric(Zinc),
              DOC = as.numeric(DOC),
              pH = as.numeric(pH),
              Hardness = as.numeric(Hardness),
              Ca = as.numeric(Ca),
              Mg = as.numeric(Mg)
      )
    
    
    Alloutput = myTMF.df
    
    if ("Cu" %in% metals) {
      
      Cu.output <- ddply(myTMF.df,.(row), function(x) GetCuGuidelines(input=x))
      GV_labels = paste("Cu", pcs, sep="")
      if (calc_biof & ("PC95" %in% pcs)) GV_labels = c(GV_labels, "CuBio")
      if (rcr & ("PC95" %in% pcs)) GV_labels = c(GV_labels, "Cu_HQ")
      Alloutput <- cbind(Alloutput, Cu.output %>% dplyr::select(all_of(c(GV_labels, "CuNote"))))
      
      i = nrow(summary)
      summary[i+1,"metal"] <<- "Copper"
      summary[i+1,"nExcluded"] <<- nrow(Cu.output[which(Cu.output$CuNote=="TMFs missing"),])
      summary[i+1,"nGVs"] <<- nrow(Cu.output) - summary[i+1,"nExcluded"]
    }
    
    if ("Zn" %in% metals) {
      
      Zn.output <- ddply(myTMF.df,.(row), function(x) GetZnGuidelines(input=x))
      GV_labels = paste("Zn", pcs, sep="")
      if (calc_biof & ("PC95" %in% pcs)) GV_labels = c(GV_labels, "ZnBio")
      if (rcr & ("PC95" %in% pcs)) GV_labels = c(GV_labels, "Zn_HQ")
      Alloutput <- cbind(Alloutput, Zn.output %>% dplyr::select(all_of(c(GV_labels, "ZnNote"))))
      
      i = nrow(summary)
      summary[i+1,"metal"] <<- "Zinc"
      summary[i+1,"nExcluded"] <<- nrow(Zn.output[which(Zn.output$ZnNote=="TMFs missing"),])
      summary[i+1,"nGVs"] <<- nrow(Zn.output) - summary[i+1,"nExcluded"]
    }
    
    if ("Ni" %in% metals) {
      
      Ni.output <- ddply(myTMF.df,.(row), function(x) GetNiGuidelines(input=x))
      GV_labels = paste("Ni", pcs, sep="")
      if (calc_biof & ("PC95" %in% pcs)) GV_labels = c(GV_labels, "NiBio")
      if (rcr & ("PC95" %in% pcs)) GV_labels = c(GV_labels, "Ni_HQ")
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
  
  return (list("results"=AllMetals, "summary"=summary))

}  
 
  
