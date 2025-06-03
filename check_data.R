# Check the data - will probably need to pass options selected for calculating
# the GVs. For now, just do some simple checks

check_data <- function(df, options){
 
  # Initialize dataframe which will return information about any issues
  # identified in df
  
  issue_df = data.frame("row"=numeric(), "col"=numeric(), "type"=character(), "message"=character())
  
  # Check that df contains the required columns
  
  cols = c("DOC")                                                                # all metals require DOC
  
  if ("Ni" %in% options$metals | "Zn" %in% options$metals) {                     # Ni and Zn require pH
    cols = c(cols, "pH")                                               
  }
  
  if ("Ni" %in% options$metals) {
    cols = c(cols, "Calcium", "Magnesium")                                       # Ni requires Ca and Mg
  }
  
  if ("Zn" %in% options$metals) {
    cols = c(cols, "Hardness")                                                   # Zn requires hardness
  }
  
  metal_labels = c("Cu"="Copper", "Ni"="Nickel", "Zn"="Zinc")
  if (options$calc_biof == TRUE | options$rcr == TRUE) {                         # if calculating bioavailable metals,
    cols = c(cols, metal_labels[options$metals])                                 # metal columns are also required
  }
  
  missing = which((cols %in% names(df)) == FALSE)
  
  for (i in missing) {
    N = nrow(issue_df)
    issue_df[N+1,] = c(0, 0, "error", paste("Error: missing column", cols[i], sep=" "))
  }
  issue_df$row = as.numeric(issue_df$row)
  issue_df$col = as.numeric(issue_df$col)
  
  # If Hardness is identified as missing, and Ca and Mg are available, calculate
  # Hardness from Ca and Mg and update the issue dataframe
  
  i = which(grepl("Error: missing column Hardness", issue_df$message))
  
  if (length(i) > 0) {
    if ("Calcium" %in% names(df) & "Magnesium" %in% names(df)) {
      Calcium = as.numeric(df$Calcium)
      Magnesium = as.numeric(df$Magnesium)
      df$Hardness = 2.497*Calcium + 4.118*Magnesium
      issue_df$type[i] = "warning"
      issue_df$message[i] = "Warning: missing column Hardness, will be calculated from Calcium and Magnesium"
    }
  }

  # Identify any missing data in required columns that are in the dataset
  
  cols_in = cols[cols %in% names(df)]
  
  missing = which(is.na(df[cols_in]), arr.ind=TRUE)
  
  if (nrow(missing) > 0) {
    issue_df = rbind(issue_df, data.frame("row"=missing[,1], "col"=missing[,2], "type"="warning", "message"=paste("Warning: missing data in column", cols_in[missing[,2]], sep=" ")))
  }
  
  # Check for non-numeric data in required columns that are in the dataset
  
  df[cols_in] = sapply(df[cols_in], as.numeric)                                  # convert to numeric
  
  na_vals = which(is.na(df[cols_in]), arr.ind=TRUE)                              # indices of na values including missing data and values that couldn't be converted to numeric
  non_numeric = dplyr::setdiff(data.frame(na_vals), data.frame(missing))         # indices of na values excluding missing data
  
  if (nrow(non_numeric) > 0) {
    issue_df = rbind(issue_df, data.frame("row"=non_numeric[,1], "col"=non_numeric[,2], "type"="warning", "message"=paste0("Warning: non-numeric data in column ", cols_in[non_numeric[,2]], ", will be converted to NA")))
  }
  
  # Check for zero data in required columns that are in the dataset
  
  zeros = which(df[cols_in]==0, arr.ind=TRUE)
  
  if (nrow(zeros) > 0) {
    issue_df = rbind(issue_df, data.frame("row"=zeros[,1], "col"=zeros[,2], "type"="warning", "message"=paste0("Warning: values of zero in column ", cols_in[zeros[,2]], ", will be converted to NA")))
  }
  
  # Check for negative data in required columns that are in the dataset
  
  negative = which(df[cols_in]<0, arr.ind=TRUE)
  
  if (nrow(negative) > 0) {
    issue_df = rbind(issue_df, data.frame("row"=negative[,1], "col"=negative[,2], "type"="error", "message"=paste0("Error: negative data in column ", cols_in[negative[,2]])))
  }
  
  results = list("cols_in"=cols_in, "issue_df"=issue_df, "df_checked"=df)
  
  return(results)
  
}