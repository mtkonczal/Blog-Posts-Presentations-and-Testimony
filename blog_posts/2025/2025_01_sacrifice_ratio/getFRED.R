
getFRED <- function(variable, keep_all = TRUE, rename_variables = NA, lagged = NA){
  variable <- toupper(variable)
  for (i in seq_along(variable)){
    df_new <- read.csv(paste0("https://fred.stlouisfed.org/series/", variable[i], "/downloaddata/", variable[i], ".csv"))
    colnames(df_new) <- c("date", tolower(variable[i]))
    if(!is.na(lagged[i])){
      if(variable[i] %in% toupper(lagged)){
        df_new[tolower(variable[i])] <- df_new[[tolower(variable[i])]]/lag(df_new[[tolower(variable[i])]]) - 1
      }
    }
    if(!is.na(rename_variables[i])){
      colnames(df_new) <- c("date", rename_variables[i])
    }
    if(i == 1){
      df <- df_new
    } else{
      df <- merge(df, df_new, by = "date", all = keep_all)
    }
  }
  df$date <- as.Date(df$date)
  return(df)
}