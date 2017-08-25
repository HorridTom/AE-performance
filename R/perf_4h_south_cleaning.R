clean_4h_data <- function(df, sv = TRUE, rt = FALSE, disp_propmiss = TRUE) {
  
  df[,"Wk_Start_Mon"] <- as.Date(df[,"Wk_Start_Mon"], "%d/%m/%Y")
  df[,"Wk_End_Sun"] <- as.Date(df[,"Wk_End_Sun"], "%d/%m/%Y")
  
  #df[,"Category"] <- as.factor(df[,"Category"])
  df[,"Prov_Code"] <- as.factor(df[,"Prov_Code"])
  
  #Need to add functionality to check for 1/NULL as opposed to TRUE/FALSE...
  
  if(disp_propmiss) {
    print(propmiss(df))
  }
  
  if(sv) {
    perf_4h_clean <- df
    save(perf_4h_clean, file = "data/perf_4h_clean.rda")
  }
  
  if(rt) {
    df
  } else {TRUE}
}