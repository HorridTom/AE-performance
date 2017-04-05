
clean_los_data <- function(df, sv = TRUE, rt = FALSE, disp_propmiss = TRUE) {
  
  # Convert the following columns to numeric
  df[,"Der_AEA_Duration"] <- as.integer(levels(df[,"Der_AEA_Duration"]))[df[,"Der_AEA_Duration"]]
  
  # Convert the following columns to character
  df[,"Provider_Name"] <- as.character(df[,"Provider_Name"])
  df[,"AEA_Attendance_Disposal_Desc"] <- as.character(df[,"AEA_Attendance_Disposal_Desc"])
  
  if(disp_propmiss) {
    print(propmiss(df))
  }
  
  if(sv) {
    save(df, file = "data/ED-los-clean.rda")
  }
  
  if(rt) {
    df
  } else {TRUE}
}

propmiss <- function(dataframe) {
  lapply(dataframe,function(x) data.frame(nmiss=sum(is.na(x)), n=length(x), propmiss=sum(is.na(x))/length(x)))
}