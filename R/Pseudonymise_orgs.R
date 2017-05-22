pseudo_table <- function(df, org_col = 'Prov_Code', name_col = 'Prov_Name') {
  X <- unique(df[,which(colnames(df) %in% c(org_col, name_col))])
  X$rord <- rnorm(nrow(X))
  X <- X[order(X$rord),]
  X$pseudoid <- c(1:nrow(X))
  X$rord <- NULL
  
  organisation_codes <- X
  save(organisation_codes, file="data/organisation_codes.rda")
  write.csv(organisation_codes, file="data/organisation_codes.csv", row.names = FALSE)
  
  organisation_codes
}