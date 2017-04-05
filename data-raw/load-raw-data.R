
load_raw_los_data <- function() {
  ed_los <- read.csv(file = "data-raw/ED-los.csv")
  save(ed_los, file="data/ED-los.rda")
}

load_raw_dtoc_data <- function() {
  dtoc_london <- read.csv(file = "data-raw/DTOC_London.csv", stringsAsFactors = FALSE)
  save(dtoc_london, file = "data/DTOC_London.rda")
}