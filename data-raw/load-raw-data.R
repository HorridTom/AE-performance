
load_raw_los_data <- function() {
  ed_los <- read.csv(file = "data-raw/ED-los.csv")
  save(ed_los, file="data/ED-los.rda")
}