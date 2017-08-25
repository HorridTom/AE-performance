
load_raw_los_data <- function() {
  ed_los <- read.csv(file = "data-raw/ED-los.csv")
  save(ed_los, file="data/ED-los.rda")
}

load_raw_dtoc_data <- function() {
  dtoc_london <- read.csv(file = "data-raw/DTOC_London.csv", stringsAsFactors = FALSE)
  save(dtoc_london, file = "data/DTOC_London.rda")
}

load_raw_4h_data <- function() {
  perf_4h <- read.csv(file = "data-raw/nhs_4h_performance.csv", stringsAsFactors = FALSE)
  save(perf_4h, file = "data/perf_4h.rda")
}

# Note current process still not fully automated:
# 1. ensure ED-los.csv and nhs_4h_performance.csv are in data-raw
# 2. in the nhs_4h_performance.csv file, ensure that in the three flag columns,
# the 1s are replaced by TRUE and the NULLs are replaced by FALSE
# 3. run the load_raw_... functions
# 4. clean the data using the clean_... functions (loading raw .rda s into workspace to do so)
# 5. generate the organisation table using pseudo_table.