
load_raw_los_data <- function() {
  load('data-raw/ed_los_colname_mapping.rda')
  fileNames <- Sys.glob("data-raw/ED-los*.csv")
  data_frames <- load_csv_files(fileNames)
  df <- do.call("rbind", data_frames)
  ed_los <- map_column_names(df, ed_los_colname_mapping)
  
  # ed_los <- read.csv(file = "data-raw/ED-los.csv")
  save(ed_los, file="data/ED-los.rda")
}

load_raw_dtoc_data <- function() {
  dtoc_london <- read.csv(file = "data-raw/DTOC_London.csv", stringsAsFactors = FALSE)
  save(dtoc_london, file = "data/DTOC_London.rda")
}

load_raw_4h_data <- function() {
  load('data-raw/perf_colname_mapping.rda')
  fileNames <- Sys.glob("data-raw/nhs_4h_performance*.csv")
  data_frames <- load_csv_files(fileNames)
  df <- do.call("rbind", data_frames)[1:9]
  perf_4h <- map_column_names(df, perf_colname_mapping)
  perf_4h <- correct_booleans(perf_4h)
  
  #perf_4h <- read.csv(file = "data-raw/nhs_4h_performance.csv", stringsAsFactors = FALSE)
  save(perf_4h, file = "data/perf_4h.rda")
}

# Note current process still not fully automated:
# 1. ensure ED-los*.csv and nhs_4h_performance*.csv files are in data-raw
# 
# 3. run the load_raw_... functions
# 4. clean the data using the clean_... functions (loading raw .rda s into workspace to do so)
# 5. generate the organisation table using pseudo_table.

map_column_names <- function(df, col_mapping) {
  raw_col_names <- colnames(df)
  standard_colnames <- dplyr::left_join(as.data.frame(raw_col_names),
                                        col_mapping, by = c('raw_col_names' = 'raw')) %>%
    select(standard)
  colnames(df) <- standard_colnames[[1]]
  df
  
}

correct_booleans <- function(df) {
  df <- df %>% mutate(Admitted = recode(Admitted, '1' = TRUE, 'NULL' = FALSE),
                      Greater_4h = recode(Greater_4h, '1' = TRUE, 'NULL' = FALSE),
                      Greater_12h = recode(Greater_12h, '1' = TRUE, 'NULL' = FALSE))
  df
}


#' load_csv_files
#'
#' load_csv_files loads in a set of csv files as dataframes
#'
#' @param fl list of paths of the files to be loaded
#'
#' @return A list of dataframes, one for each of the file paths in fl.
#' @export
load_csv_files <- function(fl) {
  
  files <- lapply(fl, function(x) read.csv(x, stringsAsFactors = FALSE))
  files
  
}