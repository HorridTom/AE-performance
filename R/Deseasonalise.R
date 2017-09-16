library(tidyverse)
library(zoo)
library(bsts)


make_seasonal_relatives <- function(df, period_col,
                               baseline_periods = NULL) {
  
  period_col <- enquo(period_col)
  
  # If baseline periods are not specified, use all. 
  if (is.null(baseline_periods)) {
    baseline_periods = df %>% distinct(!!period_col)
    baseline_periods = pull(baseline_periods, !!period_col)
    }
  
  
  period_averages <- df %>% group_by(!!period_col) %>% summarise(Period_Av = mean(Performance))
  
  df <- df %>% left_join(period_averages)
  # Only return period averages for periods in the baseline
  df <- df %>% mutate(Period_Av = ifelse(Yr %in% baseline_periods, Period_Av, NA))
  
  df %>% mutate(seasonal_relative = Performance/Period_Av)
  
}


get_seasonal_factors <- function(df, season_col) {
  
  season_col <- enquo(season_col)
  
  df %>% group_by(!!season_col) %>% summarise(seasonal_factor = mean(seasonal_relative, na.rm = TRUE))
  
}


deseasonalise <- function(df, season_col, period_col, baseline_periods = NULL) {
  
  df <- make_seasonal_relatives(df = df, period_col = !!enquo(period_col), baseline_periods = baseline_periods)
  sf <- get_seasonal_factors(df = df, season_col = !!enquo(season_col))
  
  df <- df %>% left_join(sf)
  
  df %>% mutate(Deseasonalised_Performance = Performance / seasonal_factor)
  
}


convert_weekly_to_monthly <- function(df, date_col, measure_col) {
  
  # First make two zoo objects, one for the weekly measure and one to count weeks in the month
  mps <- read.zoo(df[,which(colnames(df) %in% c(date_col,measure_col))])
  df$wc <- rep(1,nrow(df))
  mps_wc <- read.zoo(df[,which(colnames(df) %in% c(date_col,"wc"))])
  
  # Create monthly series, aportioning weeks across month boundaries linearly
  monthly_sums <- AggregateWeeksToMonths(weekly.series = mps)
  num_weeks <- AggregateWeeksToMonths(weekly.series = mps_wc)
  monthly_measure <- monthly_sums/num_weeks

  mdf <- data.frame(month = index(monthly_measure), as.data.frame(monthly_measure))
  mdf
}