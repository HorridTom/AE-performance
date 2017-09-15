make_seasonal_relatives <- function(df, season_col, period_col,
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