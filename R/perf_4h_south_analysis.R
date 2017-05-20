make_perf_series <- function(df, prov_codes = c("RBZ"), perf_only = FALSE) {
  
  data_prov <- df[df$Prov_Code %in% prov_codes,]
  
  south_4h <- aggregate(Activity ~ Wk_End_Sun + Prov_Code + Greater_4h, data = data_prov, sum)
  perf_4h_wide <- spread(data = south_4h, key = Greater_4h, value = Activity)
  
  colnames(perf_4h_wide)[colnames(perf_4h_wide) == "FALSE"] <- "Within_4h"
  colnames(perf_4h_wide)[colnames(perf_4h_wide) == "TRUE"] <- "Greater_4h"

  perf_4h_wide$Total <- perf_4h_wide$Within_4h + perf_4h_wide$Greater_4h
  perf_4h_wide$Performance <- perf_4h_wide$Within_4h / perf_4h_wide$Total
  
  perf_4h_wide <- perf_4h_wide[order(perf_4h_wide$Wk_End_Sun),]
  
  if (perf_only) {
    perf_4h_wide <- perf_4h_wide$Performance
  }
  

  perf_4h_wide

}

write_for_algorithm <- function(df) {
  
  providerCodes <- unique(df$Prov_Code)
  for (prov in providerCodes) {
    df_out <- make_perf_series(df, prov_codes = prov, perf_only = TRUE)
    write.table(df_out, file = paste("data-out/",prov,"_perf.csv",sep=""),row.names = FALSE, col.names = FALSE, sep=",")
  }
  
}

plot_performance_qcc <- function(df) {

  library(qcc)
  
  pcc1 <- qcc(data = df[1:104,"Within_4h"], sizes = df[1:104,"Total"], plot = FALSE, type = "p", newdata = df[105:nrow(df),"Within_4h"], newsizes = df[105:nrow(df),"Total"], chart.all = TRUE, data.name = "2-year Baseline")
  pcc1$newdata.name <- "Post-baseline"
  plot(pcc1, title = "Proportion of ED attendances in department for 4h or less", xlab = "Week", ylab = "Proportion attendances", yaxt="n")
  axis(2, at=pretty(c(pcc1$statistics,pcc1$newstats)), lab = paste(pretty(c(pcc1$statistics,pcc1$newstats)) * 100, "%"), las=TRUE)

}

plot_performance <- function(df, prov_codes = c("RBZ"), date.col = 'Wk_End_Sun', start.date = "2014-01-01", end.date = "2017-02-28", brk.date = "2016-01-01", max_lower_y_scale = 60) {
  # pass df as cleaned 4h perf data from the clean_4h_data function
  
  # lookup full name of provider
  # note written for just one provider
  pr_name <- df[which(df$Prov_Code == prov_codes),"Prov_Name"][[1]]
  #cht_title = paste("Weekly percentage ED attendances with time in department < 4h",pr_name,sep="\n")
  cht_title = "Weekly percentage ED attendances with time in department < 4h"
  
  df <- make_perf_series(df = df, prov_codes = prov_codes)
  
  st.dt <- as.Date(start.date)
  ed.dt <- as.Date(end.date)
  br.dt <- as.Date(brk.date)
  
  # restrict to the period specified
  df <- df[df[,date.col] >= st.dt & df[,date.col] <= ed.dt,]
  
  # locate break row
  v <- df[,date.col]
  br.row <- which(v == max(v[v < br.dt]))
  
  # This is a hack - find better way to modify colours of qicharts
  # Also needs stepped limits
  
  pct <- tcc(n = Within_4h, d = df$Total, x = df$Wk_End_Sun, data = df, chart = 'p', multiply = 100, prime = TRUE, breaks = c(br.row), runvals = TRUE, cl.lab = TRUE)
  
  # chart y limit
  ylimlow <- min(min(pct$data$y, na.rm = TRUE),min(pct$data$lcl, na.rm = TRUE),max_lower_y_scale)
  
  col1    <- rgb(000, 000, 000, maxColorValue = 255)
  col2    <- rgb(241, 088, 084, maxColorValue = 255)
  col3    <- rgb(000, 000, 000, maxColorValue = 255)
  col4    <- 'white'
  col5    <-  rgb(096, 189, 104, maxColorValue = 255)
  cols    <- c('col1' = col1, 'col2' = col2, 'col3' = col3, 'col4' = col4)

  pct + geom_line(aes_string(x = 'x', y = 'lcl', group = 'breaks'), colour = '#000000', linetype = 'dashed') +
    geom_line(aes_string(x = 'x', y = 'ucl', group = 'breaks'), colour = '#000000', linetype = 'dashed') +
    geom_line(aes_string(x = 'x', y = 'cl', group = 'breaks'), colour = '#000000', linetype = 1) +
    geom_line(aes_string(x = 'x', y = 'y', group = 'breaks'), colour = '#000000', linetype = 1, lwd = 1.1) + 
    geom_point(aes_string(x = 'x', y = 'y', group = 'breaks', fill = 'pcol'), size = 2) + 
    scale_fill_manual(values = cols) + scale_color_manual(values = cols) +
    labs(title = cht_title, x="Week Ending Sunday", y="Percentage") +
    ylim(ylimlow,100)
  
    
}

plot_volume <- function(df, prov_codes = c("RBZ"), date.col = 'Wk_End_Sun', start.date = "2014-01-01", end.date = "2017-02-28", brk.date = "2016-01-01", min_upper_y_scale = 3000) {
  # pass df as cleaned 4h perf data from the clean_4h_data function
  
  # lookup full name of provider
  # note written for just one provider
  pr_name <- df[which(df$Prov_Code == prov_codes),"Prov_Name"][[1]]
  #cht_title = paste("Weekly total ED attendances",pr_name,sep="\n")
  cht_title = "Weekly total ED attendances"
  
  df <- make_perf_series(df = df, prov_codes = prov_codes)
  
  st.dt <- as.Date(start.date)
  ed.dt <- as.Date(end.date)
  br.dt <- as.Date(brk.date)
  
  # restrict to the period specified
  df <- df[df[,date.col] >= st.dt & df[,date.col] <= ed.dt,]
  
  # chart y limit
  ylimhigh <- max(max(df$Total),min_upper_y_scale)
  
  pp <- ggplot(data = df, aes(x = Wk_End_Sun, y = Total))
  
  pp + geom_path() + geom_point() + ylim(0,ylimhigh) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
        axis.line=element_line(colour = "grey75"), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(title = cht_title, x="Week Ending Sunday", y="Attendances") +
  geom_vline(xintercept = as.numeric(br.dt), colour="grey60")
  
}

