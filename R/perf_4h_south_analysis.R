library(tidyverse)
library(ggplot2)
library(scales)

make_perf_series <- function(df, prov_codes = c("RBZ"), perf_only = FALSE, 
                             adm_only = FALSE, all_provs = FALSE,
                             dept_types = c('1','2','3')) {
  df <- df[which(df$AEA_Department_Type %in% dept_types),]
  
  if (!all_provs) {data_prov <- df[df$Prov_Code %in% prov_codes,]} else {
    data_prov <- df
  }
  
  if (adm_only) {
    data_prov <- data_prov[data_prov$Admitted == TRUE,]
  }
  
  if (all_provs) {
    south_4h <- aggregate(Activity ~ Wk_End_Sun + Greater_4h, data = data_prov, sum)
  } else {
    south_4h <- aggregate(Activity ~ Wk_End_Sun + Prov_Code + Greater_4h, data = data_prov, sum)
  }
  perf_4h_wide <- tidyr::spread(data = south_4h, key = Greater_4h, value = Activity)
  
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

plot_performance <- function(df, prov_codes = c("RBZ"), date.col = 'Wk_End_Sun',
                             start.date = "2014-01-01", end.date = "2017-02-28",
                             brk.date = "2016-01-01", max_lower_y_scale = 60,
                             adm_only = FALSE, all_provs = FALSE,
                             dept_types = c('1','2','3'), plot.chart = TRUE,
                             pr_name = NULL) {
  # pass df as cleaned 4h perf data from the clean_4h_data function
  
  # if no pr_name passed, lookup full name of provider
  # note written for just one provider
  if (is_null(pr_name)) {
    pr_name <- df[which(df$Prov_Code == prov_codes),"Prov_Name"][[1]]
  }
  #cht_title = paste("Weekly percentage ED attendances with time in department < 4h",pr_name,sep="\n")
  if (adm_only) {
    cht_title = paste("Weekly percentage admissions through ED \n with time in department < 4h. Dept. types: ",paste(dept_types,sep="",collapse=","),sep="")
  } else {
    cht_title = paste("Weekly percentage ED attendances \n with time in department < 4h. Dept. types: ",paste(dept_types,sep="",collapse=","),sep="")
  }
  
  df <- make_perf_series(df = df, prov_codes = prov_codes, adm_only = adm_only,
                         all_provs = all_provs, dept_types = dept_types)
  
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
  
  pct <- qicharts::tcc(n = Within_4h, d = df$Total, x = df$Wk_End_Sun, data = df, chart = 'p', multiply = 100, prime = TRUE, breaks = c(br.row), runvals = TRUE, cl.lab = TRUE)
  
  # chart y limit
  ylimlow <- min(min(pct$data$y, na.rm = TRUE),min(pct$data$lcl, na.rm = TRUE),max_lower_y_scale)
  
  col1    <- rgb(000, 000, 000, maxColorValue = 255)
  col2    <- rgb(241, 088, 084, maxColorValue = 255)
  col3    <- rgb(000, 000, 000, maxColorValue = 255)
  col4    <- 'white'
  col5    <-  rgb(096, 189, 104, maxColorValue = 255)
  cols    <- c('col1' = col1, 'col2' = col2, 'col3' = col3, 'col4' = col4)

  if(plot.chart == TRUE) {pct + geom_line(aes_string(x = 'x', y = 'lcl', group = 'breaks'), colour = '#000000', linetype = 'dashed') +
    geom_line(aes_string(x = 'x', y = 'ucl', group = 'breaks'), colour = '#000000', linetype = 'dashed') +
    geom_line(aes_string(x = 'x', y = 'cl', group = 'breaks'), colour = '#000000', linetype = 1) +
    geom_line(aes_string(x = 'x', y = 'y', group = 'breaks'), colour = '#000000', linetype = 1, lwd = 1.1) + 
    geom_point(aes_string(x = 'x', y = 'y', group = 'breaks', fill = 'pcol'), size = 2) + 
    scale_fill_manual(values = cols) + scale_color_manual(values = cols) +
    labs(title = cht_title, x="Week Ending Sunday", y="Percentage", subtitle = pr_name) +
    ylim(ylimlow,100) + scale_x_date(labels = date_format("%Y-%m"), breaks = date_breaks("3 months"), limits = as.Date(c(start.date, end.date))) + theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.75), plot.title = element_text(hjust = 0.5), axis.line = element_line(colour = "grey60"))
  } else {df}
    
}

plot_volume <- function(df, prov_codes = c("RBZ"), date.col = 'Wk_End_Sun',
                        start.date = "2014-01-01", end.date = "2017-02-28",
                        brk.date = "2016-01-01", min_upper_y_scale = 3000,
                        adm_only = FALSE, all_provs = FALSE,
                        dept_types = c('1','2','3')) {
  # pass df as cleaned 4h perf data from the clean_4h_data function
  
  # lookup full name of provider
  # note written for just one provider
  pr_name <- df[which(df$Prov_Code == prov_codes),"Prov_Name"][[1]]
  #cht_title = paste("Weekly total ED attendances",pr_name,sep="\n")
  if (adm_only) {
    cht_title = paste("Weekly total admissions through ED. Dept. types: ",paste(dept_types,sep="",collapse=","),sep="")
    y_axis_lab = "Admissions"
  } else {
    cht_title = paste("Weekly total ED attendances. Dept. types: ",paste(dept_types,sep="",collapse=","),sep="")
    y_axis_lab = "Attendances"
  }
  
  df <- make_perf_series(df = df, prov_codes = prov_codes, adm_only = adm_only, all_provs = all_provs, dept_types = dept_types)
  
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
        axis.line=element_line(colour = "grey75"), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.75)) +
  labs(title = cht_title, x="Week Ending Sunday", y=y_axis_lab) +
  geom_vline(xintercept = as.numeric(br.dt), colour="grey60") +
  scale_x_date(labels = date_format("%Y-%m"),breaks = date_breaks("3 months")) + theme(axis.text.x = element_text(angle=45), plot.title = element_text(hjust = 0.5))
  
}

