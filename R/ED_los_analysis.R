make_bins <- function(df, los_col = 'Der_AEA_Duration', bin_col = 'los_bin', default_labels = TRUE) {
  
  max_los <- max(df[,los_col], na.rm=TRUE)
  bins <- seq(from = 0, to = 24*15, by= 15)
  bin_labels <- make_bin_labels(bins)
  bins <- append(bins, max_los + 1)
  
  if(default_labels){
    df[,bin_col] <- cut(df[,los_col], breaks = bins, right = FALSE)
  } else{
    df[,bin_col] <- cut(df[,los_col], breaks = bins, labels = bin_labels)
  }
  df
}

make_bin_labels <- function(bins) {
  bin_chars <- as.character(bins)
  bin_chars_shift <- bin_chars[-1]
  bin_chars_shift <- append(bin_chars_shift,"")
  bin_labels <- paste(bin_chars,bin_chars_shift,sep="-")
  bin_labels
}

plot_ed_dist <- function(df, prov_codes = c("RBZ"), cumulative = TRUE) {
  prov_data <- df[df$Der_Provider_Code %in% prov_codes,]
  
  
  if (nrow(prov_data) == 0) {return(NULL)}
  ed_dist <- aggregate(prov_data$Activity, by=list(Duration=prov_data$los_bin, Admitted=prov_data$admitted), FUN=sum)
  colnames(ed_dist)[3] <- "Frequency"
  ed_dist[ed_dist$Admitted==FALSE,"cumul_sum"] <- cumsum(ed_dist[ed_dist$Admitted==FALSE,"Frequency"])
  ed_dist[ed_dist$Admitted==TRUE,"cumul_sum"] <- cumsum(ed_dist[ed_dist$Admitted==TRUE,"Frequency"])
  
  if(cumulative) {
    ed_dist_m <- reshape2::melt(ed_dist, id = c("Duration", "Admitted"))
  } else {ed_dist_m <- ed_dist}
  
  pp <- ggplot(data = ed_dist_m, aes(x = Duration, y = value, group = Admitted, colour = Admitted))
  pp <- pp + geom_path() + geom_point() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  
  if(cumulative) {pp <- pp + facet_grid(variable ~ ., scales = "free")}
  pp
    
}

make_adm_colour_palette <- function(levs) {
  adm_colours <- RColorBrewer::brewer.pal(4,"Dark2")
  names(adm_colours) <- append(levs, c(NA, NA), after = 1)
  admColScale <- scale_colour_manual(name="Admitted", values = adm_colours)
  admColScale
}