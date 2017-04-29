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

plot_ed_dist <- function(df, prov_codes = c("RBZ"), plot_line = TRUE) {
  prov_data <- df[df$Der_Provider_Code %in% prov_codes,]
  ed_dist <- aggregate(prov_data$Activity, by=list(Category=prov_data$los_bin), FUN=sum)
  colnames(ed_dist) <- c("Duration","Frequency")
  
  if(!plot_line) {
    pp <- ggplot(data = ed_dist, aes(x = Duration, y = Frequency))
    pp + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  } else {
    pp <- ggplot(data = ed_dist, aes(x = factor(Duration), y = Frequency, group = 1))
    pp + geom_line() + geom_point() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  }
  

}