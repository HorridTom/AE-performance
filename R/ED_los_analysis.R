make_bins <- function(df, los_col = 'Der_AEA_Duration', bin_col = 'los_bin', default_labels = TRUE) {
  
  max_los <- max(df[,los_col], na.rm=TRUE)
  bins <- seq(from = 0, to = 24*15, by= 15)
  bin_labels <- make_bin_labels(bins)
  bins <- append(bins, max_los)
  
  if(default_labels){
    df[,bin_col] <- cut(df[,los_col], breaks = bins)
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