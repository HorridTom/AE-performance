make_bins <- function(df, los_col = 'Der_AEA_Duration', bin_col = 'los_bin', default_labels = TRUE, top_bin_end = 48*15) {
  
  max_los <- max(max(df[,los_col], na.rm=TRUE), top_bin_end)
  bins <- seq(from = 0, to = top_bin_end, by= 15)
  bin_labels <- make_bin_labels(bins, max_los)
  bins <- append(bins, max_los + 1)
  
  if(default_labels){
    df[,bin_col] <- cut(df[,los_col], breaks = bins, right = FALSE)
  } else{
    df[,bin_col] <- cut(df[,los_col], breaks = bins, right = FALSE, labels = bin_labels)
  }
  df
}

make_bin_labels <- function(bins, maximum) {
  bins <- paste(bins%/%60,sprintf("%02d", bins%%60),sep=":")
  bin_chars <- as.character(bins)
  bin_chars_shift <- bin_chars[-1]
  bin_chars_shift <- append(bin_chars_shift,"")
  bin_labels <- paste(bin_chars,bin_chars_shift,sep="-")
  bin_labels[length(bin_labels)] <- paste("",paste(maximum%/%60,sprintf("%02d", maximum%%60),sep=":"),sep = "-")
  bin_labels
}

plot_ed_dist <- function(df, prov_codes = c("RBZ"), cumulative = TRUE) {
  prov_data <- df[df$Der_Provider_Code %in% prov_codes,]
  
  
  if (nrow(prov_data) == 0) {return(NULL)}
  
  
  #ed_dist <- aggregate(prov_data$Activity, by=list(Duration=prov_data$los_bin, Admitted=prov_data$admitted), FUN=sum)
  
  prov_data_adm <- prov_data[prov_data$admitted == TRUE,]
  prov_data_att <- prov_data[prov_data$admitted == FALSE,]
  
  #This is a hack to aggregate over all levels of the bin factor - need to refactor using dplyr
  #Also note, need to deal with the case where original data contains no times in given bin
  #Probably using original bins from the cut as basis.
  Y <- data.frame(los_bin = levels(prov_data$los_bin))
  Y$id <- c(1:nrow(Y))
  
  # This is a hack!!! to put column x of zeroes down the distribution.
  if(nrow(prov_data_adm)==0) {
    ed_dist_adm <- Y
    ed_dist_adm$x <- 0
  } else {
    ed_dist_adm <- merge(Y,aggregate(prov_data_adm$Activity, by=list(los_bin=prov_data_adm$los_bin), FUN=sum), all.x = T)
  }
  
  ed_dist_adm <- ed_dist_adm[order(ed_dist_adm$id),]
  ed_dist_att <- merge(Y,aggregate(prov_data_att$Activity, by=list(los_bin=prov_data_att$los_bin), FUN=sum), all.x = T)
  ed_dist_att <- ed_dist_att[order(ed_dist_att$id),]
  ed_dist_adm$Admitted <- rep(TRUE,nrow(ed_dist_adm))
  ed_dist_att$Admitted <- rep(FALSE,nrow(ed_dist_att))
  ed_dist <- rbind(ed_dist_att, ed_dist_adm)
  ed_dist$id <- NULL
  ed_dist$los_bin <- factor(ed_dist$los_bin, levels = levels(prov_data$los_bin))
  ed_dist$x = ifelse(is.na(ed_dist$x), 0, ed_dist$x)
  colnames(ed_dist)[which(names(ed_dist) == "x")] <- "Frequency"
  colnames(ed_dist)[which(names(ed_dist) == "los_bin")] <- "Duration"
  
  if (cumulative) {
    v <- cumsum(ed_dist[ed_dist$Admitted==FALSE,"Frequency"])
    v1 <- v/max(v)*100
    ed_dist[ed_dist$Admitted==FALSE,"Cumulative Percentage"] <- v1  
    u <- cumsum(ed_dist[ed_dist$Admitted==TRUE,"Frequency"])
    u1 <- u/max(u)*100
    ed_dist[ed_dist$Admitted==TRUE,"Cumulative Percentage"] <- u1
    w <- u + v
    w1 <- w/max(w)*100
    
  }
  if(cumulative) {
    ed_dist_m <- reshape2::melt(ed_dist, id = c("Duration", "Admitted"))
    cm_df <- ed_dist_m[ed_dist_m$Admitted=="FALSE" & ed_dist_m$variable=="Cumulative Percentage",]
    cm_df$value <- w1
    cm_df$Admitted <- rep("ALL", length(w1))
    ed_dist_m <- rbind(ed_dist_m, cm_df)
    
    intercepts_4h <- ed_dist_m[which(ed_dist_m$Duration=="3:45-4:00" & ed_dist_m$variable=="Cumulative Percentage"),which(colnames(ed_dist_m) %in% c("variable", "value", "Admitted"))]
    intercepts_12h <- ed_dist_m[which(ed_dist_m$Duration=="11:45-12:00" & ed_dist_m$variable=="Cumulative Percentage"),which(colnames(ed_dist_m) %in% c("variable", "value", "Admitted"))]
    tt95 <- ed_dist_m[which(ed_dist_m$variable=="Cumulative Percentage" & ed_dist_m$value>=95),]
    time_to_95 <- tt95[tt95$value == ave(tt95$value, tt95$Admitted, FUN=min),]
    times95 <- paste(sub(".*-","",time_to_95$Duration), collapse = '     ')
    
    ann_text1 <- data.frame(Duration = "10:15-10:30", value = 40,
                           variable = factor("Cumulative Percentage",levels=c("Frequency", "Cumulative Percentage")))
    ann_text2 <- data.frame(Duration = "8:00-8:15", value = 32,
                            variable = factor("Cumulative Percentage",levels=c("Frequency", "Cumulative Percentage")))
    ann_text3 <- data.frame(Duration = "8:00-8:15", value = 22,
                            variable = factor("Cumulative Percentage",levels=c("Frequency", "Cumulative Percentage")))
    ann_text4 <- data.frame(Duration = "8:00-8:15", value = 12,
                            variable = factor("Cumulative Percentage",levels=c("Frequency", "Cumulative Percentage")))
    perfs_4h <- paste(round(intercepts_4h$value, 1), collapse='     ')
    ann_text5 <- data.frame(Duration = "10:30-10:45", value = 32,
                            variable = factor("Cumulative Percentage",levels=c("Frequency", "Cumulative Percentage")))
    perfs_12h <- paste(round(intercepts_12h$value, 1), collapse='     ')
    ann_text6 <- data.frame(Duration = "10:30-10:45", value = 22,
                            variable = factor("Cumulative Percentage",levels=c("Frequency", "Cumulative Percentage")))
    ann_text7 <- data.frame(Duration = "10:30-10:45", value = 12,
                            variable = factor("Cumulative Percentage",levels=c("Frequency", "Cumulative Percentage")))

  } else {
    colnames(ed_dist)[3] <- "value"
    ed_dist_m <- ed_dist
  }
  
  pp <- ggplot(data = ed_dist_m, aes(x = Duration, y = value, group = Admitted, colour = Admitted))
  pp <- pp + geom_path() + geom_point() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), axis.title.y = element_blank())
  
  if(cumulative) {
    pp <- pp + facet_grid(variable ~ ., scales = "free") +
    geom_text(aes(group = TRUE, colour = "ALL"), data = ann_text1, label="Not Adm Adm     All", size = 3) +
    geom_text(aes(group = TRUE, colour = "ALL"), data = ann_text2, label="     % in 4hrs:", size = 3) +
    geom_text(aes(group = TRUE, colour = "ALL"), data = ann_text3, label="   % in 12hrs:", size = 3) +
    geom_text(aes(group = TRUE, colour = "ALL"), data = ann_text4, label="Time to 95%:", size = 3) +
    geom_text(aes(group = TRUE, colour = "ALL"), data = ann_text5, label=perfs_4h, size = 3) +
    geom_text(aes(group = TRUE, colour = "ALL"), data = ann_text6, label=perfs_12h, size = 3) +
    geom_text(aes(group = TRUE, colour = "ALL"), data = ann_text7, label=times95, size = 3)
  }
  
  pp + geom_vline(xintercept = 16, colour="grey60") + geom_segment(aes(x=0, xend = 16, y = value, yend = value, colour=Admitted), intercepts_4h) + geom_text(aes(x=rep(0,3), y=value-6, label = sprintf("%.1f",value), vjust = rep(-1,3), hjust = rep(0.02,3)), intercepts_4h, size = 2.75, show.legend = FALSE) + ggtitle("Distribution of time in Emergency Department") + xlab("Time in ED") +
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_discrete(breaks = levels(ed_dist_m$Duration)[c(T, rep(F, 1))])
    
}

make_adm_colour_palette <- function() {
  adm_colours <- RColorBrewer::brewer.pal(8,"Dark2")
  names(adm_colours) <- c("TRUE",NA,NA,"FALSE",NA,NA,NA,"ALL")
  admColScale <- scale_colour_manual(name="Admission Status", values = adm_colours, labels = c("All","Not admitted","Admitted"))
  admColScale
}