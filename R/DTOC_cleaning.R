clean_dtoc_data <- function(df, sv = TRUE, rt = FALSE, disp_propmiss = TRUE) {
  
  df[,"Date"] <- as.Date(df[,"Date"], "%d/%m/%Y")
  
  if(disp_propmiss) {
    print(propmiss(df))
  }
  
  if(sv) {
    dtoc_london_clean <- df
    save(dtoc_london_clean, file = "data/DTOC_London_clean.rda")
  }
  
  if(rt) {
    df
  } else {TRUE}
}

melt_dtoc_data <- function(df) {
  
  df_melt <- melt(df, id = c("Date"))
  colnames(df_melt) <- c("Date","Trust","DTOC")
  df_melt$Trust <- substr(df_melt$Trust,1,20)
  
  df_melt
  
}

plot_dtoc_line <- function(df, trusts = c("CHELSEA.AND.WESTMINS")) {
  
    dfm <- melt_dtoc_data(df)
    
    ggplot2::ggplot(data=dfm[dfm$Trust %in% trusts,],
           ggplot2::aes(x=Date, y=DTOC, group=Trust)) +
      ggplot2::geom_line(ggplot2::aes(color=Trust)) +
      ggplot2::geom_point(ggplot2::aes(color=Trust))
  
}
