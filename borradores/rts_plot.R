
rts_plot = function(TS, n_variable=1){
  
  require(ggplot2)
  
  serieDF = data.frame(TS)
  # serieDF = TS
  Fecha = seq.Date(from = as.Date(paste(c(start(TS),1),collapse = "/")),
                  by="month" ,length.out = dim(TS)[1] )  
  serNomb = names(serieDF)
  serieDF$Fecha = Fecha
  serNomb = paste0("`",serNomb,"`")
  p1 = ggplot(data = serieDF, 
         aes_string(x = "Fecha", 
                    y = serNomb[n_variable] )) +
    geom_line(size = 0.8) + 
    theme_minimal() +
    scale_x_date(                                        
      breaks = "2 months",
      date_labels = "%b %Y"
    )+
    # labs(
    #   title = "Caudal del clúster 1",
    #   subtitle = "Media Funcional",
    #   y = ylabel
    # )+
    theme(
      axis.text.x=element_text(size = 6 , angle=90),
      axis.text.y=element_text(size = 6 )
    )
  plot(p1)
  return(p1)
}
