#FDE   
DistribEmpirica = function(Lista_Xi,x=Lista_Xi){
  Lista_Xi = Lista_Xi[!is.na(Lista_Xi)]
  Distrib = c()
  
  for(i in 1:length(x)){
    Indicatriz = as.numeric(Lista_Xi <= x[i])
    Distrib[i] = mean(Indicatriz, na.rm = TRUE)
  }
  
  return(Distrib)
}