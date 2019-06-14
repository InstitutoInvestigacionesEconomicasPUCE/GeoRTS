{
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
  #Simulacion
  rEmpirica = function(Lista_Xi,n=1){
    Lista_Xi = Lista_Xi[!is.na(Lista_Xi)]
    if(n>0){
      u =runif(n)
      xsim=c()
      for(k in 1:n){
        xsim[k] = min(Lista_Xi[u[k]<=DistribEmpirica(Lista_Xi)],na.rm = TRUE)
      }
      
    }else{
      xsim = NA
      print("Ingrese un n adecuado")
    }
    
    return(xsim)
  }
  
}

# ===============================================

rts_clean=function(TS, seasonal=T, s=12, tr=36 ){
  require(stlplus)
  n=dim(TS)[2]
  # k=1
  lambda=1
  TSclean = matrix(NA,nrow = dim(TS)[1],ncol = n)
  for(k in 1:n){
    TSk=TS[,k]
    
    if(sum(is.na(TS))>0){
      
      if(seasonal){
        descom= stlplus(TSk,s.window = s,t.window =tr)
      } else{
        descom=hpfilter(TSk,lambda)  
      }
      
      # X=descom$data$raw
      Tren=descom$data$trend
      Seas=descom$data$seasonal
      Res=descom$data$remainder
      
      Res[is.na(Res)]=rEmpirica(Res,n=sum(is.na(Res)))
      TScleank=Tren+Seas+Res
      
    } else {
      
      TScleank=TSk
    }
    TSclean[,k] = TScleank
  }#end for
  TSclean=data.frame(TSclean)
  names(TSclean) = colnames(TS)
  TSclean = ts(TSclean,start = start(TS),frequency = frequency(TS))
  return(TSclean)
}#end function



# Example ------------------------
set.seed(1)
TS = ts(data=data.frame(x1=rnorm(40),
                        x2=rnorm(40,10,2),
                        x3=rnorm(40,30,3)),
        start = c(2000,1),frequency = 12)
TS[5:10,]=NA #sin datos
positions.TS = data.frame(lon = rnorm(3,0,10),lat=rnorm(3,30,10))
weights.TS = round(runif(3,1,10))


positions.RTS = data.frame(lon = rnorm(4,0,10),lat=rnorm(4,30,10))
weights.RTS = round(runif(4,1,10))


RTS = geoRts(TS,positions.TS,weights.TS,positions.RTS,weights.RTS)

clTS = rts_clean(TS)

plot(TS)
plot(clTS)
