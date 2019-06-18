# ===============================================
rts_clean=function(TS, seasonal=T, s=12, tr=36 ){
  require(stlplus)
  require(mFilter)
  n=dim(TS)[2]
  # k=1
  lambda=1
  if(n<2){
    warning('Error:not enough time series for reconstruction')
  } else {
  TSclean = matrix(NA,nrow = dim(TS)[1],ncol = n)
  for(k in 1:n){
    TSk=TS[,k]
    
    if(sum(is.na(TS))>0){
      
      if(seasonal){
        descom = stlplus(TSk,s.window = s,t.window =tr)
      } else{
        # descom = hpfilter(TSk,lambda)  
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
}  
  return(TSclean)
}

