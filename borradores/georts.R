# Geo-reconstruction of Time Series ----------------
geoRts = function(TS,positions.TS,weights.TS = NULL,positions.RTS,weights.RTS=NULL,D = NULL){
  require(gridExtra)
  n = dim(TS)[2] 
  if(n<2){
    warning('Error:not enough time series for reconstruction')
  } else {
    
    # Distances Matrix
    positions = rbind(positions.TS,positions.RTS)
    weights = c(weights.TS,weights.RTS)
    
    # Gravity Flow Matrix
    N = dim(positions)[1]
    G = matrix(NA, nrow = N,ncol = n)
    A = G
    for (i in seq(N)) {
      for(j in seq(n)){    
        if(is.null(D)){
          d = sum((positions[i,]-positions.TS[j,])^2)
        }else{
          d = D[i,j]
        }
        if(d!=0){
          G[i,j] = (weights[i]*weights[j])/d
        }
      }
    }
    
    BoolG = is.na(G)
    for(j in 1:dim(G)[2]){
      G[BoolG[,j],-j] = 0
      G[BoolG[,j],j] = 1
    }
    
    # Standarized Alfa coefficients
    for(i in 1:dim(A)[1]){
      A[i,] = G[i,]/sum(G[i,])
    }
    
    As = A[(n+1):N,]
    
    # Reconstruction of Time Series
    RTS = TS%*%t(As)
    RTS = ts(RTS,start = start(TS),frequency = frequency(TS))
    colnames(RTS) = paste0("Serie",(n+1):N)
  }  
  return(RTS)
  
}












