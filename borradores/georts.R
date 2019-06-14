# Geo-reconstruction of Time Series ----------------
geoRts = function(TS,positions.TS,weights.TS = NULL,positions.RTS,weights.RTS=NULL,D = NULL,plots = TRUE){
  require(gridExtra)
  n = dim(TS)[2]
  # Distances Matrix
  positions = rbind(positions.TS,positions.RTS)
  weights = c(weights.TS,weights.RTS)
  
  # Gravity Flow Matrix
  N = dim(positions)[1]
  G = matrix(NA, nrow = N,ncol = n)
  A = G
  for (i in seq(N)) {
    for(j in seq(n)){
      d = sum((positions[i,]-positions.TS[j,])^2)
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
  colnames(RTS) = paste0("Serie",(N-n):N)
  
  # RTS Plot
  if(plots){
    pl = list()
    for(k in seq(N-n)){
      pl[[k]] = rts_plot(RTS,n_variable = k)
    }
    gridExtra::grid.arrange(grobs=pl,nrow=N-n)
    }
  
  return(RTS)
  
}


# Example ------------------------
set.seed(1)
TS = ts(data=data.frame(x1=rnorm(40),
                        x2=rnorm(40,10,2),
                        x3=rnorm(40,30,3)),
        start = c(2000,1),frequency = 12)
positions.TS = data.frame(lon = rnorm(3,0,10),lat=rnorm(3,30,10))
weights.TS = round(runif(3,1,10))


positions.RTS = data.frame(lon = rnorm(4,0,10),lat=rnorm(4,30,10))
weights.RTS = round(runif(4,1,10))


y = geoRts(TS,positions.TS,weights.TS,positions.RTS,weights.RTS)












