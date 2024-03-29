# Example GeoRTS ------------------------

source("georts.R")
source("rts_clean.R")
source("rts_simu.R")
source("rts_distrib.R")
source("rts_plot.R")
source("rts_plotGroup.R")


set.seed(3)
# Tenemos m series conocidas
m = 3
p= 50
x = lapply(round(runif(m,3,20)), FUN = function(x){
  rnorm(p,mean = x,sd=1)
})
TS = as.data.frame(matrix(unlist(x) , nrow = p,ncol = m))
TS = ts(data=TS, start = c(2000,1),frequency = 12)
TS[15:20,] = NA
TS

positions.TS = data.frame(lon = rnorm(m,0,10),lat=rnorm(m,30,10))
weights.TS = round(runif(m,1,10))


# Reconstruir n series  ==================
n=5
positions.RTS = data.frame(lon = rnorm(n,0,10),lat=rnorm(n,30,10))
weights.RTS = round(runif(n,5,15))


RTS = geoRts(TS,positions.TS,weights.TS,positions.RTS,weights.RTS)

#Grafico de Series    ====================
rts_plotGroup(TS,RTS)

# 2. Limpieza  ===========================
TS_clean = rts_clean(TS)

pl = list()
for (i in seq(dim(TS)[2])) {
  pl[[i]] = rts_plotClean(TS,i)
}

grid.arrange(grobs=pl,nrow=dim(TS)[2])

