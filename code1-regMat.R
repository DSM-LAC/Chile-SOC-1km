
#paso 1, librerias requeridas

library(rgdal )
library(raster)
library(GSIF)
library(caret)
library(doMC)
library(doParallel)
library(reshape)
library(snowfall)

#directorio de trabajo

setwd("~/Chile/covariables1km/")

#importacion de datos a R

#covariables
#(covs <- stack(readRDS('ChileCovariates5kmWG-topo.rds')))

 wg <- stack("CHL_worldgridsCOVS.tif")
 names(wg) <- readRDS("worldgridsCOVS_names.rds")
 topo <- stack("CHLtopo.tif")
 names(topo) <- readRDS("namesTOPO.rds")
 covs <- stack(wg, topo)

#datos
d <- read.csv('Chilean_SOC_dataset_26jul_final.csv')
str(d)

#objeto espacial
dsp <- d
coordinates(dsp) <- ~ X+Y
proj4string(dsp) <- CRS(projection (covs))

#visualizacion de datos
plot(dsp)
map('world', add=TRUE)

#descarga los limites geopoliticos

cou <- getData("GADM", country='CHILE', level=1)
cou <- spTransform(cou, CRS=projection (covs))

#covs[covs==0] <- 0.001


#matriz de regression
idx <- seq(1:dim(covs)[3])
covs <- setZ(covs, idx)
s.list <- unstack(covs)
names(s.list) <- names(covs)
# Now, create a R cluster using all the machine cores minus one
sfInit(parallel=TRUE, cpus=parallel:::detectCores()-3)
# Load the required packages inside the cluster
sfLibrary(raster)
sfLibrary(sp)
# Run parallelized 'extract' function and stop cluster
e <- sfSapply(s.list, extract, y=dsp)
sfStop()

#matriz <- extract(x = COV, y = dsp, sp = TRUE)

#combina las based de datos 
matriz <- cbind(d, e)

#guarda resultados
write.csv(matriz, file='Chile-regMat-1km-wg-topo_FINAL.csv')

#END

