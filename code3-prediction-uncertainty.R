
#librerias

library(raster)
library(rgdal)
library(randomForest)
library(caret)

#datos y covariables

dat <- read.csv('trainingDATA-50covs-FINAL.csv')
wg <- stack("CHL_worldgridsCOVS.tif")
 names(wg) <- readRDS("worldgridsCOVS_names.rds")
 topo <- stack("CHLtopo.tif")
 names(topo) <- readRDS("namesTOPO.rds")
 covs <- stack(wg, topo)


covs <- covs[[names(dat[-c(1,2)])]] 

#formula e hipotesis de trabajo

fm = as.formula(paste("log(SOC) ~", paste0(names(covs[[-3]]),
                                            collapse = "+"))) 

fm

#modelos

cl <- makeCluster(detectCores(), type='SOCK')
registerDoParallel(cl)

ctrl <- trainControl(method = "repeatedcv", number=5, repeats=5, savePred=T)

rfmodel <- train(fm, data=dat, method = "rf", trControl = ctrl, 
             importance=TRUE)

rfmodel

varImpPlot(rfmodel[11][[1]])


plot(rfmodel[11][[1]])

pred <- predict(covs, rfmodel)

cou <- getData("GADM", country='CHILE', level=1)
cou <- spTransform(cou, CRS=projection (covs))

pred <- mask(pred, cou)

writeRaster(pred, file='randomForestPRED_Chile1km.tif')

library(raster)
        library(doMC)
        library(doParallel)
        library(caret)
        library(caretEnsemble)
        library(snowfall)
        library(quantregForest)

ctrl <- trainControl(savePred=T, method="repeatedcv", number=5, repeats=5)
(models <- caretList(dat[-c(1, 2)], dat[,2], trControl=ctrl ,
        methodList=c("glm","gam", "rf","kknn", "svmLinear")))

(ens <- caretEnsemble(models))

pred <-  predict(covs, ens)

writeRaster(pred, file='ensemblePRED_Chile1km.tif')
plot(pred, col=colorRampPalette(c("gray", "brown", "blue"))(255))
###
###incertidumbre del modelo
###

library(quantregForest)

model <- quantregForest(y=dat[,2], x=dat[-c(1, 2)], ntree=500, keep.inbag=TRUE)
beginCluster()
#Estimate model uncertainty
unc <- clusterR(covs, predict, args=list(model=model,what=sd))
unc <- trim(unc)
plot(unc)
writeRaster(unc, file='incertidumbreSOCmapQRF1km.tif',
overwrite=TRUE) 

writeRaster(unc/pred*100, file='incertidumbrePercentage_STANDARIZED_SOCmapQRF1km.tif',
overwrite=TRUE) 

endCluster()

#END
