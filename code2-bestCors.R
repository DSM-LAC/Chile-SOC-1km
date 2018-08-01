#import datasets from code 1 and load required libraries

library(reshape)

d <- read.csv('Chile-regMat-1km-wg-topo_FINAL.csv')

#remove non informative variables

d$ln2dms3a <- NULL          
d$lnmdms3a <- NULL

#remove cero values

d$OCSKGM[d$OCSKGM==0] <- NA

#remove no data

d <- na.omit(d)
dim(d)

#plot statistical distribution

par(mfrow=c(1,2))
hist(d$OCSKGM, col='gray')
hist(log(d$OCSKGM), col='gray')

#transform to logarithm
#d$OCSKGM <- log(d$OCSKGM)

#remove categorical predictors
names(d)
d <- d[c(8, 13:141)]
names(d)

#find the best correlated variables
COR <- cor(as.matrix(d[,1]), as.matrix(d[-1]))
x <- subset(melt(COR), value != 1 | value != NA)
x <- x[with(x, order(-abs(x$value))),]
idx <- as.character(x$X2[1:50])###
x[1:50,]
#select them and generate a reduced dataset
sel <- c('px1wcl3a', 'tdhmod3a','lasmod3a', 'evmmod3a', 'glvhws3a', 'ganhws3a', 'etmnts3a', 'g13esa3a' )
training <- data.frame(SOC = d$OCSKGM, d[idx])
training1 <- data.frame(SOC = d$OCSKGM, d[sel])
#save new results
write.csv(training1, file= 'trainingDATA-50covs-FINAL.csv')
###

#END

