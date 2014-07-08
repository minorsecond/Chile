library(spdep)

#Open the Chile data 
chile <- read.csv("ChileData.csv")

#Open the GeoDa weights file
chilegal <- read.gal("chile_nbs.gal", override.id=TRUE)

#attach the Chile data
attach(chile)

#convert the weights file to a neighbor file
chilegwt <- read.gwt2nb("chile_gwt.gwt", region.id=ID)

#check the files for symmetry
print(is.symmetric.nb(chilegal))
print(is.symmetric.nb(chilegwt))

#Convert the neighbors file to a listw
chile_wt <- nb2listw(chilegal, zero.policy=TRUE)


#Set seed for tests and experiments
set.seed(5)

#Check for spatial autocorrelation using Moran's I
moran.test(MeanRT, chile_wt, randomisation=FALSE, zero.policy=TRUE, alternative="two.sided", rank=FALSE, na.action=na.pass, spChk=FALSE)

#Monte Carlo simulatin of Moran's I
mor_MeanRT <-moran.mc(MeanRT, chile_wt, 99999, zero.policy=TRUE, alternative="greater",na.action=na.fail, spChk=FALSE)

#Plot the refernece distribution
morp <- mor_MeanRT$res[1:length(mor_MeanRT$res) -1]
zz <- density(morp)
plot.density(zz,main="Moran's I Permutation Test", xlab="Reference Distribution", xlim=c(-.03,1), ylim=c(0,40),lwd=2,col=2)
hist(morp,freq=F,add=T)
abline(v=mor_MeanRT$statistic,lwd=2,col=4)

#Create spatially-lagged variable
xx <- cbind(MeanRT, JAN_EVI_ME, OCT_EVI_ME, JAN_LST_ME, OCT_LST_ME, JAN_NDVI_M, OCT_NDVI_M, Dep_RT, Casa_RT, Piez_RT, Med_RT, Ranch_RT, Ruca_RT, Movil_RT, col_rt)
wxx <- lag.listw(chile_wt, xx, zero.policy=TRUE, NAOK=TRUE)

#Create Moran scatter plot
moran.plot(MeanRT, chile_wt)
x <- MeanRT
zx <- (x-mean(x))/sd(x)
mean(xx)
var(xx)
wfile <- chile_wt
wzx <- lag.listw(wfile,zx)
morlm <- lm(wzx ~ zx)
aa <- morlm$coefficients[1]
mori <- morlm$coefficients[2]
aa
mori
par(pty="s")
plot(zx,wzx,xlab="Rate",ylab="Spatial Lag of Rate")
abline(aa,mori,col=2)
abline(h=0,lty=2,col=4)
abline(v=0,lty=2,col=4)
title(paste("Moran's Scatterplot I=",format(round(mori,4))))

#Compute a random ZINB distribution against which to test hypotheses.
#this distribution approximates that of Chile's Chagas disease rates
randNB <- rnegbin(340, mu=20 ,theta = .8)

#Try OLS to model cdisease
	#Null Model
	null.ols <- lm(MeanRT ~1)
	OLS1 <- lm(MeanRT ~ JAN_EVI_ME + OCT_EVI_ME + JAN_LST_ME +OCT_LST_ME + JAN_NDVI_M + OCT_NDVI_M + Dep_RT + Casa_RT + Piez_RT + Med_RT + Ranch_RT + Ruca_RT + Movil_RT + col_rt)

	#Check against null
	anova(null.ols, OLS1)
