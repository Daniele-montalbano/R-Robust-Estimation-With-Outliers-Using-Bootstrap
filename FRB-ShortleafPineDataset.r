## In the "Shortleaf Pine" dataset from Atkinson e Riani (Robust Diagnostic Regression Analysis, 2000, appendix A.10),
## the response variable (y) is the volume (expressed in cubic foot) of 70 Pinus Echinata trees, and the two independent variables
## are the trunk circumference (x1), measured in inches, and the height of each tree (x2), in feet.
## The aim of the analysis is "to find a formula to predict the volume (of usable wood) from the other two measures".
rm(list=ls())

##uploading the "Shortleaf Pine" dataset
y<-c(2.2,2.0,3.0,4.3,3.0,2.9,3.5,3.4,5.0,7.2,6.4,5.6,7.7,10.3,8.0,12.1,11.1,16.8,13.6,16.6,20.2,17.0,17.7,19.4,17.1,23.9,22.0,23.1,22.6,22.0,27.0,27.0,27.4,25.2,25.5
,25.8,32.8,35.4,26.0,29.0,30.2,28.2,32.4,41.3,45.2,31.5,37.8,31.6,43.1,36.5,43.3,41.3,58.9,65.6,59.3,41.4,61.5,66.7,68.2,73.2,65.9,55.5,73.6,65.9,71.4,80.2,93.8,97.9,107.0,163.5)
length(y)

x1<-c(4.6,4.4,5.0,5.1,5.1,5.2,5.2,5.5,5.5,5.6,5.9,5.9,7.5,7.6,7.6,7.8,8.0,8.1,8.4,8.6,8.9,9.1,9.2,9.3,9.3,9.8,9.9,9.9,9.9,10.1,10.2,10.2,10.3,10.4,10.6
,11.0,11.1,11.2,11.5,11.7,12.0,12.2,12.2,12.5,12.9,13.0,13.1,13.1,13.4,13.8,13.8,14.3,14.3,14.6,14.8,14.9,15.1,15.2,15.2,15.3,15.4,15.7,15.9,16.0,16.8,17.8,18.3,18.3,19.4,23.4)
length(x1)

x2<-c(33,38,40,49,37,41,41,39,50,69,58,50,45,51,49,59,56,86,59,78,93,65,67,76,64,71,72,79,69,71,80,82,81,75,75
,71,81,91,66,65,72,66,72,90,88,63,69,65,73,69,77,64,77,91,90,68,96,91,97,95,89,73,99,90,90,91,96,100,94,104)
length(x2)

##################as matrix################
##data<-matrix(c(x1,x2,y),ncol=3,byrow=F)##
##dimnames(data)[[2]]<-c("x1","x2","y")  ##
##data                                   ##
###########################################

data<-data.frame(y,x1,x2)
data
pairs(data,pch=19)

plot(x1,y,pch=19,main="x1  vs  y")
plot(x2,y,pch=19,main="x2  vs  y")

model1<-lm(y~x1+x2, data=data)
summary(model1)

##quantile-quantile plot
r.stand<-rstandard(model1)
qqnorm(r.stand,pch=20,xlim=c(-3,3),ylim=c(-5,5))
abline(a=0,b=1,lty=2,lwd=2,col=2)

##residuals vs estimated values plot (for the conditional expected value)
fit.val<-fitted(model1)
plot(fit.val,r.stand,pch=20, ylim=c(-4,5), main="residuals vs expected values")
abline(h=0,lty=2,lwd=1,col=3)
abline(h=2,lty=2,lwd=2,col=4)
abline(h=-2,lty=2,lwd=2,col=4)
lines(lowess(r.stand~fit.val),col=2,lwd=2,lty=3)
legend(-10,4.7,legend=c("moving average of residuals"),col=2,lwd=2,lty=3)

##residuals vs regressors plots 
plot(x1,r.stand,pch=20, ylim=c(-4,5), main="residuals  vs  x1")
abline(h=0,lty=2,lwd=1,col=3)
abline(h=2,lty=2,lwd=2,col=4)
abline(h=-2,lty=2,lwd=2,col=4)
lines(lowess(r.stand~x1),col=2,lwd=2,lty=3)
legend(5,4.7,legend=c("moving average of residuals"),col=2,lwd=2,lty=3)

plot(x2,r.stand,pch=20,ylim=c(-4,5), main="residuals  vs  x2")
abline(h=0,lty=2,lwd=1,col=3)
abline(h=2,lty=2,lwd=2,col=4)
abline(h=-2,lty=2,lwd=2,col=4)
lines(lowess(r.stand~x2),col=2,lwd=2,lty=3)
legend(35,4.7,legend=c("moving average of residuals"),col=2,lwd=2,lty=3)

##partial regression plots
x1.x2<-resid(lm(x1~x2))
x2.x1<-resid(lm(x2~x1))
y.x2<-resid(lm(y~x2))
y.x1<-resid(lm(y~x1))

plot(x1.x2,y.x2,pch=20, main="partial regression between y and x1")
lines(lowess(y.x2~x1.x2),col=2,lwd=2,lty=3)
abline(lm(y.x2~x1.x2),col=3,lwd=2,lty=2)
legend(-6,80,legend=c("moving average of residuals","regression line"),col=c(2,3),lwd=2,lty=c(3,2))

plot(x2.x1,y.x1,pch=20, main="partial regression between and x2")
lines(lowess(y.x1~x2.x1),col=2,lwd=2,lty=3)
abline(lm(y.x1~x2.x1),col=3,lwd=2,lty=2)
legend(-1,42,legend=c("moving average of residuals","regression line"),col=c(2,3),lwd=2,lty=c(3,2))



######################
##FRB implementation##
######################

library(FRB)

set.seed(320) ##set a seed for the reproducibility of the results
#S- estimates
resS<-FRBmultiregS(y~.,data=data, R=1000)
summary(resS, confmethod="both",print.CI=T)

####as matrix
##resS1<-FRBmultiregS(X=cbind(x1,x2),Y=y, R=1000)
##summary(resS1, confmethod="both",print.CI=T)

resS$bootest
plot(resS, expl=2:3, confmethod = "BCA")
plot(resS, expl=2:3, confmethod = "basic")

diagplot(resS, Xdist=F)
diagplot(resS)


#MM- estimates
resMM<-FRBmultiregMM(y~.,data=data, R=1000)
summary(resMM, confmethod="both",print.CI=T)

####as matrix
##resM1<-FRBmultiregMM(X=cbind(x1,x2),Y=y, R=1000)
##summary(resM1, confmethod="both",print.CI=T)

resMM$bootest
plot(resMM, expl=2:3, confmethod = "BCA")
plot(resMM, expl=2:3, confmethod = "basic")

diagplot(resMM, Xdist=F)
diagplot(resMM)
