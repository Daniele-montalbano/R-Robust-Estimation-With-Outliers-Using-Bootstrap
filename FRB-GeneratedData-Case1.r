##CASE 1
##regression coefficient that is equal to 1.5 and statistically significant which due to the effect of 3 outliers
##has been distorted (estimated coefficient=0.7) while remaining statistically significant

rm(list=ls())

x<-seq(-10,10,0.2)
length(x)
beta=1.5
set.seed(320) ##set a seed for the reproducibility of the results
errors<-rnorm(length(x),0,3)
y=beta*x+errors
plot(x,y, pch=19)
out1<-lm(y~x)
summary(out1)

plot(x,y, pch=19,,main="original linear regression model")
lines(x,out1$fitted, col=2, lwd=2)


##addition of 3 outliers in the sample
set.seed(320)
x2<-c(x,runif(3,20,25))
y2<-c(y,runif(3,-30,-25))
plot(x2,y2,pch=19, main="linear regression model with outliers")
out2<-lm(y2~x2)
summary(out2)


##comparison between the regression line estimated in the original sample
##and the estimated one in the sample distorted by the 3 outliers
plot(x2[1:length(x)],y2[1:length(x)], main="linear regression model",pch=19, xlab="x", ylab="y", xlim=c(-10,25),ylim=c(-30,21))
points(x2[102:104],y2[102:104], col=4, pch=19)
lines(x,out1$fitted, col=2, lwd=2)
lines(x2,out2$fitted, col=4, lwd=2)
legend(4,-11,legend=c("original regression","regression with outliers"),
lwd=2,lty=c(1,1), col=c(2,4,1,4))

par(mfrow=c(2,2))
plot(lm(y~x))

par(mfrow=c(2,2))
plot(lm(y2~x2))

par(mfrow=c(2,3))
plot(lm(y~x),which= c(1:3),main="original regression")
plot(lm(y2~x2),which= c(1:3),main="regression with outliers")



##Bootstrap 1: regenerate 104 values of y with estimated beta regenerating the errors using a normal distribution
library(boot)
summary(out2)
beta_st<-as.numeric(out2$coefficients[2])
round(beta_st,3)

glm.diag.plots(glm(y2~x2),ret=T)

boot.err<-function(y2,x2,beta_st,B=500,seed=320){
	n<-length(y2)
	errors.b<-matrix(0,B,n)
	y.b<-matrix(0,B,n)
	intercept<-matrix(0,B)
	beta<-matrix(0,B)
	set.seed(seed)
# iterative cycle for the creation of the B bootstrap replicas
	for (i in 1:B) {	
		errors.b[i,]<-rnorm(length(y2),0,3)     
		y.b[i,]<-beta_st*x2+errors.b[i,]
		intercept[i,]<-coef(lm(y.b[i,]~x2))[1]
		beta[i,]<-coef(lm(y.b[i,]~x2))[2]
	}
mean.intercept<-mean(intercept)	##mean intercept estimates
mean.beta<-mean(beta)	##mean beta estimates
bias<-mean.beta-beta_st	##beta distortion
variance<-var(beta) 
standard.error=sqrt(variance)
MSE<-variance+bias^2
	normal<-beta_st-bias-c(1,-1)*1.96*standard.error
	percentile<-quantile(beta,c(0.025,0.975))
		list(y.b=y.b,errors.b=errors.b,beta.b=beta,mean.intercept=mean.intercept,estimate=beta_st,mean.beta=mean.beta,bias=bias,variance=variance,MSE=MSE,standard.error=standard.error,
		interval.normal=normal, interval.percentile=percentile)
}

boot1<-boot.err(y2,x2,beta_st, B=1000)
hist(boot1$beta,20)
abline(v=beta_st, col=2, lwd=2)

round(boot1$mean.intercept,3)
round(boot1$mean.beta,3)
round(boot1$bias,4)
round(boot1$variance,4)
round(boot1$MSE,5)
round(boot1$standard.error,4)
round(boot1$interval.normal,4)
round(boot1$interval.percentile,4)



##Bootstrap 2: sample bootstrap regenerating positions

boot.camp<-function(y2,x2,beta_st,B=500,seed=320){
	n<-length(y2)
	y.b<-matrix(0,B,n)
	x.b<-matrix(0,B,n)
	intercept<-matrix(0,B)
	beta<-matrix(0,B)
	set.seed(seed)
# iterative cycle for the creation of the B bootstrap replicas
	for (i in 1:B) {
		ind<-sample(1:104,replace=TRUE)
	 	x.b[i,]<-x2[ind]
		y.b[i,]<-y2[ind]
		intercept[i,]<-coef(lm(y.b[i,]~x.b[i,]))[1]
		beta[i,]<-coef(lm(y.b[i,]~x.b[i,]))[2]
	}
mean.intercept<-mean(intercept)	##mean intercept estimates
mean.beta<-mean(beta)	##mean beta estimates
bias<-mean.beta-beta_st	##beta distortion
variance<-var(beta) 
standard.error=sqrt(variance)
MSE<-variance+bias^2
	normal<-beta_st-bias-c(1,-1)*1.96*standard.error
	percentile<-quantile(beta,c(0.025,0.975))
		list(y.b=y.b,x.b=x.b,beta.b=beta,mean.intercept=mean.intercept,estimate=beta_st,mean.beta=mean.beta,bias=bias,variance=variance,MSE=MSE,standard.error=standard.error,
		interval.normal=normal, interval.percentile=percentile)
}

boot2<-boot.camp(y2,x2,beta_st, B=1000)
hist(boot2$beta,20)
abline(v=beta_st, col=2, lwd=2)

round(boot2$mean.intercept,3)
round(boot2$mean.beta,3)
round(boot2$bias,4)
round(boot2$variance,4)
round(boot2$MSE,5)
round(boot2$standard.error,4)
round(boot2$interval.normal,4)
round(boot2$interval.percentile,4)



##comparison of the two bootstrap distributions
par(mfrow=c(1,2))
hist(boot1$beta,20)
abline(v=beta_st, col=2, lwd=2)
hist(boot2$beta,20)
abline(v=beta_st, col=2, lwd=2)



##comparison between the regression line estimated in the original sample
##the estimated one in the sample distorted by the 3 outliers,
##estimated using the bootstrap errors and using sample bootstrap
par(mfrow=c(1,1))
plot(x2[1:length(x)],y2[1:length(x)], main="linear regression model",pch=19, xlab="x", ylab="y", xlim=c(-10,25),ylim=c(-30,21))
points(x2[102:104],y2[102:104], col=4, pch=19)
lines(x,out1$fitted, col=2, lwd=2)
lines(x2,out2$fitted, col=4, lwd=2)
abline(boot1$mean.intercept,boot1$mean.beta, col=3, lwd=2)
abline(boot2$mean.intercept,boot2$mean.beta, col=6, lwd=2)
legend(-1,-10,legend=c("original regression","regression with outliers","bootstrap errors estimate","bootstrap resampling estimate"),
lwd=2,lty=c(1,1), col=c(2,4,3,6))



############################
##FAST and ROBUST BOOTSTRAP
library(FRB)

set.seed(320)
#S- estimates
resS<-FRBmultiregS(X=x2,Y=y2, R=1000)
print(resS)
summary(resS, confmethod="both",print.CI=T)
resS$bootest
plot(resS, expl=2, confmethod = "BCA")
plot(resS, expl=2, confmethod = "basic")

diagplot(resS, Xdist=F)
diagplot(resS)

#MM- estimates
resMM<-FRBmultiregMM(X=x2,Y=y2, R=1000)
print(resMM)
summary(resMM, confmethod="both",print.CI=T)
resMM$bootest
plot(resMM, expl=2, confmethod = "BCA")
plot(resMM, expl=2, confmethod = "basic")

diagplot(resMM, Xdist=F)
diagplot(resMM)

summary.estimates<-matrix(c(out1$coefficients,out2$coefficients,resS$coefficients,resMM$coefficients), ncol=2, byrow=T, dimnames=list(c("lm","lm_outliers","FRB_S-","FRB_MM-"),labels(out1$coefficients)))
summary.estimates

summary.estimates<-matrix(c(out1$coefficients,round(summary(out1)$coefficients[2,4],4),
out2$coefficients,round(summary(out2)$coefficients[2,4],4),
resS$coefficients,resS$p.bca[2],
resMM$coefficients,resMM$p.bca[2]), ncol=3, byrow=T, dimnames=list(c("lm","lm_outliers","FRB_S-","FRB_MM-"),c(labels(out1$coefficients),"p-value")))
summary.estimates

-----
summary.estimates<-matrix(c(round(out1$coefficients,4),round(summary(out1)$coefficients[2,4],3),1,
round(out2$coefficients,4),round(summary(out2)$coefficients[2,4],3),1,
round(resS$coefficients,4),round(resS$p.bca[2],3),round(resS$p.basic[2],3),
round(resMM$coefficients,4),round(resMM$p.bca[2],3),round(resMM$p.basic[2],3)),ncol=4, byrow=T, dimnames=list(c("lm","lm_outliers","FRB_S-","FRB_MM-"),c(labels(out1$coefficients),"p-value_1","p-value_2")))
summary.estimates
------

c(round(resS$CI.bca.lower[2],3),round(resS$CI.bca.upper[2],3))
c(round(resS$CI.basic.lower[2],3),round(resS$CI.basic.upper[2],3))
c(round(resMM$CI.bca.lower[2],3),round(resMM$CI.bca.upper[2],3))
c(round(resMM$CI.basic.lower[2],3),round(resMM$CI.basic.upper[2],3))

plot(x,y, pch=19,,main="estimated regression models")
lines(x,out1$fitted, col=2, lwd=2)
lines(x2,resS$fitted.values, col=3, lwd=2)
lines(x2,resMM$fitted.values, col=6, lwd=2)
legend(1.5,-13,legend=c("classic estimate","FRB S- estimate","FRB MM- estimate"),
lwd=2,lty=c(1,1), col=c(2,3,6))

plot(x2[1:length(x)],y2[1:length(x)], main="estimated regression models",pch=19, xlab="x", ylab="y", xlim=c(-10,25),ylim=c(-30,21))
points(x2[102:104],y2[102:104], col=4, pch=19)
lines(x,out1$fitted, col=2, lwd=2)
lines(x2,out2$fitted, col=4, lwd=2)
lines(x2,resS$fitted.values, col=3, lwd=2)
lines(x2,resMM$fitted.values, col=6, lwd=2)
legend(-1,-17,legend=c("classic estimate","distorted estimate","FRB S- estimate","FRB MM- estimate"),
lwd=2,lty=c(1,1), col=c(2,4,3,6))
