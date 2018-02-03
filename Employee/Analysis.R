load("ImputationResults.RData")

require(ggplot2)

theta.est<- apply(coeff.all[,1:6], 2, mean)
theta.est

#Variance of the standard errors
Vw<- apply(coeff.all[,7:12]^2, 2, mean)
Vw

#variance of the estimators
Vb<- sapply(1:6, FUN=function(i){
  1/(M-1)*sum((coeff.all[,i]-theta.est[i])^2)
})
Vb
#combined variance
V<-Vw+Vb+Vb/M

#standard errors
SE<-sqrt(V)
SE
plot(mu2)
coeff.all


#Fraction of Missing information
FMI<-(Vb+Vb/M)/V
FMI

#degrees of freedom
df<- (M-1)*(1/FMI^2)

##Confidence interval on well being
theta.est[4] +c(-1,1)* qt(.975, df[4])*SE[4]

#Hypothesis test
pt(theta.est[4]/SE[4], df[4], lower.tail = F)*2
#Significant!

##Confidence interval on job satisfaction
theta.est[5] +c(-1,1)* qt(.975, df[5])*SE[5]

#hypothesis test
pt(theta.est[5]/SE[5], df[5])*2

pdf(file="Rsq.pdf")
ggplot(data.frame(RSquared=rsquares))+geom_histogram(aes(x=RSquared), bins=16)+xlab("R Squared Values")+
  theme(axis.text.x=element_text(size=18, angle=0, vjust=0.3), axis.title.x = element_text(size=22),
        axis.title.y=element_text(size=22),
        axis.text.y=element_text(size=16),
        plot.title=element_text(size=16))
dev.off()
mean(rsquares)
hist(sigmas^2)

examp<-data.frame(newdat)
names(examp)<-c("Age", "Tenure","WellBeing", "JobSat", "JobPerf", "IQ")
examp$JobSatMissing<-missing[,4]
examp$JobPerfMissing<-missing[,5]
examp$WellBeingMissing<-missing[,3]

pdf(file="impexamp.pdf")
ggplot(examp)+geom_point(aes(x=WellBeing, y=JobPerf, col=WellBeingMissing, shape=WellBeingMissing))+
  theme(axis.text.x=element_text(size=18, angle=0, vjust=0.3), axis.title.x = element_text(size=20),
        axis.title.y=element_text(size=20),
        axis.text.y=element_text(size=16),
        plot.title=element_text(size=16))
dev.off()
