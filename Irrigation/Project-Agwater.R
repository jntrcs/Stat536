####################################################
####### Case Study for Gene Agriculture Data #######
####################################################

# Read in the data and fit the model

agwater <- read.table(file="https://mheaton.byu.edu/Courses/Stat536/Case%20Studies/CWSI-NonLin/Data/AgricultureWater.txt",header=T)
agwater<-agwater[order(agwater$cwsi),]
fit <- lm(swc~cwsi, data=agwater)

#########################################
####### Exploratory Data analysis #######
#########################################

## Linearity ##

# Create scatter plot to evaluate the linearity of the data
pdf("splot.pdf")
plot(agwater)
dev.off()

## Normality ##

fit.stres <- stdres(fit)

pdf("stdreshist.pdf")
hist(fit.stres,xlab="Standardized Residuals",main="")
dev.off()

## Equal Variance ##

fit.res <- resid(fit)
fit.vals <- fitted.values(fit)

pdf("fitvalsres.pdf")
par(mfrow=c(1,2))
hist(fit.stres,xlab="Standardized Residuals",main="")
plot(fit.vals, fit.res, ylab = "Residuals", xlab="Fitted Values", main="")
abline(0,0)
dev.off()

### Collinearity ###
plot(agwater)
vif(fit)

### Outliers ###
outlierTest(fit)

####################################
####### Non-linear Fit - WLS #######
####################################



fitloc <- loess(swc~cwsi,data=agwater,family="gaussian")
summary(fitloc)

predict(fitloc,data.frame(cwsi=seq(0,1,length=100)), se=TRUE)


#pdf("compare.pdf")
par(mfrow=c(1,3))
plot(x=agwater$cwsi,y=agwater$swc)
abline(fit, col="blue")
scatter.smooth(x=agwater$cwsi,y=agwater$swc,degree=1,family="gaussian",span=.35)
scatter.smooth(x=agwater$cwsi,y=agwater$swc,degree=2,family="gaussian",span=.35)
#dev.off()

# Use degree=2

# cross validation to find the span and degree

rand<-agwater[sample(1:nrow(agwater), replace=F),]
breaks<-c(0,20,40,60,78)
mses<-numeric(0)
for (i in 1:4){
  mses<-numeric(0)
  test<-rand[breaks[i]:breaks[i+1],]
  train<-rand[-(breaks[i]:breaks[i+1]),]
  for (band in seq(.2, .5, by=.05)){
    mod<-loess(swc~cwsi, data=train, family="gaussian",control=loess.control(surface="direct"), span = band)
    preds<-predict(mod, newdata = test)  
    #predict(mod, newdata = test, se = T)  
    mse<-sum((preds-test$swc)^2)
    mses<-c(mses, mse)
  }
  print(mses)
  plot(mses~seq(.2, .5, by=.05))
}

##anything over .5 might as well just be polynomial regression
#Anything below .25 causes warnings and errors about having not enough data (near singular)
best<-numeric(1000)
mses<-numeric(0)
for (i in 1:1000){
  indices<-sample(1:nrow(agwater), replace=F)
  test<-agwater[indices[56:78],]
  train<-agwater[indices[1:55],]
  mses<-numeric(0)
  for (band in seq(.25, .5, by=.05)){
    mod<-loess(swc~cwsi, data=train, family="gaussian",control=loess.control(surface="direct"), span = band)
    preds<-predict(mod, newdata = test)  
    #predict(mod, newdata = test, se = T)  
    mse<-sum((preds-test$swc)^2)
    mses<-c(mses, mse)
  }
  best[i]=seq(.25, .5, by=.05)[which.min(mses)]
}
hist(best)

require(ggplot2)
pdf(file="CVHist.pdf")
ggplot(data.frame(x=best))+geom_bar(aes(x=x))+ggtitle("Monte Carlo Cross Validation") +
  xlab("Span that achieved lowest MSE")
dev.off()

final.mod<-loess(swc~cwsi, data=agwater, family="gaussian", span=.35, degree=2)
summary(final.mod)
xs=seq(.01, 1, length=100)
preds<-predict(final.mod, newdata=data.frame(cwsi=xs), se=T)
preds.insample<-predict(final.mod, se=T)

plot(agwater$swc~agwater$cwsi)
lines(preds$fit~xs)
upper<-preds$fit+qt(.975, 25)*preds$se.fit
lines(upper~xs, lty=2, col="blue")
lower<-preds$fit-qt(.975, 25)*preds$se.fit
lines(lower~xs, lty=2, col="blue")

upper.pi<-preds$fit+qt(.975, 25)*sqrt(.2394^2+preds$se.fit^2)
lines(upper.pi~xs, lty=2, col="forestgreen")
lower.pi<-preds$fit-qt(.975, 25)*sqrt(.2394^2+preds$se.fit^2)
lines(lower.pi~xs, lty=2, col="forestgreen")

predframe<-data.frame(CWSI=xs, pt=preds$fit, upper=upper, lower=lower, pred.lower=lower.pi, pred.upper=upper.pi)
yhat=predict(final.mod)
resids<-agwater$swc-yhat
plot(resids)
acf(resids)



pdf(file="IntervalGraph.pdf")
ggplot(agwater)+geom_point(aes(x=cwsi, y=swc))+
  geom_ribbon(data=predframe, aes(ymin=upper, ymax=lower, x=CWSI), alpha=.2)+
  geom_line(data=predframe, aes(x=CWSI, y=pt))+
  geom_line(data=predframe, aes(x=CWSI, y=pred.lower), linetype=2)+
  geom_line(data=predframe, aes(x=CWSI, y=pred.upper), linetype=2)
dev.off()  

r2<- 1-sum((agwater$swc-yhat)^2)/sum((agwater$swc-mean(agwater$swc))^2)

predframe$IntervalSize<-predframe$pred.upper-predframe$pred.lower

pdf(file="IntervalSize.pdf")
ggplot(predframe)+ geom_line(aes(x=CWSI, IntervalSize))+ylab("Width of 95% Prediction Interval")
dev.off()

set.seed(125)
#Calculate RMSE from 10 fold CV
indices<-sample(1:78, replace=F)
rmses<-numeric(10)
for (i in 0:9){
  holdouts<-(8*i+1):min(c((8*i+8), 78))
  test<-agwater[indices[holdouts],]
  train<-agwater[indices[-holdouts],]
  mod<-loess(swc~cwsi, data=train, family="gaussian",control=loess.control(surface="direct"), span=.35, degree=2)
  preds<-predict(mod, newdata=test)
  mse<-mean((test$swc-preds)^2)
  rmses[i+1]<-sqrt(mse)
}
mean(rmses)


standard.resids=resids/preds.insample$se.fit
