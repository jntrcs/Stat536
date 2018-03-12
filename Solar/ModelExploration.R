#Data
solar=read.csv("SolarSavings.csv")
solar$Date<-as.Date(solar$Date, format="%m/%d/%Y")
solar<-solar[order(solar$Date),]
solar$DateNumber<-as.numeric(solar$Date)
solar$DateNumber<-solar$DateNumber-min(solar$DateNumber)
solar$Month <- as.numeric(format(solar$Date, "%m"))
solar$Summer<-solar$Month%in%c(7,8,9,10)
solar$Winter<-solar$Month%in% c(1,2,12)
solar$PowerUsed<-solar$PowerBill-9
solar$n<-1:51


solar$SummerD<- pmin(abs(13-solar$Month), abs(1-solar$Month))/6
solar$SummerD2<-solar$Summer^2
##Model fitting
mod<-lm(PowerBill~Date+Solar+Date:Solar, data=solar)
acf(mod$residuals)
plot(mod$residuals~solar$Date)

hist(mod$residuals)


require(nlme)
mod.base<-gls(PowerUsed~-1+Solar+Winter:Solar+Summer:Solar, correlation = corAR1(form=~DateNumber), data=solar, method="ML")
summary(mod.base) 

mod<-gls(PowerUsed~-1+Solar+DateNumber+SummerD+SummerD:Solar, correlation = corAR1(form=~DateNumber), data=solar, method="ML")
summary(mod) 

mod.quad<-gls(PowerUsed~-1+Solar+SummerD+SummerD:Solar+SummerD2+Solar:SummerD2, correlation = corAR1(form=~n), data=solar, method="ML")
summary(mod.quad)


neg.cor<-gls(PowerUsed~Solar+Solar*Winter+Solar*Summer, correlation = corAR1(form = ~n), data=solar, method="ML")
summary(neg.cor)

plot(solar$PowerBill~solar$Date, type="l", ylim=c(0, 250))
lines(predict(mod.base)~solar$Date, col="green")
lines(predict(mod)~solar$Date)
lines(predict(mod.quad)~solar$Date, col="red")
lines(predict(neg.cor)~solar$Date, col="green")

plot(mod.quad$residuals~mod.quad$fitted)
plot(mod.quad$residuals)
AIC(mod.base)
AIC(mod)
AIC(mod.quad)

plot(mod$residuals~solar$Date)
abline(v=min(solar$Date[solar$Solar=="Y"]))
acf(mod$residuals)

times <- 1:51
rho <- .05096927
sigma <- 26.42298
###############
H <- abs(outer(times, times, "-"))
V <- sigma * rho^H
p <- nrow(V)
V[cbind(1:p, 1:p)] <- V[cbind(1:p, 1:p)] * sigma
X<-model.matrix(~1+Solar+SummerD+SummerD:Solar+SummerD2+Solar:SummerD2,, data=solar)
as.numeric(C%*%betahats) +c(-1,1) * qt(.975, 45) * as.numeric(sqrt(C %*% solve(t(X)%*%solve(V,X), t(C))))

#If we ignored correlation in the data
as.numeric(C%*%betahats) +c(-1,1) * qt(.975, 45) * as.numeric(sigma*sqrt(C %*% solve(t(X)%*%X, t(C))))

###Checking our residuals by uncorrelating them
L<-t(chol(V))
ystar<-solve(L,solar$PowerUsed)
xstar<-solve(L, X)
uncor.mod<-lm(ystar~-1+xstar)
summary(uncor.mod)
hist(rstandard(uncor.mod))
require(car)
avPlots(uncor.mod)
qqnorm(uncor.mod$residuals)
