#Data
solar=read.csv("SolarSavings.csv")
solar$Date<-as.Date(solar$Date, format="%m/%d/%Y")
solar<-solar[order(solar$Date),]
solar$DateNumber<-as.numeric(solar$Date)
solar$DateNumber<-solar$DateNumber-min(solar$DateNumber)
solar$Month <- as.numeric(format(solar$Date, "%m"))
solar$Summer<-solar$Month%in%c(8,9,10)
solar$Winter<-solar$Month%in% c(1,2,3)
solar$PowerUsed<-(solar$PowerBill-9)
solar$n<-1:51

require(nlme)
##Note for the sake of getting correlated data (a requirement for this class project, we are ignoring month as a covariate, which 
# zeroes out our estimate of phi)
final.mod<-gls(PowerUsed~Solar+Solar*Winter+Solar*Summer, correlation = corAR1(form = ~n), data=solar, method="ML")
summary(final.mod)

require(xtable)
a=summary(final.mod)
xtable(a$tTable)

#The average monthly bill over a whole year is [1 0 3/12 4/12 0 0]
#The average monthly bill over a whole year with solar panels is [1 1 3/12 4/12 3/12 4/12]
#Taking the difference results in average monthly savings
C<-matrix(c(0,-1,0,0,-3/12,-3/12), nrow = 1)
betahats=coef(final.mod)
#Pt estimate for amount saved per month over a full calendar year
C%*%betahats


times <- 1:51
rho <- coef(final.mod$modelStruct$corStruct, uncons=F, allCoef=T)
sigma <- final.mod$sigma
###############
H <- abs(outer(times, times, "-"))
V <- sigma * rho^H
p <- nrow(V)
V[cbind(1:p, 1:p)] <- V[cbind(1:p, 1:p)] * sigma
X<-model.matrix(~Solar+Solar*Winter+Solar*Summer, data=solar)
as.numeric(C%*%betahats) +c(-1,1) * qt(.995, 45) * as.numeric(sqrt(C %*% solve(t(X)%*%solve(V,X), t(C))))

#If we ignored correlation in the data
as.numeric(C%*%betahats) +c(-1,1) * qt(.995, 45) * as.numeric(sigma*sqrt(C %*% solve(t(X)%*%X, t(C))))

###Checking our residuals by uncorrelating them
L<-t(chol(V))
ystar<-solve(L,solar$PowerUsed)
xstar<-solve(L, X)
uncor.mod<-lm(ystar~-1+xstar)
summary(uncor.mod)
pdf(file="resids.pdf")
hist(rstandard(uncor.mod), main = "Standardized Residuals", xlab="Residual")
dev.off()
require(car)
pdf(file="AVPlot.pdf")
avPlots(uncor.mod)
dev.off()

pdf(file="fitted.pdf")
plot(uncor.mod$residuals~uncor.mod$fitted.values, ylab="Residuals", xlab="Fitted Values")
dev.off()
qqnorm(uncor.mod$residuals)
 qqline(uncor.mod$residuals)

ggplot(solar)+geom_line(aes(x=Date, y=PowerUsed, color=Solar))+geom_point(aes(x=Date, y=PowerUsed,color=Solar, shape=Solar))

