##Model Fitting
require(car)
mod<-lm(Balance~.-Limit, data=data)
avPlot(mod, variable = c("Income"))
avPlot(mod, variable = c("Limit"))
avPlot(mod, variable = c("Rating"))
avPlot(mod, variable = c("Cards"))
avPlot(mod, variable = c("Age"))
avPlot(mod, variable = c("Education"))
avPlot(mod, variable = c("GenderFemale"))

pdf(file="AVPlot.pdf")
avPlots(mod, terms=~.-Ethnicity-Student-Gender-Married)
dev.off()

mod<-lm()

require(GGally)
ggpairs(data)

mod<-lm(Balance~Income+Limit+Cards+Age+Student, data=data)
summary(mod)
AIC(mod)

mod.inter<-lm(Balance~Income+Limit+Cards+Age+Student+Student:Income, data=data)
summary(mod.inter)
AIC(mod.inter)

par(mfrow=c(1,1))
avPlot(mod.inter, variable = "Income:StudentYes")


resids<-rstudent(mod)
plot(resids)
qqnorm(resids)
hist(resids)
data$residual<-resids
data[abs(data$residual)>2,]
pnorm(-2)*2*294 #expected number of residuals greater than +-2
data$leverage<-influence(mod)$hat
plot(data$leverage~data$Limit)
data$cooks<-cooks.distance(mod)
plot(data$cooks~data$Income)
mean(data$leverage)*2
data$predictions<-fitted(mod)
plot(data$residual~data$predictions)
plot(mod)
