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
data$fitted = fitted(mod)
plot(data$cooks~data$Income)
mean(data$leverage)*2
data$predictions<-fitted(mod)
plot(data$residual~data$predictions)
plot(mod)

require(ggplot2)
pdf(file="resids.pdf")
ggplot(data)+geom_histogram(aes(x=residual), binwidth = .4)+xlab("Residuals")+ylab("Count")+
  theme(axis.text.x=element_text(size=18, angle=0, vjust=0.3), axis.title.x = element_text(size=22),
        axis.title.y=element_text(size=22),
        axis.text.y=element_text(size=16),
        plot.title=element_text(size=16))
dev.off()

pdf(file="fitted.pdf")
ggplot(data)+geom_point(aes(y=residual, x=fitted))+ylab("Standardized Residuals")+xlab("Fitted Values")+
  theme(axis.text.x=element_text(size=18, angle=0, vjust=0.3), axis.title.x = element_text(size=22),
        axis.title.y=element_text(size=22),
        axis.text.y=element_text(size=16),
        plot.title=element_text(size=16))
dev.off()
