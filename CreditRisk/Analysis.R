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


plot(data$Balance~data$Income)
abline(a=2000+531.92, b=-8.505 )
