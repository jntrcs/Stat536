data=read.table("https://mheaton.byu.edu/Courses/Stat536/Case%20Studies/CWSI-NonLin/Data/AgricultureWater.txt", header=T)
data
require(ggplot2)
ggplot(data)+geom_point(aes(x=cwsi, y=swc))

data$cwsi2<-data$cwsi^2
mod=lm(swc~., data=data)
summary(mod)
hist(rstandard(mod))
plot(mod$residuals~mod$fitted.values)
plot(data$swc~data$cwsi)
f<-function(x)28.3536-13.0225*x+8.82771*x^2
curve(f, add=T)
plot(mod$residuals)
