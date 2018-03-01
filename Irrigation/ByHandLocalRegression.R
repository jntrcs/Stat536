pred<-numeric(100)
xs<-  seq(0, 1, length=100)
for (i in 1:100){
  x<-xs[i]
  mod<-lm(swc~cwsi, data=data, weights=dnorm(data$cwsi-x, 0, .1))
  pred[i]=predict(mod, data.frame(cwsi=x))
}
plot(data$swc~data$cwsi)
lines(pred~xs, col="red")

data$cwsi2<-data$cwsi^2
pred2<-numeric(100)
xs<-  seq(0, 1, length=100)
for (i in 1:100){
  x<-xs[i]
  mod<-lm(swc~cwsi+cwsi2, data=data, weights=dnorm(data$cwsi-x, 0, .1))
  pred2[i]=predict(mod, data.frame(cwsi=x, cwsi2=x^2))
}
#plot(data$swc~data$cwsi)
lines(pred2~xs, col="blue")
