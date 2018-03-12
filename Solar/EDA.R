#EDA
solar=read.csv("SolarSavings.csv")
solar$Date<-as.Date(solar$Date, format="%m/%d/%Y")
solar<-solar[order(solar$Date),]
require(ggplot2)
ggplot(solar)+geom_line(aes(x=Date, y=PowerBill, color=Solar))+geom_point(aes(x=Date, y=PowerBill,color=Solar, shape=Solar))

hist(solar$PowerBill[solar$Solar=='N'])
solar

acf(solar$PowerBill)

