data<-read.csv("Credit2.csv")
summary(data)
plot(data$Balance~data$Rating, main="Credit Rating vs. Balance", ylab="Balance", xlab="Rating")
plot(data$Balance~data$Limit, main="Credit Limit vs. Balance", ylab="Balance", xlab="Credit Limit")
plot(data$Rating~data$Limit)
plot(data)

table( data$Ethnicity,data$Gender, data$Student, data$Married)

hist(data$Income)
hist(data$Limit)
hist(data$Rating)
hist(data$Cards)
hist(data$Age)
hist(data$Education)

require(ggplot2)
pdf(file="limitRating.pdf")
ggplot(data)+geom_point(aes(x=Rating, y=Limit))+
  theme(axis.text.x=element_text(size=18, angle=0, vjust=0.3), axis.title.x = element_text(size=22),
        axis.title.y=element_text(size=22),
        axis.text.y=element_text(size=16),
        plot.title=element_text(size=16))
dev.off()
