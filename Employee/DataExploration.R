data<-read.table("https://mheaton.byu.edu/Courses/Stat536/Case%20Studies/Employee/Data/Employee.txt",header=T)
summary(data)
plot(data$Age~data$Tenure)
var(data, na.rm=T)
?cor
plot(data)

require(GGally)
pdf(file="GGPairs.pdf")
ggpairs(as.data.frame(data[,-1]))
dev.off()
sum(apply(data, 1, anyNA))#/nrow(data)

