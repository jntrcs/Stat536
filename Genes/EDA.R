#EDA

data=read.table("https://mheaton.byu.edu/Courses/Stat536/Case%20Studies/GeneExpression/Data/GeneExpression2.txt", header=T)

hist(data$Malignant, main="Malignant Score", xlab="Score")
all(apply(data, 2, class)=="numeric")
data[1,10]
max(data)
min(data)

require(GGally)
pdf(file="PairsPlot.pdf")
ggpairs(data[,c(1,19,51,433,232,1345)])
dev.off()
