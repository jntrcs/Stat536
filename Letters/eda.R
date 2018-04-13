#Eda

letters<-read.csv("https://mheaton.byu.edu/Courses/Stat536/Case%20Studies/LetterRecognition/Data/letter-recognition.txt", header=T)
head(letters)

ggplot(letters)+geom_bar(aes(x=letter))

require(GGally)
a=apply(letters[,-1],2, jitter, amount=.5)

ggpairs(letters[,2:4])
ggpairs(letters[,7:11])

ggpairs(data.frame(a[,c(1,12,8,6)]))
ggpairs(data.frame(a[,5:8]))
ggpairs(data.frame(a[,9:12]))
ggpairs(data.frame(a[,13:16]))

plot(a[,1]~a[,6])

locs<-expand.grid(1:100, 1:100)
locs$loc<-50+10*locs$Var1-20*locs$Var2
locs$color<-ifelse(locs$loc==0, "red", ifelse(locs$loc>0, "blue", "black"))
pdf(file="hyper.pdf")
plot(locs$Var1~locs$Var2, col=locs$color, main="2D Hyperplane Example", ylab="Y", xlab="X")
dev.off()

