#Partial Least Squares

require(pls)
data<-as.matrix(data)
data[,-1]=scale(data[,-1])
data[,1]<-scale(data[,1], scale = F)
pls.mod=plsr(data[,1]~data[,-1], 3,  validation="CV", scale=F)
pdf(file="PLS.Resids.pdf")
ggplot(data.frame(Resids=pls.mod$residuals[,,3]))+geom_histogram(aes(x=Resids), bins=9)
dev.off()
psi<-pls.mod$loadings
Z<-data[,-1]%*%psi
sigma2<-1/(102-4)*sum((pls.mod$residuals[,,3]-mean(pls.mod$residuals[,,3]))^2)
var.betas<-sigma2*diag(psi%*%solve(t(Z)%*%Z, t(psi)))
pls.betas<-coef(pls.mod)[,,1]
lowers<-pls.betas-qt(.995,102-4)*sqrt(var.betas)
uppers<-pls.betas+qt(.995,102-4)*sqrt(var.betas)


pls.intervals<-data.frame(gene=colnames(data[,-1]), beta.hat=pls.betas, lowerbound=lowers, upperbound=uppers)
pls.intervals<-pls.intervals[order(-abs(pls.intervals$beta.hat)),]
save(pls.intervals, r2.pls, file="PLSIntervals.RData")

pred.pls<-predict(pls.mod)[,,3]
r2.pls<-sum((pred.pls-mean(data[,1]))^2)/sum((data[,1]-mean(data[,1]))^2)
#R squared is .8827

plot(pred.pls, data$Malignant)
pls=data.frame(name=colnames(data)[-1], betapls=coef(pls.mod))
b= merge(m, pls, by="name")
plot(b$betahat~b$Malignant.33.comps)


pls<-pls[order(-abs(pls$Malignant.33.comps)),]

validationplot(pls.mod)

?setdiff

top<-intersect(pls$name[1:50], pca$name[1:50])
intersect(top, sig.beta$genes)

head(pls)
names(pls)<-c("genes", "PLS.betas")
head(pls)
head(pca)
pca.int<-pca
pca<-data.frame(genes=pca.int$name, pca.betas=pca.int$betahat)
head(pca)

merged<-merge(pca, pls)
head(merged)
head(beta.lasso)
merged<-merge(merged, beta.lasso)
head(merged)
head(beta.ridge)
names(beta.ridge)<-c("genes", "ridge.betas")
merged<-merge(merged, beta.ridge)
head(merged)
names(merged)[4]<-"lasso.betas"
all.betas<-merged
save(all.betas, file="AllBetaHats.RData")

load("AllBetaHats.RData")
top.pca<-all.betas[order(-abs(all.betas$pca.betas)),]
top.pca<-top.pca[1:50, 1:2]
top.lasso<-all.betas[order(-abs(all.betas$lasso.betas)),]
top.lasso<-top.lasso[1:50, c(1,4)]
top.pls<-all.betas[order(-abs(all.betas$PLS.betas)),]
top.pls<-top.pls[1:50, c(1,3)]
top.ridge<-all.betas[order(-abs(all.betas$ridge.betas)),]
top.ridge<-top.ridge[1:50, c(1,5)]

intersect(intersect(top.pca$genes, top.pls$genes), intersect(top.lasso$genes, top.ridge$genes))

