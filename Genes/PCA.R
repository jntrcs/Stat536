load("PCA.RData")
data=read.table("https://mheaton.byu.edu/Courses/Stat536/Case%20Studies/GeneExpression/Data/GeneExpression2.txt", header=T)

X<-as.matrix(data[,-1])
X<-apply(X, 2, scale)

y<-data$Malignant
y<-scale(y, scale=F)

hist(log(y/(1-y)))

psi<- eigen(cov(X))

psi$values[1:10]
Z=X%*%psi$vectors
(cumsum(psi$values)/sum(psi$values))[1:55]
#sum(psi$values)==sum(diag(cov(X)))

Z<-as.matrix(Z)

save(data, psi, X, file="PCA.RData")

mod<-lm(y~1)
all.aic<-rep(0, 103)
for (i in 1:103){
  mod<-lm(y~Z[,1:i])
  all.aic[i]<-BIC(mod)
    
} 
plot(all.aic)
#20 and 33 seem to produce noticable drops. I will do one cross validation to get prediction on those two as well as 
#an extremely overfit model using 90 principal components

indices<-sample(1:102, 102, replace=F)
train<-y[indices[1:85],]
test<-y[indices[86:102],]
z.train<-Z[indices[1:85],]
z.test<-Z[indices[86:102],]

mod20<-lm(train~z.train[,1:20])
mod33<-lm(train~z.train[,1:33])
mod83<-lm(train~z.train[,1:83])

thetas20<-coef(mod20)
preds20<-thetas20[1]+z.test[,1:20]%*%(thetas20[2:21])
plot(preds20~test)
mse20<-mean((preds20-test)^2)

thetas33<-coef(mod33)
preds33<-thetas33[1]+z.test[,1:33]%*%(thetas33[2:34])
plot(preds33~test)
mse33<-mean((preds33-test)^2)

thetas83<-coef(mod83)
preds83<-thetas90[1]+z.test[,1:83]%*%(thetas90[2:84])
plot(preds83~test)
mse83<-mean((preds83-test)^2)

best.mod<-lm(y~Z[,1:33]) #Based on choosing number of eigenvectors that summarize 90% of the data

pdf(file="PCA.Resids.pdf")
ggplot(data.frame(Resids=best.mod$residuals))+geom_histogram(aes(x=Resids), bins=10)
dev.off()
Z.red<-Z[,1:33]
theta.hats<-coef(best.mod)[-1]
psi.red<-psi$vectors[,1:33]
beta.hats<-psi.red%*%theta.hats
importance<-order(-abs(beta.hats))
head(importance)
tail(importance)
plot(y~X[,19])

var.betas<-sigma(best.mod)^2*diag(psi.red%*%solve(t(Z.red)%*%Z.red, t(psi.red)))
head(var.betas)

lowers<-beta.hats-qt(.995,102-34)*sqrt(var.betas)
uppers<-beta.hats+qt(.995,102-34)*sqrt(var.betas)

significant<-lowers>0|uppers<0
load("BrandonData.RData")
pca=data.frame(name=colnames(X), betahat=beta.hats, lower=lowers, upper=uppers, significant= significant)
pca<-pca[order(-abs(pca$betahat)),]

head(pca)
summary(best.mod)
r2.pca<-.893
save(pca, r2.pca, file="PCAIntervals.RData")

head(sig.beta)
class(sig.beta)
com<-merge(pca, sig.beta, by.x="name", by.y="genes")
head(com)
plot(com$beta.hat~com$betahat, col=as.numeric(com$significant)+1)
points(rep(0, nrow(m))~m$betahat, pch=".",col=as.numeric(m$significant)+1)



require(pls)
pcr.mod=pcr(Malignant~., data=data, scale=T, validation="CV")
validationplot(pcr.mod, val.type="MSEP")
summary(pcr.mod)
