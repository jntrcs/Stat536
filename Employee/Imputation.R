###Multiple Imputation
require(ggplot2)

data<-read.table("https://mheaton.byu.edu/Courses/Stat536/Case%20Studies/Employee/Data/Employee.txt",header=T)
pdf(file="MissingSamp.pdf")
subdat<-data[!apply(data, 1, anyNA),]
ggplot(subdat)+geom_point(aes(x=JobSat, y=JobPerf))+xlab("Job Satisfaction")+ylab("Job Performance")+
  theme(axis.text.x=element_text(size=18, angle=0, vjust=0.3), axis.title.x = element_text(size=22),
        axis.title.y=element_text(size=22),
        axis.text.y=element_text(size=16),
        plot.title=element_text(size=16))
dev.off()
data<-as.matrix(data[,-1])

#require(GGally)
require(mvtnorm)
#ggpairs(data)




M0<-apply(data,2,mean,na.rm=T)
M0
sigma0<-cov(data, use="pairwise.complete.obs")
sigma0
missing<-apply(data, 2, is.na)

last.M<-M0
last.sig<-sigma0
M<-10
start<-system.time()
coeff.all<-matrix(0, nrow=M, ncol=12)
mu2<-rep(0, M)
rsquares<-rep(0,M)
sigmas<-rep(0, M)

for (i in 1:M){
  newdat<-matrix(0, nrow=nrow(data), ncol=ncol(data))
  for (r in 1:nrow(missing))
  {
    miss.row<-which(missing[r,])
    newdat[r,]<-data[r,]
    if (length(miss.row)>0){
      sigma12<-last.sig[miss.row, -miss.row]
      if(class(sigma12)!="matrix"){
        sigma12<-matrix(sigma12, nrow=1)
      }
      sigma2<-last.sig[-miss.row, -miss.row]
      sig1<-last.sig[miss.row, miss.row]
      
      m.cond<- mean(last.M[miss.row])+sigma12%*%solve(sigma2,(data[r, -miss.row]-last.M[-miss.row]))
      sig.cond<-sig1 - sigma12 %*%solve(sigma2, t(sigma12))
      newdat[r, miss.row]<-rmvnorm(1, m.cond, sig.cond)
      if (r==2){
        mu2[i]<-m.cond
      }
      }
  }
  
  last.M<-apply(newdat,2,mean)
  last.sig<-cov(newdat)
  mod<-lm(newdat[,5]~newdat[,-5])
  coeff.all[i, ]<-cbind(coef(summary(mod))[,1], coef(summary(mod))[,2])
  rsquares[i]<-summary(mod)$r.squared
  sigmas[i]<-summary(mod)$sigma
  
}
save(rsquares, coeff.all, sigmas, mu2, file="ImputationResults.RData")
