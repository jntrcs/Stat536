library(dplyr)
library(nlme)
library(lubridate)

solar_dat <- read.csv('SolarSavings.csv')
solar_dat$new.date <- as.POSIXct(solar_dat$Date, format = "%m/%d/%y")
solar_dat$month <- month(solar_dat$new.date)
solar_dat$n <- 1:nrow(solar_dat)
solar_dat$PowerBill  <- solar_dat$PowerBill - 9

solar_dat %>% filter(.,Solar == 'Y') %>% select(.,PowerBill) %>% unlist() %>% boxplot()
solar_dat %>% filter(.,Solar == 'N') %>% select(.,PowerBill) %>% unlist() %>% boxplot()
solar_dat %>% filter(.,Solar == 'Y' & PowerBill > 40 + 40*1.5)


# These months fit produce a model that fits the data much better
solar_dat$summer <- ifelse(solar_dat$month %in% c(8,9,10), 1, 0)
solar_dat$winter <- ifelse(solar_dat$month %in% c(1,2,3), 1, 0)



# ACF Plot
lm.fit <- lm(PowerBill ~ Solar + Solar*summer + Solar*winter, data = solar_dat)
acf(lm.fit$residuals)
plot(lm.fit$residuals)

###################################################################################################
# Model
###################################################################################################

ar1.gls <- gls(PowerBill ~ Solar + Solar*summer + Solar*winter,
               correlation = corAR1(form = ~ n),
               data = solar_dat, method = 'ML')
summary(ar1.gls)

ndf <- data.frame(PowerBill = solar_dat$PowerBill, n = solar_dat$n,p = predict(ar1.gls))
ggplot(ndf) + 
  geom_line(aes(x = n, y = p)) + 
  geom_line(aes(n, PowerBill),linetype = "3313") 


###################################################################################################
# Predictions
###################################################################################################

# Pedictions

# Prediction data.frame

phi <- coef(ar1.gls$modelStruct$corStruct, uncons = FALSE, allCoef = TRUE)
## Set up the R matrix for observations AND predictions
N <- nrow(solar_dat) #Number of observed time periods
K <- 90 #number of time periods forward
R <- diag(K+N)
R <- sigma(ar1.gls)^2*zapsmall(phi^(abs(row(R)-col(R)))) ## AR(1) correlation matrix


# Create new indicators for Future Solar Data
n.solar <- rep(1,K)
n.month <- month(seq(ymd('2018-02-07'),by='month', length.out = K))
n.summer <- ifelse(n.month %in% c(8,9,10), 1, 0)
n.winter  <-  ifelse(n.month %in% c(1,2,3), 1, 0)
n.solarN.winter <- rep(0,K)
n.solarY.winter <- n.solar*n.winter
n.solarN.summer <- rep(0,K)
n.solarY.summer <- n.solar*n.summer
n.n <- 51:(51+K-1)

# Create new indicators for Future no Data
n.solar.ns <- rep(0,K)
n.month.ns <- month(seq(ymd('2018-02-07'),by='month', length.out = K))
n.summer.ns <- ifelse(n.month.ns %in% c(8,9,10), 1, 0)
n.winter.ns  <-  ifelse(n.month.ns %in% c(1,2,3), 1, 0)
n.solarN.winter.ns <- rep(0,K)
n.solarY.winter.ns <- n.solar.ns*n.winter.ns
n.solarN.summer.ns <- rep(0,K)
n.solarY.summer.ns <- n.solar.ns*n.summer.ns


Xstar <- cbind(1,n.solar, n.summer, n.winter,n.solarY.summer,n.solarY.winter)
Xstar.ns <- cbind(1,n.solar.ns, n.summer.ns, n.winter.ns,n.solarY.summer.ns,n.solarY.winter.ns)

# Create original Y and X matricies
Y <- matrix(solar_dat$PowerBill)
X <- model.matrix(PowerBill ~ Solar + Solar*summer + Solar*winter, data = solar_dat)

# Pull Coefficients from model
bhat <- coef(ar1.gls)


pred.mn <- Xstar%*%bhat + R[N+(1:K), (1:N)]%*%solve(R[(1:N),(1:N)])%*%
  (Y-X%*%bhat)
pred.ns <- Xstar.ns%*%bhat + R[N+(1:K), (1:N)]%*%solve(R[(1:N),(1:N)])%*%
  (Y-X%*%bhat)

ndf <- tibble(PowerBill = c(solar_dat$PowerBill[-51],rep(0,K+1)),
              n = c(solar_dat$new.date,floor_date(as.POSIXct( seq(ymd('2018-04-01'),by='month', length.out = K)),unit='month')),
              p = c(predict(ar1.gls),pred.mn))
ggplot(ndf) + 
  geom_line(aes(x = n, y = p)) + 
  geom_line(aes(n, PowerBill),linetype = "3313")  +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = solar_dat$new.date[51]))


pred.var <- sigma(ar1.gls)^2*(R[N+(1:K),N+(1:K)]-R[N+(1:K),
                                       (1:N)]%*%solve(R[(1:N),(1:N)])%*%R[(1:N),N+(1:K)]) # conditional variance of MVN

diag(pred.var)

###################################################################################################
# How well does our model fit? Lets uncorrelate the data
###################################################################################################

###################################################################################################
# Uncorrelate the data to look at assumptions
###################################################################################################
R <- diag(N)
R <- sigma(ar1.gls)^2*zapsmall(phi^(abs(row(R)-col(R)))) ## AR(1) correlation matrix

## Set up the R matrix for observations AND predictions

L <- t(chol(R))

X <- model.matrix(PowerBill ~ Solar + Solar*summer + Solar*winter, data = solar_dat)

LX <- solve(L)%*%X

dim(L)
dim(X)

LY <- solve(L)%*%solar_dat$PowerBill

Ldat <- data.frame(LX,LY)

bhat <- solve(t(LX)%*%LX)%*%t(LX)%*%LY
yhat <- LX%*%bhat
ybar <- mean(LY)
resids <- LY - yhat
qqnorm(resids)
qqline(resids)
hist(resids)
acf(resids)

rss <- t(LY - yhat)%*%(LY - yhat)
s2 <- rss/(nrow(solar_dat)-ncol(X))
tss <- t(LY - ybar)%*%(LY - ybar)

R2 <- 1 - rss/tss
### How well does the model fit the data
R2

###################################################################################################
# Now lets look to see if we can predict the differences
###################################################################################################

###########
# How much did we save within data
###########
phi <- coef(ar1.gls$modelStruct$corStruct, uncons = FALSE, allCoef = TRUE)
## Set up the R matrix for observations AND predictions
N <- nrow(solar_dat) #Number of observed time periods
k <- 22 #number of time periods forward
R <- diag(k+N)
R <- sigma(ar1.gls)^2*zapsmall(phi^(abs(row(R)-col(R)))) ## AR(1) correlation matrix
# Create indicators to predict non-solar for the remainder of the data
n.solar.ns2 <- rep(0,k)
n.month.ns2 <- month(seq(ymd('2016-05-01'),by='month', length.out = k))
n.summer.ns2 <- ifelse(n.month.ns2 %in% c(8,9,10), 1, 0)
n.winter.ns2  <-  ifelse(n.month.ns2 %in% c(1,2,3), 1, 0)
n.solarN.winter.ns2 <- rep(0,k)
n.solarY.winter.ns2 <- n.solar.ns2*n.winter.ns2
n.solarN.summer.ns2 <- rep(0,k)
n.solarY.summer.ns2 <- n.solar.ns2*n.summer.ns2



Xstar.ns2 <- cbind(1,n.solar.ns2, n.summer.ns2, n.winter.ns2,n.solarY.summer.ns2,n.solarY.winter.ns2)

pred.nosolar <- Xstar.ns2%*%bhat + R[N+(1:k), (1:N)]%*%solve(R[(1:N),(1:N)])%*%
  (Y-X%*%bhat)
pred.solar <- predict(ar1.gls)[30:51]

pre.savings <- pred.nosolar - solar_dat$PowerBill[solar_dat$Solar == 'Y']
pre.predicted <-  pred.nosolar - pred.solar

sqrt(mean((pre.savings-pre.predicted)^2))




######
# How much did we save post data
######
phi <- coef(ar1.gls$modelStruct$corStruct, uncons = FALSE, allCoef = TRUE)
N <- nrow(solar_dat) #Number of observed time periods
R <- diag(N)
R <- sigma(ar1.gls)*zapsmall(phi^(abs(row(R)-col(R)))) ## AR(1) correlation matrix
X <- model.matrix(PowerBill ~ Solar + Solar*summer + Solar*winter, data = solar_dat)
var.preds <- diag(zapsmall((Xstar-Xstar.ns)%*%solve(t(X)%*%solve(R)%*%X)%*%t(Xstar-Xstar.ns)))



savings <- pred.ns - pred.mn

tot.savings <- c(pre.savings,savings)

sd.difs <- sqrt(diag((Xstar - Xstar.ns)%*%solve(t(X)%*%solve(R)%*%X)%*%t(Xstar - Xstar.ns)))

#sd.difs <- sqrt(diag(t(j)%*%(Xstar - Xstar.ns)%*%solve(t(X)%*%solve(R)%*%X)%*%t(Xstar - Xstar.ns)%*%j))

j <- matrix(1,nrow = K)

Xstar2 <- rbind(X[30:51,],Xstar)
Xstar.ns2 <- rbind(X[30:51,],Xstar.ns)
Xstar.ns2[,2] <- 0 

cum.dif.sds <- numeric(nrow(Xstar2-1))
for(i in 2:nrow(Xstar2-1)){
  xns12 <- (colSums(Xstar2[1:i,]) - colSums(Xstar.ns2[1:i,]))
  cum.dif.sds[i] <- sqrt(zapsmall((xns12)%*%solve(t(X)%*%solve(R)%*%X)%*%t(t(xns12))))
}
cum.dif.sds[1] <- sd.difs[1]

length(tot.savings)
length(cum.dif.sds)

df <- data.frame(post_month= floor_date(as.POSIXct(seq(ymd('2016-05-01'),by='month', length.out = length(tot.savings),unit='month'))),lower = cumsum(tot.savings) - cum.dif.sds*1.96, est = cumsum(tot.savings), upper = cumsum(tot.savings) + cum.dif.sds*1.96)

ggplot(df) + 
  geom_line(aes(x = post_month, y = est ), size = .9) +
  geom_line(aes(x = post_month, y = lower ), color = 'red', linetype = 'dotted', size = .9) +
  geom_line(aes(x = post_month, y = upper ), color = 'red', linetype = 'dotted', size = .9) + 
  geom_hline(aes(yintercept = 8000), linetype = 'dashed') +
  ylab('Amount Saved') + 
  xlab('Year') + 
  theme(text = element_text(size = 15)) + 
  geom_vline(aes(xintercept = as.POSIXct('2023-09-30')), linetype = 'dashed') + 
  geom_vline(aes(xintercept = as.POSIXct('2024-04-30')), linetype = 'dashed')  +
  annotate("text", x = as.POSIXct('2023-06-30'), y = 2500, label = "September 2023", angle = 90, color = 'darkgreen')+
  annotate("text", x = as.POSIXct('2024-07-30'), y = 2500, label = "April 2024", angle = 90, color = 'darkgreen')






