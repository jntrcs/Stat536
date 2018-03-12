library(tidyverse)
library(lubridate)

solar_dat <- read.csv('SolarSavings.csv')
solar_dat$new.date <- as.POSIXct(solar_dat$Date, format = "%m/%d/%y")
solar_dat$month <- month(solar_dat$new.date)

solar_dat$n <- 1:nrow(solar_dat)

solar_dat[order(solar_dat$PowerBill, decreasing= TRUE),]

solar_dat %>% filter(month == 3)

ggplot(solar_dat, aes(new.date, PowerBill, color = Solar)) + geom_point() + geom_line(linetype = "3313") + theme(text = element_text(size = 15))



ggplot(solar_dat) + 
  geom_jitter(aes(x = month, y = PowerBill, color = Solar), width = .05, height = 0) +
  scale_x_continuous(breaks = round(seq(min(solar_dat$month), max(solar_dat$month), by = 1),1)) +
  scale_y_continuous(breaks = round(seq(min(solar_dat$PowerBill), max(solar_dat$PowerBill), by = 25),1)) +
  theme(text = element_text(size = 15)) +
  annotate("segment", x = 7, xend = 7, y = 95, yend = 180, colour = "pink", size=1.5, alpha=1, arrow=arrow()) +
  annotate("segment", x = 7, xend = 7, y = 180, yend = 95, colour = "pink", size=1.5, alpha=1, arrow=arrow())+
  annotate("segment", x = 9, xend = 9, y = 9, yend = 70, colour = "Blue", size=1.5, alpha=0.15, arrow=arrow()) +
  annotate("segment", x = 9, xend = 9, y = 65, yend = 7, colour = "Blue", size=1.5, alpha=0.15, arrow=arrow()) +
  annotate("segment", x = 1, xend = 1, y = 80, yend = 135, colour = "Blue", size=1.5, alpha=0.2, arrow=arrow()) +
  annotate("segment", x = 1, xend = 1, y = 130, yend = 75, colour = "Blue", size=1.5, alpha=0.2, arrow=arrow())

set.seed(12)
ggplot(solar_dat %>% filter(Solar == 'N')) + 
  geom_jitter(aes(x = month, y = PowerBill), width = .05, height = 0,  color = 'red') +
  scale_x_continuous(breaks = round(seq(min(solar_dat$month), max(solar_dat$month), by = 1),1)) +
  scale_y_continuous(breaks = round(seq(min(solar_dat$PowerBill), max(solar_dat$PowerBill), by = 25),1)) +
  # Add arrow
  annotate("segment", x = 7, xend = 7, y = 109, yend = 183, colour = "pink", size=3, alpha=0.6, arrow=arrow()) +
  annotate("segment", x = 7, xend = 7, y = 183, yend = 105, colour = "pink", size=3, alpha=0.6, arrow=arrow())+
  # Add arrow 2
  annotate("segment", x = 9, xend = 9, y = 170, yend = 220, colour = "pink", size=3, alpha=0.6, arrow=arrow()) +
  annotate("segment", x = 9, xend = 9, y = 220, yend = 170, colour = "pink", size=3, alpha=0.6, arrow=arrow()) + 
  theme(text = element_text(size = 15))

ggplot(solar_dat %>% filter(Solar == 'Y')) + 
  geom_jitter(aes(x = month, y = PowerBill), width = .05, height = 0, color = 'Blue') +
  scale_x_continuous(breaks = round(seq(min(solar_dat$month), max(solar_dat$month), by = 1),1)) +
  scale_y_continuous(breaks = round(seq(min(solar_dat$PowerBill), max(solar_dat$PowerBill), by = 25),1)) +
  # Add arrow
  annotate("segment", x = 1, xend = 1, y = 82, yend = 145, colour = "Blue", size=3, alpha=0.2, arrow=arrow()) +
  annotate("segment", x = 1, xend = 1, y = 145, yend = 82, colour = "Blue", size=3, alpha=0.2, arrow=arrow())+
  # Add arrow 2
  annotate("segment", x = 9, xend = 9, y = 17, yend = 80, colour = "Blue", size=3, alpha=0.2, arrow=arrow()) +
  annotate("segment", x = 9, xend = 9, y = 80, yend = 17, colour = "Blue", size=3, alpha=0.2, arrow=arrow()) + 
  theme(text = element_text(size = 15))


###################################################################################################
# Plot of Amount saved overtime
###################################################################################################

phi <- -.1676131
N <- nrow(solar_dat) #Number of observed time periods
R <- diag(N)
R <- sigma(ar1.gls)*zapsmall(phi^(abs(row(R)-col(R)))) ## AR(1) correlation matrix
X <- model.matrix(PowerBill ~ Solar + Solar*summer + Solar*winter, data = solar_dat)
var.preds <- zapsmall((Xstar-Xstar1)%*%solve(t(X)%*%solve(R)%*%X)%*%t(Xstar-Xstar1))

cum.dif.sds <- numeric(K)
for(i in 2:K){
  xns12 <- (colSums(Xstar[1:i,]) - colSums(Xstar1[1:i,]))
  cum.dif.sds[i] <- sqrt(zapsmall((xns12)%*%solve(t(X)%*%solve(R)%*%X)%*%t(t(xns12))))
}
cum.dif.sds[1] <- sd.difs[1]
length(cum.dif.sds*1.96)
cumsum(savings) + cum.dif.sds*1.96

cum.dif.sds <- cum.dif.sds

df <- data.frame(post_month= floor_date(as.POSIXct( seq(ymd('2018-04-01'),by='month', length.out = K)),unit='month'),lower = cumsum(savings) - cum.dif.sds*1.96, est = cumsum(savings), upper = cumsum(savings) + cum.dif.sds*1.96)

ggplot(df) + 
  geom_line(aes(x = post_month, y = est ), size = .9) +
  geom_line(aes(x = post_month, y = lower ), color = 'red', linetype = 'dotted', size = .9) +
  geom_line(aes(x = post_month, y = upper ), color = 'red', linetype = 'dotted', size = .9) + 
  geom_hline(aes(yintercept = 8000), linetype = 'dashed') +
  ylab('Year') + 
  xlab('Amount Saved') + 
  theme(text = element_text(size = 15))
