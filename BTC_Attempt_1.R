# Preliminary Functions
# 
# Function for Determining Capital Growth over time
KapitalGrowth <- function(x, y){
  m <- length(y)
  y <- y/100
  k <- vector()
  g <- vector()
  k[1] <- x
  for(i in 2:m){
    g[i]<- k[i-1]*y[i]
    k[i]<- k[i-1] + g[i]
  }
  return(k)
}

# Function for Determining Returns of a Given Stock or Index
Returns_p <- function(x) {
  Returns <- vector()
  for(i in 2:length(x)){
    Returns[i]  <- ((x[i] - x[i-1])/x[i-1])*100
  }
  return(Returns)
}

LagReturns1 <- function(x) {
  LagReturns1 <- vector()
  for(i in 2:length(x)){
    LagReturns1[i] <- (x[i-1])
  }
  return(LagReturns1)
}

# Function for Determing the Value at Risk Assuming Normal Distribution
NDVaR <- function(x){
  MU <- mean(x, na.rm = T)
  SD <- sd(x, na.rm = T)
  NDV <- MU + qnorm(.05)*SD
  return(NDV)
}

# Function for Determing the Value at Risk Assuming Non-Normal Distribution
NON_NDVaR <- function(x){
  temp0 <- na.omit(x)
  o.x <- sort(temp0,decreasing = F)
  i <- .05*length(o.x)
  i.r <- round(i)
  d <- abs(i.r-1- i)
  NNVaR <- o.x[i.r-1] + d*(abs(o.x[i.r-1]-o.x[i.r]))
  return(NNVaR)
}

LagReturns1 <- function(x) {
  LagReturns1 <- vector()
  for(i in 2:length(x)){
    LagReturns1[i] <- (x[i-1])
  }
  return(LagReturns1)
}

BTC <- read.csv("C:/Users/jeanp/OneDrive/Documents/GitHub/BTC.csv")

BTC$Returns <- Returns_p(BTC$Adj.Close)

y <- BTC$Returns
x <- 5000
BTC$Invest <- KapitalGrowth(x, y)
sum(BTC$Returns, na.rm = TRUE)
summary(BTC$Returns)
sd(BTC$Returns, na.rm = TRUE)
var(BTC$Returns, na.rm = TRUE)
NDVaR(BTC$Returns)
NON_NDVaR(BTC$Returns)
summary(BTC_Invest)
tail(BTC_Invest, n =1)



## Statistics Table with qwraps2
library(qwraps2)
options(qwraps2_markup = "markdown")
summary_statistics <-
  list(
    "Statistics" =
      list(
        "Mean (sd)" = ~qwraps2::mean_sd(Returns, na_rm = TRUE),
        "median (Q1, Q3)" = ~qwraps2::median_iqr(Returns, na_rm = TRUE),
        "min" = ~min(Returns, na.rm = TRUE),
        "max" = ~max(Returns, na.rm = TRUE),
        "Cumulative Return"= ~sum(Returns, na.rm = TRUE)
      ),
    "Value At Risk" =
      list(
        "Normal Distribution VaR" = ~NDVaR(Returns),
        "Non-Normal Distributed VaR" = ~NON_NDVaR(Returns)
      ),
    "Investment" =
      list(
        "Purchase Price" = ~head(Adj.Close, n = 1),
        "Sell Price" = ~tail(Adj.Close, n = 1),
        "min" = ~min(Invest, na.rm = TRUE),
        "max" = ~max(Invest, na.rm = TRUE),
        "Ending Investment" = ~tail(Invest, n = 1)
      )
  )

summary_table(BTC, summary_statistics)



library(knitr)
## Statistics Table
kable(summary_table(BTC, summary_statistics))



## Chart
df <- data.frame(Date=index(BTC),coredata(BTC))
fig <- df %>% plot_ly(x = ~Date, type="candlestick",
                      open = ~AAPL.Open, close = ~AAPL.Close,
                      high = ~AAPL.High, low = ~AAPL.Low) 
fig <- fig %>% layout(title = "BTC Chart")




## Logistic Regression
BTC <- na.omit(BTC)
BTC$Max <- ifelse(BTC$Returns == max(BTC$Returns, na.rm = TRUE), 1, 0)
BTC$Negative <- ifelse(BTC$Returns < 0, 1, 0)
for(i in 1:length(BTC$Negative)){
  BTC$Negativef[i]  <- BTC$Negative[i + 1] 
}
BTC$Abnorm <- ifelse(BTC$Returns > 8 , 1, 0)
BTC <- select(filter(BTC, Returns > 8), c(Returns, Abnorm, Negativef))

Samp <- sort(sample(nrow(BTC), nrow(BTC)*.7))
TestD <- BTC[Samp, ]
TestP <- BTC[-Samp, ]

Test <- glm(Negativef ~ Returns, family = "binomial", data = TestD)
summary(Test)
prob <- predict.glm(Test, TestP, type = "response")
pred <- ifelse(prob > .5, 1, 0)
confusion_matrix(pred, TestP$Negativef)
TestP$Pred <- pred



library(sde)
mu=0.16; sigma=0.2; P0=40; T = 1/12 ##1 month
nt=50; n=2^(8)
#############Generate nt trajectories
dt=T/n; t=seq(0,T,by=dt)
X=matrix(rep(0,length(t)*nt), nrow=nt)
for (i in 1:nt) {X[i,]= GBM(x=P0,r=mu,sigma=sigma,T=T,N=n)}
##Plot
ymax=max(X); ymin=min(X) #bounds for simulated prices
plot(t,X[1,],t='l',ylim=c(ymin, ymax), col=1,
     ylab="Price P(t)",xlab="time t")
for(i in 2:nt){lines(t,X[i,], t='l',ylim=c(ymin, ymax),col=i)}


a <- Returns_p(BTC$Adj.Close)/100

mu<- mean(a, na.rm = TRUE) # mean of log returns
sig<- sd(a, na.rm = TRUE) # sd of log returns 

library(tidyverse)
price<-rep(NA,365*4)
price[1] <- 200
#start simulating prices
for(j in 1:500){
for(i in 2:(365*4)){
  price[i,j]<-price[i-1,j]*exp(rnorm(1,mu,sig))
  }
}
mc_matrix <- price 
final_mat<-cbind(1:(365*4),mc_matrix)
final_mat<-as.tibble(final_mat)
random_data<-cbind(price,1:(365*4))
