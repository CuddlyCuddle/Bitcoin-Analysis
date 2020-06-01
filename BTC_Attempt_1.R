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
kable(summary_table(BTC, summary_statistics))


df <- data.frame(Date=index(BTC),coredata(BTC))
fig <- df %>% plot_ly(x = ~Date, type="candlestick",
                      open = ~AAPL.Open, close = ~AAPL.Close,
                      high = ~AAPL.High, low = ~AAPL.Low) 
fig <- fig %>% layout(title = "BTC Chart")





