# Program		        : PERSONAL PROJECT USING R
# Name			        : RUPANTA RWITEEJ DUTTA
# Date of Creation	: 05.08.2017

# Load dataset
library(huge)
data(stockdata)
head(stockdata)

# Check matrix dimensions
dim(stockdata$data)
dim(stockdata$info)

# Combine data into a single matrix to improve readability
total <- cbind(stockdata$info, t(stockdata$data))
View(total)
dim(total)

# Convert matrix into a dataframe and obtain statistics of each row
total_dtf <- as.data.frame(total)
library(Hmisc)
describe(total_dtf)

# Get 10 company row numbers
which(total_dtf$V1 == "AAPL")          # Apple Inc.
which(total_dtf$V1 == "BA")            # Boeing Company
which(total_dtf$V1 == "COF")           # Capital One Financial
which(total_dtf$V1 == "CSCO")          # Cisco Systems
which(total_dtf$V1 == "FDX")           # FedEx Corporation
which(total_dtf$V1 == "GS")            # Goldman Sachs Group
which(total_dtf$V1 == "KO")            # Coca Cola Co.
which(total_dtf$V1 == "MSFT")          # Microsoft Corp.
which(total_dtf$V1 == "T")             # AT&T Inc.
which(total_dtf$V1 == "WFC")           # Wells Fargo

# Plot 10 company stock-time graphs
library(ggplot2)
par(mfrow = c(2, 2))  
plot(stockdata$data[, "V36"], ylab = "Stock Price", xlab = "Days", main = "AAPL", type = "l", col = "red")
plot(stockdata$data[, "V63"], ylab = "Stock Price", xlab = "Days", main = "BA", type = "l", col = "blue")
plot(stockdata$data[, "V73"], ylab = "Stock Price", xlab = "Days", main = "COF", type = "l", col = "green")
plot(stockdata$data[, "V90"], ylab = "Stock Price", xlab = "Days", main = "CSCO", type = "l")

par(mfrow = c(2, 2))  
plot(stockdata$data[, "V161"], ylab = "Stock Price", xlab = "Days", main = "FDX", type = "l", col = "red")
plot(stockdata$data[, "V186"], ylab = "Stock Price", xlab = "Days", main = "GS", type = "l", col = "blue")
plot(stockdata$data[, "V98"], ylab = "Stock Price", xlab = "Days", main = "KO", type = "l", col = "green")
plot(stockdata$data[, "V274"], ylab = "Stock Price", xlab = "Days", main = "MSFT", type = "l")

par(mfrow = c(1, 2)) 
plot(stockdata$data[, "V39"], ylab = "Stock Price", xlab = "Days", main = "T", type = "l", col = "red")
plot(stockdata$data[, "V437"], ylab = "Stock Price", xlab = "Days", main = "WFC", type = "l", col = "blue")

# Plot 10 company stock-time graphs into a single graph
library(xts)
data_ten <- subset(stockdata$data, select = c("V36", "V63", "V73", "V90", "V161", "V186", "V98", "V274", "V39", "V437"))
data_ten_dtf <- as.data.frame(data_ten)

data_info <- subset(stockdata$info, select = c("V1", "V2", "V3"))
data_info_dtf <- as.data.frame(data_info)

library(plyr)
data_ten_dtf <- plyr::rename(data_ten_dtf, c("V36"="AAPL", "V63"="BA", "V73"="COF", "V90"="CSCO", "V161"="FDX", "V186"="GS", "V98"="KO", "V274"="MSFT", "V39"="T", "V437"="WFC"))
dateCol <- c(seq(as.Date('2003-01-01'),as.Date('2006-06-11'),by = 1))
data_ten_dtf <- cbind(dateCol, data_ten_dtf)

library(tidyquant)
data_ten_xts <- as_xts(data_ten_dtf, date_col = dateCol)
class(data_ten_xts)
head(data_ten_xts)
stocks <- as.xts(data.frame(AAPL = data_ten_xts[, "AAPL"], BA = data_ten_xts[, "BA"], COF = data_ten_xts[, "COF"], CSCO = data_ten_xts[, "CSCO"], FDX = data_ten_xts[, "FDX"], GS = data_ten_xts[, "GS"], KO = data_ten_xts[, "KO"], MSFT = data_ten_xts[, "MSFT"], T = data_ten_xts[, "T"], WFC = data_ten_xts[, "WFC"]))
par(mfrow = c(1, 1))  
plot(as.zoo(stocks), screens = 1, lty = 1, xlab = "Days", ylab = "Stock Price", main = "STOCKS", col = rainbow(10))
company_names <- c("AAPL", "BA", "COF", "CSCO", "FDX", "GS", "KO", "MSFT", "T", "WFC")
legend("topleft", company_names, lty = 1, cex = 0.5, col = rainbow(10))

# Get summary of 10 company stocks
summary(data_ten_xts)

# Get information of 10 companies
myfunction <- function(x, parametric = TRUE){
  for(i in x)
  {
    y <- c("select * from data_info_dtf where V1 = '", i)
    z <- paste(y, collapse="")
    a <- c(z, "'")
    b <- paste(a, collapse="")
    print(sqldf(b, row.names=TRUE))
  }
}

library(sqldf)
myfunction(company_names)

# Plot 10 company histograms with fit and rug
par(mfrow = c(2, 2))
h <- hist(data_ten_xts[,"AAPL"], xlab = "Stock Price", col = "yellow", border = "blue", main = "AAPL")
rug(jitter(data_ten_xts$AAPL, amount=0.01))
xfit<-seq(min(data_ten_xts$AAPL), max(data_ten_xts$AAPL), length=40)
yfit<-dnorm(xfit, mean=mean(data_ten_xts$AAPL), sd=sd(data_ten_xts$AAPL))
yfit <- yfit*diff(h$mids[1:2])*length(data_ten_xts$AAPL)
lines(xfit, yfit, col="red", lwd=2)
box()

h <- hist(data_ten_xts[,"BA"], xlab = "Stock Price", col = "yellow", border = "blue", main = "BA")
rug(jitter(data_ten_xts$BA, amount=0.01))
xfit<-seq(min(data_ten_xts$BA), max(data_ten_xts$BA), length=40)
yfit<-dnorm(xfit, mean=mean(data_ten_xts$BA), sd=sd(data_ten_xts$BA))
yfit <- yfit*diff(h$mids[1:2])*length(data_ten_xts$BA)
lines(xfit, yfit, col="red", lwd=2)
box()

h <- hist(data_ten_xts[,"COF"], xlab = "Stock Price", col = "yellow", border = "blue", main = "COF")
rug(jitter(data_ten_xts$COF, amount=0.01))
xfit<-seq(min(data_ten_xts$COF), max(data_ten_xts$COF), length=40)
yfit<-dnorm(xfit, mean=mean(data_ten_xts$COF), sd=sd(data_ten_xts$COF))
yfit <- yfit*diff(h$mids[1:2])*length(data_ten_xts$COF)
lines(xfit, yfit, col="red", lwd=2)
box()

h <- hist(data_ten_xts[,"CSCO"], xlab = "Stock Price", col = "yellow", border = "blue", main = "CSCO")
rug(jitter(data_ten_xts$CSCO, amount=0.01))
xfit<-seq(min(data_ten_xts$CSCO), max(data_ten_xts$CSCO), length=40)
yfit<-dnorm(xfit, mean=mean(data_ten_xts$CSCO), sd=sd(data_ten_xts$CSCO))
yfit <- yfit*diff(h$mids[1:2])*length(data_ten_xts$CSCO)
lines(xfit, yfit, col="red", lwd=2)
box()

par(mfrow = c(2, 2))
h <- hist(data_ten_xts[,"FDX"], xlab = "Stock Price", col = "yellow", border = "blue", main = "FDX")
rug(jitter(data_ten_xts$FDX, amount=0.01))
xfit<-seq(min(data_ten_xts$FDX), max(data_ten_xts$FDX), length=40)
yfit<-dnorm(xfit, mean=mean(data_ten_xts$FDX), sd=sd(data_ten_xts$FDX))
yfit <- yfit*diff(h$mids[1:2])*length(data_ten_xts$FDX)
lines(xfit, yfit, col="red", lwd=2)
box()

h <- hist(data_ten_xts[,"GS"], xlab = "Stock Price", col = "yellow", border = "blue", main = "GS")
rug(jitter(data_ten_xts$GS, amount=0.01))
xfit<-seq(min(data_ten_xts$GS), max(data_ten_xts$GS), length=40)
yfit<-dnorm(xfit, mean=mean(data_ten_xts$GS), sd=sd(data_ten_xts$GS))
yfit <- yfit*diff(h$mids[1:2])*length(data_ten_xts$GS)
lines(xfit, yfit, col="red", lwd=2)
box()

h <- hist(data_ten_xts[,"KO"], xlab = "Stock Price", col = "yellow", border = "blue", main = "KO")
rug(jitter(data_ten_xts$KO, amount=0.01))
xfit<-seq(min(data_ten_xts$KO), max(data_ten_xts$KO), length=40)
yfit<-dnorm(xfit, mean=mean(data_ten_xts$KO), sd=sd(data_ten_xts$KO))
yfit <- yfit*diff(h$mids[1:2])*length(data_ten_xts$KO)
lines(xfit, yfit, col="red", lwd=2)
box()

h <- hist(data_ten_xts[,"MSFT"], xlab = "Stock Price", col = "yellow", border = "blue", main = "MSFT")
rug(jitter(data_ten_xts$MSFT, amount=0.01))
xfit<-seq(min(data_ten_xts$MSFT), max(data_ten_xts$MSFT), length=40)
yfit<-dnorm(xfit, mean=mean(data_ten_xts$MSFT), sd=sd(data_ten_xts$MSFT))
yfit <- yfit*diff(h$mids[1:2])*length(data_ten_xts$MSFT)
lines(xfit, yfit, col="red", lwd=2)
box()

par(mfrow = c(1, 2))
h <- hist(data_ten_xts[,"T"], xlab = "Stock Price", col = "yellow", border = "blue", main = "T")
rug(jitter(data_ten_xts$T, amount=0.01))
xfit<-seq(min(data_ten_xts$T), max(data_ten_xts$T), length=40)
yfit<-dnorm(xfit, mean=mean(data_ten_xts$T), sd=sd(data_ten_xts$T))
yfit <- yfit*diff(h$mids[1:2])*length(data_ten_xts$T)
lines(xfit, yfit, col="red", lwd=2)
box()

h <- hist(data_ten_xts[,"WFC"], xlab = "Stock Price", col = "yellow", border = "blue", main = "WFC")
rug(jitter(data_ten_xts$WFC, amount=0.01))
xfit<-seq(min(data_ten_xts$WFC), max(data_ten_xts$WFC), length=40)
yfit<-dnorm(xfit, mean=mean(data_ten_xts$WFC), sd=sd(data_ten_xts$WFC))
yfit <- yfit*diff(h$mids[1:2])*length(data_ten_xts$WFC)
lines(xfit, yfit, col="red", lwd=2)
box()

# Regression

# Simple linear regression using AAPL data
aapl_reg <- data_ten_xts$AAPL
data_reg <- c(1:1258)
aapl_reg <- cbind(data_reg, aapl_reg)
aapl_reg <- plyr::rename(aapl_reg, c("..1"="Days", "AAPL"="StockPrice"))
aapl_reg <- as.data.frame(aapl_reg)
fit <- lm(StockPrice ~ Days, data = aapl_reg) 
summary(fit)
aapl_reg$StockPrice
fitted(fit)
residuals(fit)

# Scatter plot of stock price by days of AAPL
par(mfrow = c(1, 1))
plot(aapl_reg$Days, aapl_reg$StockPrice, main = "AAPL", xlab = "Days", ylab = "Stock Price", cex = 0.1)
abline(fit, col = "red")

# Polynomial regression
fit2 <- lm(StockPrice ~ Days + I(Days^2), data = aapl_reg)
summary(fit2)

# Scatter plot of stock price by days of AAPL
plot(aapl_reg$Days, aapl_reg$StockPrice, main = "AAPL", xlab = "Days", ylab = "Stock Price", cex = 0.1)
lines(aapl_reg$Days, fitted(fit2), col = "red") 

# Moving average
library(quantmod)
start <- as.Date("2016-01-01")
end <- as.Date("2016-10-01")
getSymbols("AAPL", src = "yahoo", from = start, to = end)
head(AAPL)
candleChart(AAPL, up.col = "black", dn.col = "red", theme = "white", subset = "2016-01-04/")
addSMA(n = 20)

# Classification

# Create a data frame of the 10 companys' mean stock prices
myfunction1 <- function(x, parametric = TRUE){
  for(i in x)
  {
    mean_list <<- c(mean(data_ten_xts[,i]))
    temp_list <<- rbind(temp_list, mean_list)
  }
}
data_ten_list <- t(data_ten_dtf[,-1])
temp_list <- data.frame()
myfunction1(company_names)
rm(mean_list)
temp_list <- plyr::rename(temp_list, c("X63.4634976152623"="Mean"))
data_ten_list <- cbind(temp_list, data_ten_list)
rm(temp_list)

data_ten_list$Category[data_ten_list$Mean > 66.66] <- 3
data_ten_list$Category[data_ten_list$Mean > 33.33 & data_ten_list$Mean <= 66.66] <- 2
data_ten_list$Category[data_ten_list$Mean <= 33.33] <- 1
df <- data_ten_list[-1]
df$Category <- factor(df$Category, levels=c(1, 2, 3), labels=c("low", "average", "high"))
set.seed(1234)                                                       
train <- sample(nrow(df), 0.7*nrow(df))                             
df.train <- df[train,]
df.train_matrix <- as.matrix(df.train)
df.validate <- df[-train,]                                       
table(df.train$Category)                                               
table(df.validate$Category)

# Creating a conditional inference tree with ctree()
library(party)
fit.ctree <- ctree(Category~., data=df.train)
plot(fit.ctree, main="Conditional Inference Tree")

ctree.pred <- predict(fit.ctree, df.validate, type="response")
ctree.perf <- table(df.validate$Category, ctree.pred, 
                    dnn=c("Actual", "Predicted"))
ctree.perf

# Average linkage clustering of 10 companys' stock data
data_ten_list.scaled <- scale(data_ten_list[,2])
distance <- dist(data_ten_list.scaled)
fit.average <- hclust(distance, method="average")
plot(fit.average, hang=-1, cex=.8, main="Average Linkage Clustering")
rect.hclust(fit.average, k=3)
  
# Extract AAPL stocks and compute statistics
prices <- data_ten_xts$AAPL
mean_prices <- round(mean(prices), 2)
mean_prices
sd_prices <- round(sd(prices), 2)
sd_prices

# Plot the histogram along with a legend for AAPL
par(mfrow = c(1, 1))  
hist(prices, breaks = 100, prob=T, cex.main = 0.9)
abline(v = mean_prices, lwd = 2)
legend("topright", cex = 0.8, bty = "n", paste("mean=", mean_prices, "; sd=", sd_prices))

# Compute log returns for AAPL
returns <- diff(log(prices))
returns

# Get AAPL data and confirm that it is non-stationary
spy <- data_ten_xts$AAPL

# Use the default settings
library(tseries)
test <- adf.test(as.numeric(spy))
testbis<-kpss.test(spy)
test
testbis

spy_returns <- diff(log(spy))
spy_returns=spy_returns[-1,]

# Test on the returns
test_returns <- adf.test(as.numeric(spy_returns))
testbiss<-kpss.test(as.numeric(spy_returns))
testbiss

# Cointegration implementation using vars package
data_AAPL_MSFT_xts <- subset(data_ten_xts, select = c("AAPL", "MSFT"))
data_AAPL_MSFT_ts <- ts(data_AAPL_MSFT_xts)
plot(data_AAPL_MSFT_ts, plot.type="single", col=1:2, ylab="Stock Price", main="AAPL vs MSFT")
legend("topleft", c("AAPL", "MSFT"), bty="n", col=1:2, lty=rep(1,2))

adf.test(log(data_AAPL_MSFT_ts[,"AAPL"]))
adf.test(diff(log(data_AAPL_MSFT_ts[,"AAPL"])))

pp.test(log(data_AAPL_MSFT_ts[,"MSFT"]), type="Z(t_alpha)")
pp.test(diff(log(data_AAPL_MSFT_ts[,"MSFT"])), type="Z(t_alpha)")
kpss.test(log(data_AAPL_MSFT_ts[,"MSFT"]))
kpss.test(diff(log(data_AAPL_MSFT_ts[,"MSFT"])))
po.test(log(data_AAPL_MSFT_ts))

po.test(log(data_AAPL_MSFT_ts[,2:1]))

library(urca)
data_AAPL_MSFT_jo<-ca.jo(log(data_AAPL_MSFT_ts), ecdet="const", type="trace")
summary(data_AAPL_MSFT_jo)

# VAR implementation using vars package
library(vars)
AAPL_xts <- subset(data_ten_xts, select = "AAPL")
chartSeries(AAPL_xts) 
MSFT_xts <- subset(data_ten_xts, select = "MSFT")
chartSeries(MSFT_xts)

AAPL.sub <- AAPL_xts['2003-01-01/2006-06-11']

# Calculate returns
AAPL.ret  <- diff(log(AAPL_xts))
MSFT.ret <- diff(log(MSFT_xts))

# VAR modeling to obtain monthly data
AAPL.M  <- to.monthly(AAPL.ret)$AAPL.ret.Close
MSFT.M <- to.monthly(MSFT.ret)$MSFT.ret.Close

# Merge the two databases to get the same length
dataMonthly <- na.omit(merge(AAPL.M,MSFT.M), join='inner')

# Fit a simple VAR model to the data with lag max = 4 and AIC
var1 <- VAR(dataMonthly, lag.max=4, ic="AIC")

# More established model selection 
VARselect(dataMonthly, lag.max=4)
summary(var1)
var1

# Obtain the results
summary(var1)
var1
var1$varresult
var1$type
var1$p
var1$K
var1$obs
var1$totobs
var1$restrictions
var1$call

graphics.off()
par("mar")
par(mar=c(1,1,1,1))
plot(var1) 		    # Diagram of fit and residuals for each variables
coef(var1)		    # Concise summary of the estimated variables
residuals(var1)	  # List of residuals (of the corresponding ~lm)
fitted(var1)	    # List of fitted values
Phi(var1)		      # Coefficient matrices of VMA representation

# Forecast based on VAR model
var.pred <- predict(var1, n.ahead=10, ci=0.95)
graphics.off()
par("mar")
par(mar=c(1,1,1,1))
plot(var.pred)

# Assumptions of normality 

# Plot histogram and density
mu <- mean(returns, na.rm = TRUE)
sigma <- sd(returns, na.rm = TRUE)
x <- seq(-5 * sigma, 5 * sigma, length = nrow(returns))

hist(returns, breaks = 100, main = "Histogram of returns for AAPL", cex.main = 0.8, prob=TRUE)
lines(x, dnorm(x, mu, sigma), col = "red", lwd = 2)

par(mfrow = c(1, 2))

# AAPL data
qqnorm(as.numeric(returns),
       main = "AAPL empirical returns qqplot()",
       cex.main = 0.8)
qqline(as.numeric(returns),  lwd = 2)
grid()

# Normal random data
normal_data <- rnorm(nrow(returns), mean = mu, sd = sigma)

qqnorm(normal_data, main = "Normal returns", cex.main = 0.8)
qqline(normal_data, lwd = 2)
grid()

answer <- shapiro.test(as.numeric(returns))
answer[[2]]

# Correlations
aapl <- data_ten_xts$AAPL
msft <- data_ten_xts$MSFT
price_matrix<-cbind(aapl, msft)
returns_matrix <- apply(price_matrix, 2, function(x) diff(log(x)))
plot(returns_matrix)

sv <- as.xts(returns_matrix)
head(sv)

cor(sv)

# Filtering data

# Find the outliers
plot(returns_matrix)
outliers <- which(sv[, 2] < -1.0)

# If any outliers exist, remove them
if(length(outliers) > 0) {
  sv <- sv[-outliers, ]
}

cor(sv)

# Volatility
data_AAPL_ts <- ts(AAPL_xts)

# Autocorrelation of returns and squared returns
par(mfrow = c(2, 1))
acf(data_AAPL_ts, main = "returns", cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
grid()

acf(data_AAPL_ts ^ 2, main = "returns squared", cex.lab = 0.8, cex.axis = 0.8)
grid()

par(mfrow = c(1, 1))
acf(sv[, 1] ^ 2, main = "Actual returns squared", cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
grid()

par(mfrow = c(1, 2))
acf(sv[, 1]^3)
acf(abs(sv[, 1]))

# Garch

# Plot prices
library(Quandl)
library(fGarch)
par(mfrow = c(1, 1))
plot(AAPL_xts, main="AAPL")

# Calculate log-returns for GARCH analysis
library(PerformanceAnalytics)
AAPL.ret = CalculateReturns(AAPL_xts, method="log")

# Remove first NA observation
AAPL.ret = AAPL.ret[-1,]
colnames(AAPL.ret) = "AAPL"

# Plot returns
plot(AAPL.ret, main="AAPL Returns")

# Function to perform Lagrange Multiplier Test for ARCH effect of a time series
archTest=function(rtn,m=10){
  y=(rtn-mean(rtn))^2
  T=length(rtn)
  atsq=y[(m+1):T]
  x=matrix(0,(T-m),m)
  for (i in 1:m){
    x[,i]=y[(m+1-i):(T-i)]
  }
  md=lm(atsq~x)
  summary(md)
}

archTest(AAPL.ret)

acf(AAPL.ret)
acf(AAPL.ret^2)
pacf(AAPL.ret^2)

# Confirm arch effects
m1=garchFit(~1+garch(1,1),data=AAPL.ret,trace=F,cond.dist = "std")     # Fit an ARCH(3) model
summary(m1)
plot(m1)

head(MSFT_xts)
plot(MSFT_xts, main="MSFT")

####GARCH model 
#in many situations, the GARCH(1,1) model appears to be adequate
#1/ demonstration of empirical analysis of GARCH processes
#2/ compare different GARCH models
#3/ show prediction of a GARCH model 

msft=log(MSFT_xts$MSFT+1)
plot(msft, type="l", main="MSFT")
acf(msft)
acf(msft^2)
pacf(msft^2)
m4=garchFit(~1+garch(1,1),data=msft,trace=F)
summary(m4)

# All estimates are significant at 5% level except for the normality tests, model checking statistics indicates that 
# this GARCH(1,1) model is adequate for r(t)

# We plot the standardized residuals and the volatility to check our model 
v1=volatility(m4)                # Obtain volatility
resi=residuals(m4,standardize=T) # Standardized residuals
vol=ts(v1,frequency=12)
res=ts(resi,frequency=12)

# Show volatility and residuals
plot(vol,xlab='year',ylab='volatility',type='l')
plot(msft, type="l", main="MSFT")

plot(res,xlab='year',ylab='st. resi',type='l')

# Obtain ACF & PACF
acf(resi,lag=24)
pacf(resi,lag=24)
acf(resi^2,lag=24)
pacf(resi^2,lag=24) 

plot(vol,xlab='year',ylab='volatility',type='l')

# Obtain plot of predictive intervals
par(mfcol=c(1,1))
upp=3.869+1.3*v1
low=3.869-1.3*v1
tdx=c(1:1258)/365+2003
plot(tdx,msft,xlab='year',ylab='series',type='l')
lines(tdx,upp,lty=2,col='red')
lines(tdx,low,lty=2,col='red')
abline(h=c(3.869))

# Compare different GARCH models

# Student-t innovations
m5=garchFit(~1+garch(1,1),data=msft,trace=F,cond.dist="std")

summary(m5)
v2=volatility(m5)
plot(v2,type="l")

m6=garchFit(~1+garch(1,1),data=msft,trace=F,cond.dist='sstd')
summary(m6)
v3=volatility(m6)

par(mfcol=c(3,1))
plot(tdx,v1,xlab='year',ylab='volatility',type='l',ylim=c(0.06,0.3))
title(main='(a) Gaussian')
plot(tdx,v2,xlab='year',ylab='volatility',type='l',ylim=c(0.06,0.3))
title(main='(b) Student-t')
plot(tdx,v3,xlab='year',ylab='volatility',type='l',ylim=c(0.06,0.3))
title(main='(c) Skew Student-t') 

cor(cbind(v1,v2,v3))

# AIC
m4@fit$ics[1]
m5@fit$ics[1]
m6@fit$ics[1]

# BIC
m4@fit$ics[2]
m5@fit$ics[2]
m6@fit$ics[2]

# QQ plot of the standardized residuals of the model with skew Student t distribtuion
par(mfcol=c(1,1))
plot(m4)
plot(m5)
plot(m6)

normal<-predict(m4, n.ahead=12)
normal
student<-predict(m5, n.ahead=12)
student
skewstudent<-predict(m6, n.ahead=12)
skewstudent

# Load Apple data and calculate log-returns
ret.aapl <- dailyReturn(AAPL_xts, type='log')
chartSeries(ret.aapl)

# EGARCH

# Specify EGARCH(1,1) model with only constant in mean equation
library(rugarch)
egarch11.spec = ugarchspec(variance.model = list(model="eGARCH", garchOrder=c(1,1)), 
                           mean.model = list(armaOrder=c(0,0)))
aapl.egarch11.fit = ugarchfit(spec=egarch11.spec, data=ret.aapl)
coef(aapl.egarch11.fit)

ni.egarch11 <- newsimpact(aapl.egarch11.fit)
par(font.axis=2, font.lab=2)
plot(ni.egarch11$zx, ni.egarch11$zy, type="l", lwd=2, col="blue", main="EGARCH(1,1) - AAPL", 
     ylab=ni.egarch11$yexpr, xlab=ni.egarch11$xexpr)

# TGARCH

# Specify TGARCH(1,1) model with only constant in mean equation
tgarch11.spec = ugarchspec(variance.model = list(model="fGARCH", submodel="TGARCH", garchOrder=c(1,1)), 
                           mean.model = list(armaOrder=c(0,0)))
aapl.tgarch11.fit = ugarchfit(spec=tgarch11.spec, data=ret.aapl)
coef(aapl.egarch11.fit)

ni.tgarch11 <- newsimpact(aapl.tgarch11.fit)
plot(ni.tgarch11$zx, ni.tgarch11$zy, type="l", lwd=2, col="blue", main="TGARCH(1,1) - AAPL", 
     ylab=ni.tgarch11$yexpr, xlab=ni.tgarch11$xexpr)