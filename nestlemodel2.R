#Packages Used
library(data.table)
library(FitAR)
library(forecast)
library(ggplot2)
library(lubridate)
library(quantmod)
library(xts)
library(tseries)
library(urca)
#Custom Functions
bestmodel<-function(data,p,d,q) #Best AIC
{
  aic.min<-10^6
  for (i in p)
  {
    for (j in d)
    {
      for (k in q)
      {
        fit<-Arima(data,order=c(i,j,k))
        aic<-AIC(fit)
        if (aic<-aic.min)
        {
          aic.min<-aic
          fit.min<-fit
        }
      }
    }
  }
  return(fit.min)
  return(aic.min)
}



bestrmse<-function(train,test,p,d,q) #Finds Best Model Based on RMSE
{
  min.rmse<-10^6   
  l<-length(test)
  rmse<-array(0,dim=c(length(p)*length(d)*length(q),1))
  p.vec<-array(0,dim=c(length(p)*length(d)*length(q),1))
  d.vec<-array(0,dim=c(length(p)*length(d)*length(q),1))
  q.vec<-array(0,dim=c(length(p)*length(d)*length(q),1))
  count<-1
  for (i in p)
  {
    for (j in d)
    {
      for (k in q)
      {
        fit<-Arima(train,order=c(i,j,k))
        rmse[count]<-accuracy(forecast(fit,h=l)$mean,test)[2]
        p.vec[count]<-i
        d.vec[count]<-j
        q.vec[count]<-k
        if (rmse[count]<min.rmse)
        {
          min.rmse<-rmse[count]
          min.p<-i
          min.d<-j
          min.q<-k
        }
        count<-count+1
      }
    }
  }
  finish<-data.table("p"=p.vec,"d"=d.vec,"q"=q.vec,"RMSE"=rmse)
  return(finish[order(finish$RMSE),])
}

#Load Stock
#More stock data from: <https://in.finance.yahoo.com/q/hp?s=NESTLEIND.NS&a=03&b=01&c=2012&d=11&e=01&f=2014&g=d>
Stock <- read.csv(file.choose()) #Choose File
head(Stock)

#Convert To Time Series
Stock.TS <- xts(Stock$Close,ymd(Stock$Date),drop=FALSE)
Stock.high <- xts(Stock$High,ymd(Stock$Date)) #Time series of highs
Stock.low <- xts(Stock$Low,ymd(Stock$Date)) #Time series of lows
Stock.mean <- (Stock.high + Stock.low)/2; head(Stock.mean) #Create mean of stock
  
        
        #Naming dimensions of Stock Data
#Close
dimnames(Stock.TS) <- list("Date","Close")
plot(Stock.TS)
head(Stock.TS)

#Mean Data
dimnames(Stock.mean) <- list("Date","Daily_Mean_Price")
head(Stock.mean)
plot(Stock.mean)


#Separate Test Data From Train Data
  #Define Time Intervals
    #makes interval from first date to last for train and test
    train.start <- as.Date("2014-10-01"); train.end <- as.Date("2016-06-02") #Start and end
    train.int <-  seq(train.start,train.end,"days") 
          
    test.start <- as.Date("2016-06-03"); test.end <- as.Date("2016-07-19") #start and end
    test.int <- seq(test.start,test.end,"days")   
  
  #Subsetting Test and Train for Closing Price
  Stock.Train <- Stock.TS[train.int,]
  head(Stock.Train)
  tail(Stock.Train)
  plot(Stock.Train)
  
  Stock.Test <- Stock.TS[test.int,]
  head(Stock.Test)
  tail(Stock.Test)
  plot(Stock.Test)
  
  #Convert highs and lows into vector form taking first 30 days
  Stock.high.v <- (Stock.high[test.int,][1:30]); Stock.high.v <- as.vector(Stock.high.v) 
  Stock.low.v <- (Stock.low[test.int,][1:30]); Stock.low.v <- as.vector(Stock.low.v)
  
  #Subsetting Test and Train for Mean Price
  Stock.mean.Train <- Stock.mean[train.int,]
  Stock.mean.test <- Stock.mean[test.int,]
  SmallerStock.mean.test <- Stock.mean.test[1:30] #Taking thirty days
  
#Run Tests on Data
  #Initial tests for stationarity
  adf.test(Stock.Train)
  kpss.test(Stock.Train)
  ndiffs(Stock.Train)
  
  #BoxCox to stabalize variance
  lambda0<-BoxCox.lambda(Stock.Train)
  lambda0
  Stock.Train.t <-BoxCox(Stock.Train,lambda0)
  
  
  #Differencing for stationarity
  diff.Train <- diff(Stock.Train)
  diff.Train.t <- diff(Stock.Train.t)
  plot(diff.Train)
  
  #Final Test for stationarity
  adf.test(diff.Train[-1,])
  kpss.test(diff.Train[-1,]) 
  ndiffs(diff.Train)
  
  #ACF and PACF
  Acf(diff.Train);Pacf(diff.Train) 
  Acf(diff.Train.t);Pacf(diff.Train.t) 
  

  
                                    #Tests for Stock.mean
  #Initial tests for stationarity
  adf.test(Stock.mean.Train)
  kpss.test(Stock.mean.Train)
  ndiffs(Stock.mean.Train)
  
  #BoxCox to stabalize variance
  lambda.m<-BoxCox.lambda(Stock.mean.Train)
  lambda.m
  Stock.mean.Train.t <-BoxCox(Stock.mean.Train,lambda.m)
  
  
  #Differencing for stationarity
  diff.mean.Train <- diff(Stock.mean.Train)
  diff.mean.Train.t <- diff(Stock.mean.Train.t)
  plot(diff.mean.Train)
  #Final Test for stationarity
  adf.test(diff.mean.Train[-1,])
  kpss.test(diff.mean.Train[-1,]) 
  ndiffs(diff.mean.Train)
  
  #ACF and PACF
  Acf(diff.mean.Train);Pacf(diff.Train) 
  Acf(diff.mean.Train.t);Pacf(diff.Train.t) 
  
  
                      #Auto Arima Test
  #Auto Arima for transformed Data and non transformed data.
  auto.arima(Stock.Train.t,stepwise=FALSE,approximation = FALSE,trace=TRUE)
  auto.arima(Stock.Train,stepwise = FALSE,approximation = FALSE,trace=TRUE)
  
  #Auto.Arima for stock.mean
  auto.arima(Stock.mean.Train.t,stepwise=FALSE,approximation = FALSE,trace=TRUE)
  auto.arima(Stock.mean.Train,stepwise = FALSE,approximation = FALSE,trace=TRUE)
  
  #Custom Function to find best RMSE
  bestrmse(Stock.Train,Stock.Test[1:30],p=c(1:6),d=c(1),q=c(1:6))
  
  #Find bestrmse for stock.mean
  bestrmse(Stock.mean.Train,SmallerStock.mean.test,p=c(0:3),d=c(0,1),q=c(0:3))
  bestrmse(Stock.mean.Train.t,SmallerStock.mean.test,p=c(0:5),d=c(0,1),q=c(0:5))
  
#Build Arima for transformed and not transformed
  #with lambda
  Arima(Stock.Train,order= c(0,1,1),lambda = lambda0)
  Arima(Stock.Train,order= c(1,1,0),lambda = lambda0)
  Arima(Stock.Train,order= c(1,1,1),lambda = lambda0)
  fit.stock.train.t <- Arima(Stock.Train.t,order= c(0,1,1),lambda = lambda0)
    #Potential Models (0,1,0 with drift), (0,1,1), (0,1,2)
  #without lambda
  Arima(Stock.Train,order= c(3,1,4)) #ACF:3 PACF:3
  Arima(Stock.Train,order= c(2,1,3)) #ACF:3 PACF:3
  Arima(Stock.Train,order= c(2,1,2))
  fit.stock.train <- Arima(Stock.Train,order= c(2,1,3),include.drift = TRUE)
  
  #Residual Check
  tsdisplay(fit.stock.train$residuals);
  adf.test(fit.stock.train$residuals);kpss.testß(fit.stock.train$residuals) #Stationarity residual test
  tsdisplay(fit.stock.train.t$residuals)
  adf.test(fit.stock.train.t$residuals);kpss.test(fit.stock.train.t$residuals)
                          #Stock.Mean
  #Check residuals for stock.mean and build arima for transformed and not transformed
  #with lambda
  Arima(Stock.mean.Train.t,order= c(0,1,1),lambda = lambda0)
  Arima(Stock.mean.Train.t,order= c(1,1,0),lambda = lambda0)
  Arima(Stock.mean.Train.t,order= c(1,1,1),lambda = lambda0)
  fit.stock.mean.train.t <- Arima(Stock.mean.Train.t,order= c(2,1,0),lambda = lambda0)
  
  #without lambda
  Arima(Stock.mean.Train,order= c(5,1,4),include.drift = TRUE) #ACF:3 PACF:3
  Arima(Stock.mean.Train,order= c(1,1,0)) #ACF:3 PACF:3
  Arima(Stock.mean.Train,order= c(1,1,1))
  fit.stock.mean.train <- Arima(Stock.mean.Train,order= c(3,1,2),include.drift = TRUE)
  
  #Residual Check
  tsdisplay(fit.stock.mean.train$residuals)
  tsdisplay(fit.stock.mean.train.t$residuals)
  
  
  #forecasting
  forecast <- (forecast.Arima(fit.stock.train,h=30))
  forecast.mean <- (forecast.Arima(fit.stock.mean.train,h=30))
  smallertest <- Stock.Test[1:30]
  
  #Matrix of Forecast and Test Data For Manipulation
  accuracy.matrix <- cbind(forecast$mean,Stock.Test[1:30],SmallerStock.mean.test,forecast.mean$mean)#Matrix of forecasts and values
  colnames(accuracy.matrix) <- c("Forecasts","Close","Mean Price","Mean Forecasts")#Titles
  accuracy.matrix
  forecast.month <- accuracy.matrix[,1] #Creates Variable for Forecast Predictions
  forecast.mean.month <- accuracy.matrix[,4]
#BackTesting
  
  #Mean Absolute Error (Percentage)
  forecast.Accuracy <- 100 - ((abs((smallertest - forecast.month))/smallertest) * 100)
  forecast.Accuracy #Prints accuracy of individual predictions
  mean(forecast.Accuracy) #Gives the mean accuracy
  
  #(9.)Calculate same results for stock.mean
  forecast.mean.Accuracy <- 100 - ((abs((SmallerStock.mean.test - forecast.mean.month))/SmallerStock.mean.test) * 100)
  forecast.mean.Accuracy
  mean(forecast.mean.Accuracy)
  forecast.mean.month<=Stock.high.v & forecast.mean.month>=Stock.low.v
    #Calculate how often forecasts hit points inbetween high and lows (In percent)
       (forecast.mean.month<=Stock.high.v & forecast.mean.month>=Stock.low.v)/length(SmallerStock.mean.test)

  #Make Multivariate time series of Point Forecasts, Low, Highs etc with test data set
  accuracy.ts <- xts(x=smallertest); head(accuracy.ts)
  accuracy.ts$forecast <- forecast$mean
  
  
  accuracy.mean.ts <- xts(x=SmallerStock.mean.test); head(accuracy.mean.ts)
  accuracy.mean.ts$forecast <- forecast.mean$mean
  
  #Accuracy function
   result.test <- accuracy(accuracy.matrix[,1],accuracy.matrix[,2])
   result.test
   result.mean.test <- accuracy(forecast.mean.month,SmallerStock.mean.test)
   result.mean.test
   
  #Obtain 95% CI (or whatever percentage you want)
   confidence.int <- .50
   confidence.set <- c(Stock.Train,smallertest)
   fit.total<-Arima(smallertest,order=c(1,1,1))
   
   upper <- fitted(fit.total) + qnorm((1+confidence.int)/2)*sqrt(fit.stock.train$sigma2)
   lower <- fitted(fit.total) - qnorm((1+confidence.int)/2)*sqrt(fit.stock.train$sigma2)
   upper
   
  #Results
   #closing Prices
   result.test
   mean(forecast.Accuracy)
   
   #Mean Prices
   result.mean.test
   mean(forecast.mean.Accuracy)
   
   
  #Obtain 95% CI (or whatever percentage you want)
   confidence.int <- .50
   confidence.set <- c(Stock.Train,smallertest)
   fit.total<-Arima(smallertest,order=c(1,1,1))
   
   upper <- fitted(fit.total) + qnorm((1+confidence.int)/2)*sqrt(fit.stock.train$sigma2)
   lower <- fitted(fit.total) - qnorm((1+confidence.int)/2)*sqrt(fit.stock.train$sigma2)
   upper
    
   # Plot of CI 
   plot(fitted(fit.total),type='l')
   lines(upper,col="red")
   lines(lower,col="green")
   plot(forecast(fit.total))
   
   #Plot of forecast vs Test Data 
   forecast$mean<Stock.high.v & forecast$mean>Stock.low.v #Testing when forecast hits price
  plot(accuracy.ts[,1],type='l',main="Forecasts vs Price",ylab="Price (USD)",xlab="Day") #Plot Test data
  lines(accuracy.ts[,2],type='l',col="red") #Plot Forecast
  lines(Stock.high.v,type='l',col="Green") #Plot high
  lines(Stock.low.v,type='l',col="Blue") #Plot Lows
  lines(forecast$upper[,1],col="Brown") 
  lines(forecast$lower[,1],col="Brown")
  #Plot Mean Prices vs Mean forecasts
  forecast.mean$mean<=Stock.high.v & forecast.mean$mean>=Stock.low.v #Testing when forecast hits price
  plot(accuracy.mean.ts[,1],type='l',main="Forecasts vs Daily Mean Price",ylab="Price (USD)",xlab="Day") #Plot Test data
  lines(accuracy.mean.ts[,2],type='l',col="red") #Plot Forecast
  lines(Stock.high.v,type='l',col="Green") #Plot high
  lines(Stock.low.v,type='l',col="Blue") #Plot Lows
  
  #Do the same for the mean.forecasts
    
      forecast$mean<Stock.high.v & forecast$mean>Stock.low.v
      ###########Ignore for now#######
      #lines(forecast$lower[,1],col="Green")
      #lines(forecast$lower[,2],col="Blue")
      #lines(forecast$upper[,1],col="yellow")
      #lines(forecast$upper[,2],col="purple")
      ######################################

  
  #I have no clue what this does. (Ask Mike)
  plot(Stock.Train, type="n", ylim=range(lower,upper))
  polygon(c(time(Stock.Train),rev(time(Stock.Train))), c(upper,rev(lower)), 
          col=rgb(0,0,0.6,0.2), border=FALSE)
  lines(Stock.Train)
  lines(fitted(fit.stock.train),col='red')
  out <- (Stock.Train < lower | Stock.Train > upper)
  points(time(Stock.Train)[out], Stock.Train[out], pch=19)

  

  