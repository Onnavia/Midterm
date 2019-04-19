rm(list=ls())
install.packages("quantmod")
library(quantmod)
tickers<-c("INDF.JK", "UNVR.JK", "TLKM.JK")
getSymbols(tickers, from = '2007-01-01', to = '2018-12-31', auto.assign = TRUE)
data = new.env()
getSymbols(tickers, from = "2007-01-01", env = data , auto.assign = TRUE)
ls(data)
names(data)
head(data$INDF.JK)
firmList<-merge(Ad(INDF.JK), Ad(UNVR.JK), Ad(TLKM.JK))
options(max.print=1000000)
firmList
head(firmList)
colnames(firmList)<-c("Indofood", "Unilever", "Sampoerna")
head(firmList)
install.packages("xts")
library(xts)
#Calculate weekly return using weeklyReturn function one by one
returnList<-weeklyReturn(Ad(INDF.JK), subset=NULL, type='arithmetic',
             leading=TRUE)
weeklyReturn(Ad(UNVR.JK), subset=NULL, type='arithmetic',
                      leading=TRUE)
weeklyReturn(Ad(TLKM.JK), subset=NULL, type='arithmetic',
                 leading=TRUE)
#Calculate weekly return using PMwR 
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
head(na.omit(Return.calculate(firmList)))
firmList.week<-to.weekly(firmList, OHLC=FALSE) 
head(firmList.week)
  Return.calculate(firmList.week,method="discrete")
#Calculate daily and monthly return using PMwR
install.packages("PMwR")
library(PMwR)
firmList.day<-returns(firmList, pad=0)
head(firmList.day)
firmList.day
firmList.mon<-returns(firmList, period = "month")
head(firmList.mon)
firmList.mon
head(merge(firmList, firmList.day))

=====================================================================
#MVP (Minimum Variance Portfolio)
=====================================================================
  install.packages("fBasics")
library(fBasics)
Sigma = cov(firm_data1[,2:4])
std = sqrt(diag(Sigma))
ones = rep(1,3)     
one.vec = matrix(ones, ncol=1)
a = inv(Sigma)%*%one.vec
b = t(one.vec)%*%a
mvp.w =a / as.numeric(b)
mvp.w
mvp.ret<-sum((mvp.w)*colMeans(firm_data1[,2:4]))
mvp.ret
