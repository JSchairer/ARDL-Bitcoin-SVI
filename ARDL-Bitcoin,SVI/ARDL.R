####Datenanalyse####
#Daten einlesen und Pakete laden
library(readxl)
library(lubridate)
library(ARDL)
library(dplyr)
library(ggplot2)
library(modelsummary)

DailyData <- read_excel("RData.xlsx")

#Datumsvariable erstellen und in Monat und Jahr aufteilen
DailyData$Date <- as.Date(DailyData$Date)
DailyData$Week <- week(DailyData$Date)
DailyData$Year <- year(DailyData$Date)

DailyData$lnReturnBitcoinsq <- DailyData$lnReturnBitcoin*DailyData$lnReturnBitcoin
DailyData$lnReturnEthsq <- DailyData$lnReturnEth*DailyData$lnReturnEth
DailyData$lnGoogleBTC <- log(DailyData$GoogleBTC)
DailyData$lnGoogleETH <- log(DailyData$GoogleEth)

#Datensatz fuer Wochenbetrachtung erstellen
WeeklyData <- DailyData %>%
  group_by(Year, Week) %>% 
  summarise(Return_Bitcoin = mean(lnReturnBitcoinsq),Return_Eth = mean(lnReturnEthsq), Bitcoin = mean(Bitcoin), Ethereum = mean(Ethereum), GoogleBTC=mean(lnGoogleBTC), GoogleEth=mean(lnGoogleETH))

WeeklyData$Yearweek <- NA
WeeklyData$Yearweek <- 1:105

WeeklyData$lnReturnBitcoinsq <- WeeklyData$Return_Bitcoin*WeeklyData$Return_Bitcoin
WeeklyData$lnReturnEthsq <- WeeklyData$Return_Eth*WeeklyData$Return_Eth
WeeklyData$lnGoogleBTC <- log(WeeklyData$GoogleBTC)
WeeklyData$lnGoogleETH <- log(WeeklyData$GoogleEth)

####Grafiken erstellen####
###Verlaufsgrafiken ueber die Zeit
# Bitcoin Daily
ggplot(DailyData, aes(x=Date, y=Bitcoin)) +
  geom_line() + 
  xlab("Date")+
  ylab("Bitcoin Price in USD")+
  theme_classic() +
  geom_vline(xintercept = as.numeric(DailyData$Date[400]), linetype="dotted")

ggplot(DailyData, aes(x=Date, y=GoogleBTC)) +
  geom_line() + 
  xlab("Date")+
  ylab("Google Searches for Bitcoin")+
  theme_classic() +
  geom_vline(xintercept = as.numeric(DailyData$Date[400]), linetype="dotted")

# Bitcoin Weekly
ggplot(WeeklyData, aes(x=Yearweek, y=Bitcoin)) +
  geom_line() + 
  xlab("Week")+
  ylab("Bitcoin Price in USD")+
  theme_classic()+
  geom_vline(xintercept = 60, linetype="dotted")

WeeklyData$expGoogleBTC <- exp(WeeklyData$GoogleBTC)
ggplot(WeeklyData, aes(x=Yearweek, y=expGoogleBTC)) +
  geom_line() + 
  xlab("Week")+
  ylab("Google Searches for Bitcoin")+
  theme_classic()+
  geom_vline(xintercept = 60, linetype="dotted")

# Eth Daily
ggplot(DailyData, aes(x=Date, y=Ethereum)) +
  geom_line() + 
  xlab("Date")+
  ylab("Ethereum Price in USD")+
  theme_classic()+
  geom_vline(xintercept = as.numeric(DailyData$Date[400]), linetype="dotted")

ggplot(DailyData, aes(x=Date, y=GoogleEth)) +
  geom_line() + 
  xlab("Date")+
  ylab("Google Searches for Ethereum")+
  theme_classic() +
  geom_vline(xintercept = as.numeric(DailyData$Date[400]), linetype="dotted")

# Eth Weekly
ggplot(WeeklyData, aes(x=Yearweek, y=Ethereum)) +
  geom_line() + 
  xlab("Week")+
  ylab("Ethereum Price in USD")+
  theme_classic()+
  geom_vline(xintercept = 60, linetype="dotted")

WeeklyData$expGoogleEth <- exp(WeeklyData$GoogleEth)
ggplot(WeeklyData, aes(x=Yearweek, y=expGoogleEth)) +
  geom_line() + 
  xlab("Week")+
  ylab("Google Searches for Ethereum")+
  theme_classic()+
  geom_vline(xintercept = 60, linetype="dotted")

####Calculate Variance####
#BITCOIN DAILY
model1 <- auto_ardl(lnReturnBitcoinsq ~ Bitcoin, data = DailyData,
                    max_order = c(5,5))
model1$top_orders
model1.c.variance <- ardl(lnReturnBitcoinsq ~ Bitcoin, data = DailyData, order=c(1,2))
summary(model1.c.variance)


model2 <- auto_ardl(lnReturnBitcoinsq ~ Bitcoin + lnGoogleBTC, data = DailyData,
                    max_order = c(5,5,5))
model2$top_orders
model2.c.variance <- ardl(lnReturnBitcoinsq ~ Bitcoin + lnGoogleBTC, data = DailyData, order=c(1,2,2))
summary(model2.c.variance)

anova(model1.c.variance, model2.c.variance)


models <- list()
models[['Bitcoin Daily']] <- model1.c.variance
models[['Bitcoin Daily + Google']] <- model2.c.variance
modelsummary(models)
modelsummary(models, output="latex")

#ETH DAILY
model1 <- auto_ardl(lnReturnEthsq ~ Ethereum, data = DailyData,
                    max_order = c(10,10))
model1$top_orders
model3.c.variance <- ardl(lnReturnEthsq ~ Ethereum, data = DailyData, order=c(5,5))
summary(model3.c.variance)


model2 <- auto_ardl(lnReturnEthsq ~ Ethereum + lnGoogleETH, data = DailyData,
                    max_order = c(10,10,10))
model2$top_orders
model4.c.variance <- ardl(lnReturnEthsq ~ Ethereum + lnGoogleETH, data = DailyData, order=c(5,5,5))
summary(model4.c.variance)

anova(model3.c.variance, model4.c.variance)


models <- list()
models[['Ethereum Daily']] <- model3.c.variance
models[['Ethereum Daily + Google']] <- model4.c.variance
modelsummary(models)
modelsummary(models, output="latex")

#BITCOIN Weekly
model1 <- auto_ardl(lnReturnBitcoinsq ~ Bitcoin, data = WeeklyData,
                    max_order = c(5,5))
model1$top_orders
model5.c.variance <- ardl(lnReturnBitcoinsq ~ Bitcoin, data = WeeklyData, order=c(1,0))
summary(model5.c.variance)


model2 <- auto_ardl(lnReturnBitcoinsq ~ Bitcoin + lnGoogleBTC, data = WeeklyData,
                    max_order = c(5,5,5))
model2$top_orders
model6.c.variance <- ardl(lnReturnBitcoinsq ~ Bitcoin + lnGoogleBTC, data = WeeklyData, order=c(1,2,1))
summary(model6.c.variance)

anova(model5.c.variance, model6.c.variance)


models <- list()
models[['Bitcoin Weekly']] <- model5.c.variance
models[['Bitcoin Weekly + Google']] <- model6.c.variance
modelsummary(models)
modelsummary(models, output="latex")

#ETH Weekly
model1 <- auto_ardl(lnReturnEthsq ~ Ethereum, data = WeeklyData,
                    max_order = c(10,10))
model1$top_orders
model7.c.variance <- ardl(lnReturnEthsq ~ Ethereum, data = WeeklyData, order=c(1,2))
summary(model7.c.variance)


model2 <- auto_ardl(lnReturnEthsq ~ Ethereum + lnGoogleETH, data = WeeklyData,
                    max_order = c(10,10,10))
model2$top_orders
model8.c.variance <- ardl(lnReturnEthsq ~ Ethereum + lnGoogleETH, data = WeeklyData, order=c(1,2,0))
summary(model8.c.variance)

anova(model7.c.variance, model8.c.variance)


models <- list()
models[['Ethereum Weekly']] <- model7.c.variance
models[['Ethereum Weekly + Google']] <- model8.c.variance
modelsummary(models)
modelsummary(models, output="latex")


####ARDL####
#Bitcoin Daily
#Calculate best Order
model1 <- auto_ardl(lnReturnBitcoin ~ Bitcoin, data = DailyData,
                    max_order = c(5,5))
model1$top_orders

model2 <- auto_ardl(lnReturnBitcoin ~ Bitcoin + lnGoogleBTC, data = DailyData,
                    max_order = c(5,5,5))
model2$top_orders

#Calculate model
model1.c <- ardl(lnReturnBitcoin ~ Bitcoin, data = DailyData, order=c(1,2))
summary(model1.c)

model2.c <- ardl(lnReturnBitcoin ~ Bitcoin + lnGoogleBTC, data = DailyData, order=c(4,2,2))
summary(model2.c)


models <- list()
models[['Bitcoin Daily']] <- model1.c
models[['Bitcoin Daily + Google']] <- model2.c
modelsummary(models)
modelsummary(models, output="latex")

#Bitcoin Weekly
model1 <- auto_ardl(Return_Bitcoin ~ Bitcoin, data = WeeklyData,
                    max_order = c(5,5))
model1$top_orders

model2 <- auto_ardl(Return_Bitcoin ~ Bitcoin + GoogleBTC, data = WeeklyData,
                    max_order = c(5,5,5))
model2$top_orders

#Calculate model
model3.c <- ardl(Return_Bitcoin ~ Bitcoin, data = WeeklyData, order=c(2,0))
summary(model3.c)

model4.c <- ardl(Return_Bitcoin ~ Bitcoin + GoogleBTC, data = WeeklyData, order=c(1,2,2))
summary(model4.c)


models <- list()
models[['Bitcoin Weekly']] <- model3.c
models[['Bitcoin Weekly + Google']] <- model4.c
modelsummary(models)
modelsummary(models, output="latex")

#Ethereum Daily
#Calculate best Order
model5 <- auto_ardl(lnReturnEth ~ Ethereum, data = DailyData,
                    max_order = c(5,5))
model5$top_orders

model6 <- auto_ardl(lnReturnEth ~ Ethereum + lnGoogleETH, data = DailyData,
                    max_order = c(5,5,5))
model6$top_orders

#Calculate model
model5.c <- ardl(lnReturnEth ~ Ethereum, data = DailyData, order=c(1,2))
summary(model5.c)

model6.c <- ardl(lnReturnEth ~ Ethereum + lnGoogleETH, data = DailyData, order=c(1,2,3))
summary(model6.c)


models <- list()
models[['Ethereum Daily']] <- model5.c
models[['Ethereum Daily + Google']] <- model6.c
modelsummary(models)
modelsummary(models, output="latex")

#Ethereum Weekly
model1 <- auto_ardl(Return_Eth ~ Ethereum, data = WeeklyData,
                    max_order = c(5,5))
model1$top_orders

model2 <- auto_ardl(Return_Eth ~ Ethereum + GoogleEth, data = WeeklyData,
                    max_order = c(5,5,5))
model2$top_orders

#Calculate model
model7.c <- ardl(Return_Eth ~ Ethereum, data = WeeklyData, order=c(1,2))
summary(model7.c)

model8.c <- ardl(Return_Eth ~ Ethereum + GoogleEth, data = WeeklyData, order=c(1,2,1))
summary(model8.c)


models <- list()
models[['Ethereum Weekly']] <- model7.c
models[['Ethereum Weekly + Google']] <- model8.c
modelsummary(models)
modelsummary(models, output="latex")

####NARDL - Modelle####
#Am Beispiel Bitcoin
library(nardl)
a <- nardl(lnReturnBitcoin ~ Bitcoin, data = DailyData, graph=TRUE)
summary(a)

#Am Beispiel Ethereum
b <- nardl(lnReturnEth ~ Ethereum, data = DailyData, graph=TRUE)
summary(b)

####ARDL Modelle####
####Predict und Vergleich Bitcoin Daily####
DailyData$Insample <- NA
DailyData$Insample[1:400] <- 1
DailyData$Insample[401:717] <- 0

DailyData$lnReturnBitcoin.insample <- NA
DailyData$lnReturnBitcoin.insample[DailyData$Insample==1] <- DailyData$lnReturnBitcoin

DailyData$lnReturnEth.insample <- NA
DailyData$lnReturnEth.insample[DailyData$Insample==1] <- DailyData$lnReturnEth

library(dLagM)

model1.c.test <- ardlDlm(lnReturnBitcoin.insample ~ Bitcoin, data = DailyData, p = 2, q = 1)
summary(model1.c.test)

rem.p = list(GoogleBTC = c(1,2), Bitcoin = c())
rem.q = c()
remove = list(p = rem.p , q = rem.q)
model2.c.test <- ardlDlm(lnReturnBitcoin.insample ~ Bitcoin + lnGoogleBTC, data = DailyData, p = 2, q = 1, remove = remove)
summary(model2.c.test)

new = data.frame(DailyData[c("lnReturnBitcoin","Bitcoin","lnGoogleBTC","lnReturnBitcoin.insample")])
new$lnReturnBitcoin.insample <- new$lnReturnBitcoin

new$predict.M1 <- predict(model1.c.test$model, new)
new$predict.M2 <- predict(model2.c.test$model, new)

new$MSE.M1 <- (new$lnReturnBitcoin-new$predict.M1)^2
mean(new$MSE.M1, na.rm=TRUE)

new$MSE.M2 <- (new$lnReturnBitcoin-new$predict.M2)^2
mean(new$MSE.M2, na.rm=TRUE)

t.test(new$MSE.M1, new$MSE.M2, paired=TRUE)
lsr::cohensD(new$MSE.M1, new$MSE.M2, method="paired")

boxplot(new$MSE.M1, new$MSE.M2, ylim=c(0,0.02), xlab="Model Daily Bitcoin", ylab="Mean Square Error of prediction", names = c("Without Google","With Google"))

####Predict und Vergleich Eth Daily####
library(dLagM)

model1.c.test <- ardlDlm(lnReturnEth.insample ~ Ethereum, data = DailyData, p = 2, q = 1)
summary(model1.c.test)

model2.c.test <- ardlDlm(lnReturnEth.insample ~ Ethereum + lnGoogleETH, data = DailyData, p = 2, q = 1)
summary(model2.c.test)

new = data.frame(DailyData[c("lnReturnEth","Ethereum","lnGoogleETH","lnReturnEth.insample")])
new$lnReturnEth.insample <- new$lnReturnEth

new$predict.M1 <- predict(model1.c.test$model, new)
new$predict.M2 <- predict(model2.c.test$model, new)

new$MSE.M1 <- (new$lnReturnEth-new$predict.M1)^2
mean(new$MSE.M1, na.rm=TRUE)

new$MSE.M2 <- (new$lnReturnEth-new$predict.M2)^2
mean(new$MSE.M2, na.rm=TRUE)

t.test(new$MSE.M1, new$MSE.M2, paired=TRUE)
lsr::cohensD(new$MSE.M1, new$MSE.M2, method="paired")

boxplot(new$MSE.M1, new$MSE.M2, ylim=c(0,0.02), xlab="Model Daily Ethereum", ylab="Mean Square Error of prediction", names = c("Without Google","With Google"))

####Predict und Vergleich Bitcoin Weekly####
WeeklyData$Insample <- NA
WeeklyData$Insample[1:60] <- 1
WeeklyData$Insample[61:105] <- 0

WeeklyData$lnReturnBitcoin.insample <- NA
WeeklyData$lnReturnBitcoin.insample[WeeklyData$Insample==1] <- WeeklyData$Return_Bitcoin

WeeklyData$lnReturnEth.insample <- NA
WeeklyData$lnReturnEth.insample[WeeklyData$Insample==1] <- WeeklyData$Return_Eth


model1.c.test <- ardlDlm(lnReturnBitcoin.insample ~ Bitcoin, data = WeeklyData, p = 2, q = 1)
summary(model1.c.test)

rem.p = list(GoogleBTC = c(1,2), Bitcoin = c())
rem.q = c()
remove = list(p = rem.p , q = rem.q)
model2.c.test <- ardlDlm(lnReturnBitcoin.insample ~ Bitcoin + GoogleBTC, data = WeeklyData, p = 2, q = 1, remove = remove)
summary(model2.c.test)

new = data.frame(WeeklyData[c("Return_Bitcoin","Bitcoin","GoogleBTC","lnReturnBitcoin.insample")])
new$lnReturnBitcoin.insample <- new$lnReturnBitcoin

new$predict.M1 <- predict(model1.c.test$model, new)
new$predict.M2 <- predict(model2.c.test$model, new)

new$MSE.M1 <- (new$lnReturnBitcoin-new$predict.M1)^2
mean(new$MSE.M1, na.rm=TRUE)

new$MSE.M2 <- (new$lnReturnBitcoin-new$predict.M2)^2
mean(new$MSE.M2, na.rm=TRUE)

t.test(new$MSE.M1, new$MSE.M2, paired=TRUE)
lsr::cohensD(new$MSE.M1, new$MSE.M2, method="paired")

boxplot(new$MSE.M1, new$MSE.M2, xlab="Model Weekly Bitcoin", ylab="Mean Square Error of prediction", names = c("Without Google","With Google"))


####Predict und Vergleich Eth Weekly####
WeeklyData$Insample <- NA
WeeklyData$Insample[1:60] <- 1
WeeklyData$Insample[61:105] <- 0

WeeklyData$lnReturnBitcoin.insample <- NA
WeeklyData$lnReturnBitcoin.insample[WeeklyData$Insample==1] <- WeeklyData$Return_Bitcoin

WeeklyData$lnReturnEth.insample <- NA
WeeklyData$lnReturnEth.insample[WeeklyData$Insample==1] <- WeeklyData$Return_Eth


model1.c.test <- ardlDlm(lnReturnEth.insample ~ Ethereum, data = WeeklyData, p = 2, q = 1)
summary(model1.c.test)

rem.p = list(GoogleBTC = c(1,2), Ethereum = c())
rem.q = c()
remove = list(p = rem.p , q = rem.q)
model2.c.test <- ardlDlm(lnReturnEth.insample ~ Ethereum + GoogleEth, data = WeeklyData, p = 2, q = 1, remove = remove)
summary(model2.c.test)

new = data.frame(WeeklyData[c("Return_Eth","Ethereum","GoogleEth","lnReturnEth.insample")])
new$lnReturnEth.insample <- new$Return_Eth

new$predict.M1 <- predict(model1.c.test$model, new)
new$predict.M2 <- predict(model2.c.test$model, new)

new$MSE.M1 <- (new$Return_Eth-new$predict.M1)^2
mean(new$MSE.M1, na.rm=TRUE)

new$MSE.M2 <- (new$Return_Eth-new$predict.M2)^2
mean(new$MSE.M2, na.rm=TRUE)

t.test(new$MSE.M1, new$MSE.M2, paired=TRUE)
lsr::cohensD(new$MSE.M1, new$MSE.M2, method="paired")

boxplot(new$MSE.M1, new$MSE.M2, xlab="Model Weekly Ethereum", ylab="Mean Square Error of prediction", names = c("Without Google","With Google"))

####Auto Arima####
ReturnBtc <- DailyData$lnReturnBitcoin
library(forecast)
fit <- auto.arima(ReturnBtc)
autoplot(forecast(fit, h = 100))


####Varianz Modelle####
####Predict und Vergleich Bitcoin Daily####
DailyData$Insample <- NA
DailyData$Insample[1:400] <- 1
DailyData$Insample[401:717] <- 0

DailyData$lnReturnBitcoin.insample <- NA
DailyData$lnReturnBitcoin.insample[DailyData$Insample==1] <- DailyData$lnReturnBitcoinsq

DailyData$lnReturnEth.insample <- NA
DailyData$lnReturnEth.insample[DailyData$Insample==1] <- DailyData$lnReturnEthsq

library(dLagM)

model1.c.test <- ardlDlm(lnReturnBitcoin.insample ~ Bitcoin, data = DailyData, p = 2, q = 1)
summary(model1.c.test)

rem.p = list(lnGoogleBTC = c(1,2), Bitcoin = c())
rem.q = c()
remove = list(p = rem.p , q = rem.q)
model2.c.test <- ardlDlm(lnReturnBitcoin.insample ~ Bitcoin + lnGoogleBTC, data = DailyData, p = 2, q = 1, remove = remove)
summary(model2.c.test)

new = data.frame(DailyData[c("lnReturnBitcoinsq","Bitcoin","lnGoogleBTC","lnReturnBitcoin.insample")])
new$lnReturnBitcoin.insample <- new$lnReturnBitcoinsq

new$predict.M1 <- predict(model1.c.test$model, new)
new$predict.M2 <- predict(model2.c.test$model, new)

new$MSE.M1 <- (new$lnReturnBitcoinsq-new$predict.M1)^2
mean(new$MSE.M1, na.rm=TRUE)

new$MSE.M2 <- (new$lnReturnBitcoinsq-new$predict.M2)^2
mean(new$MSE.M2, na.rm=TRUE)

t.test(new$MSE.M1, new$MSE.M2, paired=TRUE)
lsr::cohensD(new$MSE.M1, new$MSE.M2, method="paired")

boxplot(new$MSE.M1, new$MSE.M2, ylim=c(0,0.00005), xlab="Model Daily Bitcoin", ylab="Mean Square Error of prediction", names = c("Without Google","With Google"))

####Predict und Vergleich Eth Daily####
library(dLagM)

model1.c.test <- ardlDlm(lnReturnEth.insample ~ Ethereum, data = DailyData, p = 2, q = 1)
summary(model1.c.test)

model2.c.test <- ardlDlm(lnReturnEth.insample ~ Ethereum + lnGoogleETH, data = DailyData, p = 2, q = 1)
summary(model2.c.test)

new = data.frame(DailyData[c("lnReturnEthsq","Ethereum","lnGoogleETH","lnReturnEth.insample")])
new$lnReturnEth.insample <- new$lnReturnEthsq

new$predict.M1 <- predict(model1.c.test$model, new)
new$predict.M2 <- predict(model2.c.test$model, new)

new$MSE.M1 <- (new$lnReturnEthsq-new$predict.M1)^2
mean(new$MSE.M1, na.rm=TRUE)

new$MSE.M2 <- (new$lnReturnEthsq-new$predict.M2)^2
mean(new$MSE.M2, na.rm=TRUE)

t.test(new$MSE.M1, new$MSE.M2, paired=TRUE)
lsr::cohensD(new$MSE.M1, new$MSE.M2, method="paired")

boxplot(new$MSE.M1, new$MSE.M2, ylim=c(0,0.0001), xlab="Model Daily Ethereum", ylab="Mean Square Error of prediction", names = c("Without Google","With Google"))

####Predict und Vergleich Bitcoin Weekly####
WeeklyData$Insample <- NA
WeeklyData$Insample[1:60] <- 1
WeeklyData$Insample[61:105] <- 0

WeeklyData$lnReturnBitcoin.insample <- NA
WeeklyData$lnReturnBitcoin.insample[WeeklyData$Insample==1] <- WeeklyData$lnReturnBitcoinsq

WeeklyData$lnReturnEth.insample <- NA
WeeklyData$lnReturnEth.insample[WeeklyData$Insample==1] <- WeeklyData$lnReturnEthsq


model1.c.test <- ardlDlm(lnReturnBitcoin.insample ~ Bitcoin, data = WeeklyData, p = 2, q = 1)
summary(model1.c.test)

rem.p = list(GoogleBTC = c(1,2), Bitcoin = c())
rem.q = c()
remove = list(p = rem.p , q = rem.q)
model2.c.test <- ardlDlm(lnReturnBitcoin.insample ~ Bitcoin + GoogleBTC, data = WeeklyData, p = 2, q = 1, remove = remove)
summary(model2.c.test)

new = data.frame(WeeklyData[c("lnReturnBitcoinsq","Bitcoin","GoogleBTC","lnReturnBitcoin.insample")])
new$lnReturnBitcoin.insample <- new$lnReturnBitcoinsq

new$predict.M1 <- predict(model1.c.test$model, new)
new$predict.M2 <- predict(model2.c.test$model, new)

new$MSE.M1 <- (new$lnReturnBitcoinsq-new$predict.M1)^2
mean(new$MSE.M1, na.rm=TRUE)

new$MSE.M2 <- (new$lnReturnBitcoinsq-new$predict.M2)^2
mean(new$MSE.M2, na.rm=TRUE)

t.test(new$MSE.M1, new$MSE.M2, paired=TRUE)
lsr::cohensD(new$MSE.M1, new$MSE.M2, method="paired")

boxplot(new$MSE.M1, new$MSE.M2, xlab="Model Weekly Bitcoin", ylab="Mean Square Error of prediction", names = c("Without Google","With Google"))

####Predict und Vergleich Eth Weekly####
model1.c.test <- ardlDlm(lnReturnEth.insample ~ Ethereum, data = WeeklyData, p = 2, q = 1)
summary(model1.c.test)

rem.p = list(GoogleBTC = c(1,2), Ethereum = c())
rem.q = c()
remove = list(p = rem.p , q = rem.q)
model2.c.test <- ardlDlm(lnReturnEth.insample ~ Ethereum + GoogleEth, data = WeeklyData, p = 2, q = 1, remove = remove)
summary(model2.c.test)

new = data.frame(WeeklyData[c("lnReturnEthsq","Ethereum","GoogleEth","lnReturnEth.insample")])
new$lnReturnEth.insample <- new$lnReturnEthsq

new$predict.M1 <- predict(model1.c.test$model, new)
new$predict.M2 <- predict(model2.c.test$model, new)

new$MSE.M1 <- (new$lnReturnEthsq-new$predict.M1)^2
mean(new$MSE.M1, na.rm=TRUE)

new$MSE.M2 <- (new$lnReturnEthsq-new$predict.M2)^2
mean(new$MSE.M2, na.rm=TRUE)

t.test(new$MSE.M1, new$MSE.M2, paired=TRUE)
lsr::cohensD(new$MSE.M1, new$MSE.M2, method="paired")

boxplot(new$MSE.M1, new$MSE.M2, xlab="Model Weekly Ethereum", ylab="Mean Square Error of prediction", names = c("Without Google","With Google"))
