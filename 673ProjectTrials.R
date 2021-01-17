setwd("C:/Users/sreer/OneDrive/Desktop/BAN673/BAN673Project")
getwd()

library(plotly)
library(ggplot2)
library(dplyr)
library(zoo)
library(forecast)


today <- Sys.Date()
tm <- seq(0, 600, by = 10)
x <- today - tm
y <- rnorm(length(x))
fig <- plot_ly(x = ~x, y = ~y, mode = 'lines', text = paste(tm, "days from today"))

fig



list.files()
trainDF <- read.csv("train_shortlisted.csv")
#keyDF <- read.csv("key_1.csv")
#@sampleSubmissionDF <- read.csv("sample_submission_1.csv")

length(trainDF)
#head(keyDF)
#head(sampleSubmissionDF)

#transposing rows to columns
#traindfTransposed <- data.frame(t(trainDF[-1]))
#colnames(traindfTransposed) <- trainDF[, 1]
#traindfTransposed
#data <- traindfTransposed #renaming the transposed data
#head(data)

#separate bus data
trainDFbus <- trainDF[1:5,]
trainDFbus$ï..Profession <-NULL
traindfTransposedbus <- data.frame(t(trainDFbus[-1]))
traindfTransposedbus
#traindfTransposedbus$`Elon_Musk_en.wikipedia.org_desktop_all-agents`
colnames(traindfTransposedbus) <- trainDFbus[, 1]
bus <- traindfTransposedbus


#separate movie data
trainDFmov <- trainDF[6:18,]
trainDFmov$ï..Profession <-NULL
traindfTransposedmov <- data.frame(t(trainDFmov[-1]))
colnames(traindfTransposedmov) <- trainDFmov[, 1]
mov <- traindfTransposedmov
mov

#separate political data
trainDFpoli <- trainDF[19:30,]
trainDFpoli$ï..Profession <-NULL
traindfTransposedpoli <- data.frame(t(trainDFpoli[-1]))
colnames(traindfTransposedpoli) <- trainDFpoli[, 1]
poli <- traindfTransposedpoli
poli


#separate singer data
trainDFsing <- trainDF[31:37,]
trainDFsing$ï..Profession <-NULL
traindfTransposedsing <- data.frame(t(trainDFsing[-1]))
colnames(traindfTransposedsing) <- trainDFsing[, 1]
singer <- traindfTransposedsing
singer

#separate sport data
trainDFsport <- trainDF[38:43,]
trainDFsport$ï..Profession <-NULL
traindfTransposedsport <- data.frame(t(trainDFsport[-1]))
colnames(traindfTransposedsport) <- trainDFsport[, 1]
sport <- traindfTransposedsport
sport




inds <- seq(as.Date("2015-07-01"), as.Date("2016-12-31"), by = "day")
length(inds)
as.numeric(format(inds[1], "%j"))
as.numeric(format(inds[385], "%j"))


nValid <- 165
nTrain <- 385
length(bus$`Elon_Musk_en.wikipedia.org_desktop_all-agents`)


#ELON MUSK ForeCasting

elon.ts <- ts(bus$`Elon_Musk_en.wikipedia.org_desktop_all-agents`,start = c(2015, as.numeric(format(inds[1], "%j"))),frequency = 365)
plot(elon.ts, xlab = "Time", ylab = "Views on Wikipedia", main = "Elon Wikipedia Views Time Series")

train.ts <- window(elon.ts, start = c(2015,as.numeric(format(inds[1], "%j"))), end = c(2015, as.numeric(format(inds[1], "%j"))+nTrain -1))


valid.ts <- window(elon.ts, start = c(2015,as.numeric(format(inds[1], "%j"))+nTrain), end = c(2015, as.numeric(format(inds[1], "%j"))+nTrain + nValid))

train.ts
valid.ts
plot(valid.ts, xlab = "Time", ylab = "Views on Wikipedia", main = "Elon Wikipedia Views Time Series")

##Elon Case 1 stuff
trailing.ma.2 <- rollmean(train.ts, k = 2, align = "right")
trailing.ma.6 <- rollmean(train.ts, k = 6, align = "right")
trailing.ma.12 <- rollmean(train.ts, k = 12, align = "right")

trailing.ma.2
trailing.ma.6
trailing.ma.12

trailing.ma.2.prediction <- forecast(trailing.ma.2, h=nValid, level = 0)
trailing.ma.6.prediction <- forecast(trailing.ma.6, h=nValid, level = 0)
trailing.ma.12.prediction <- forecast(trailing.ma.12, h=nValid, level = 0)

trailing.ma.2.prediction
trailing.ma.6.prediction
trailing.ma.12.prediction



##Elon Case 2 Stuff
trend.lin.reg <- tslm(train.ts ~ trend)
summary(trend.lin.reg)
#ii
trend.quad.reg <- tslm(train.ts ~ trend + I(trend^2))
summary(trend.quad.reg)
#iii
trend.seas.reg <- tslm(train.ts ~ season)
summary(trend.seas.reg)
#iv
trend.seasandline.reg <- tslm(train.ts ~ trend + season)
summary(trend.seasandline.reg)
#v
trend.seasandquad.reg <- tslm(train.ts ~ trend + I(trend^2) + season)
summary(trend.seasandquad.reg)

trend.lin.reg.pred <- forecast(trend.lin.reg, h = nValid, level = 0)
trend.lin.reg.pred

trend.quad.reg.pred <- forecast(trend.quad.reg, h = nValid, level = 0)
trend.quad.reg.pred

trend.seas.reg.pred <- forecast(trend.seas.reg, h = nValid, level = 0)
trend.seas.reg.pred

trend.seasandline.reg.pred <- forecast(trend.seasandline.reg, h = nValid, level = 0)
trend.seasandline.reg.pred

trend.seasandquad.reg.pred <- forecast(trend.seasandquad.reg, h = nValid, level = 0)
trend.seasandquad.reg.pred


#Elon Musk Case 3 Stuff
plot(trend.seasandquad.reg.pred$residuals)
Acf(trend.seasandquad.reg.pred$residuals, lag.max = 17, 
    main = "Autocorrelation for Elon Wikipedia Views Training Residuals")
res.ar1 <- Arima(trend.seasandquad.reg$residuals, order = c(1,0,0))
summary(res.ar1)


Acf(res.ar1$residuals, lag.max = 17, 
    main = "Autocorrelation for Elon Wikipedia Views Training Residuals of Residuals")
res.ar1.pred <- forecast(res.ar1, h = nValid, level = 0)

valid.two.level.pred <- trend.seasandquad.reg.pred$mean + res.ar1.pred$mean
valid.df <- data.frame(valid.ts, trend.seasandquad.reg.pred$mean, 
                       res.ar1.pred$mean, valid.two.level.pred)
names(valid.df) <- c("Views", "Reg.Forecast", 
                     "AR(1)Forecast", "Combined.Forecast")
valid.df


#train.ar3 <- Arima(train.ts, order = c(1,1,1), seasonal = c(1,1,1))
#summary(train.ar3)
#train.ar3.pred <- forecast(train.ar3, h = nValid, level = 0)
#train.ar3.pred


auto.arima1 <- auto.arima(train.ts)
summary(auto.arima1)

auto.arima1.pred <- forecast(auto.arima1, h = nValid, level = 0)
auto.arima1.pred




round(accuracy((snaive(elon.ts))$fitted, elon.ts), 3)
round(accuracy(trailing.ma.2.prediction, elon.ts), 3)
round(accuracy(trailing.ma.6.prediction, elon.ts), 3)
round(accuracy(trailing.ma.12.prediction, elon.ts), 3)
round(accuracy(trend.lin.reg.pred$fitted, elon.ts), 3)
round(accuracy(trend.quad.reg.pred$fitted, elon.ts), 3)
round(accuracy(trend.seas.reg.pred$fitted, elon.ts), 3)
round(accuracy(trend.seasandline.reg.pred$fitted, elon.ts), 3)
round(accuracy(trend.seasandquad.reg.pred$fitted, elon.ts), 3)
round(accuracy(valid.two.level.pred, elon.ts), 3)
round(accuracy(trend.seasandquad.reg.pred$mean, elon.ts), 3)
#round(accuracy(train.ar3.pred, elon.ts), 3)
round(accuracy(auto.arima1.pred, elon.ts), 3)

#length(train.ts)
#length(valid.ts)
#length(trend.lin.reg.pred$mean)
#length(res.ar1.pred$mean)
#length(valid.two.level.pred)



#Jeff Bezos
jeff.ts <- ts(bus$`Jeff_Bezos_en.wikipedia.org_desktop_all-agents`,start = c(2015, as.numeric(format(inds[1], "%j"))),frequency = 365)
plot(jeff.ts, xlab = "Time", ylab = "Views on Wikipedia", main = "Jeff Bezos Wikipedia Views Time Series")

train.ts <- window(jeff.ts, start = c(2015,as.numeric(format(inds[1], "%j"))), end = c(2015, as.numeric(format(inds[1], "%j"))+nTrain -1))


valid.ts <- window(jeff.ts, start = c(2015,as.numeric(format(inds[1], "%j"))+nTrain), end = c(2015, as.numeric(format(inds[1], "%j"))+nTrain + nValid))

train.ts
valid.ts
plot(train.ts, xlab = "Time", ylab = "Views on Wikipedia", main = "Jeff Wikipedia Views Time Series")

##Jeff Case 1 stuff
trailing.ma.2 <- rollmean(train.ts, k = 2, align = "right")
trailing.ma.6 <- rollmean(train.ts, k = 6, align = "right")
trailing.ma.12 <- rollmean(train.ts, k = 12, align = "right")

trailing.ma.2
trailing.ma.6
trailing.ma.12

trailing.ma.2.prediction <- forecast(trailing.ma.2, h=nValid, level = 0)
trailing.ma.6.prediction <- forecast(trailing.ma.6, h=nValid, level = 0)
trailing.ma.12.prediction <- forecast(trailing.ma.12, h=nValid, level = 0)

trailing.ma.2.prediction
trailing.ma.6.prediction
trailing.ma.12.prediction



##Jeff Case 2 Stuff
trend.lin.reg <- tslm(train.ts ~ trend)
summary(trend.lin.reg)
#ii
trend.quad.reg <- tslm(train.ts ~ trend + I(trend^2))
summary(trend.quad.reg)
#iii
trend.seas.reg <- tslm(train.ts ~ season)
summary(trend.seas.reg)
#iv
trend.seasandline.reg <- tslm(train.ts ~ trend + season)
summary(trend.seasandline.reg)
#v
trend.seasandquad.reg <- tslm(train.ts ~ trend + I(trend^2) + season)
summary(trend.seasandquad.reg)

trend.lin.reg.pred <- forecast(trend.lin.reg, h = nValid, level = 0)
trend.lin.reg.pred

trend.quad.reg.pred <- forecast(trend.quad.reg, h = nValid, level = 0)
trend.quad.reg.pred

trend.seas.reg.pred <- forecast(trend.seas.reg, h = nValid, level = 0)
trend.seas.reg.pred

trend.seasandline.reg.pred <- forecast(trend.seasandline.reg, h = nValid, level = 0)
trend.seasandline.reg.pred

trend.seasandquad.reg.pred <- forecast(trend.seasandquad.reg, h = nValid, level = 0)
trend.seasandquad.reg.pred


#Jeff Case 3 Stuff
plot(trend.seasandquad.reg.pred$residuals)
Acf(trend.seasandquad.reg.pred$residuals, lag.max = 17, 
    main = "Autocorrelation for Jeff Wikipedia Views Training Residuals")
res.ar1 <- Arima(trend.seasandquad.reg$residuals, order = c(1,0,0))
summary(res.ar1)


Acf(res.ar1$residuals, lag.max = 17, 
    main = "Autocorrelation for Jeff Wikipedia Views Training Residuals of Residuals")
res.ar1.pred <- forecast(res.ar1, h = nValid, level = 0)

valid.two.level.pred <- trend.seasandquad.reg.pred$mean + res.ar1.pred$mean
valid.df <- data.frame(valid.ts, trend.seasandquad.reg.pred$mean, 
                       res.ar1.pred$mean, valid.two.level.pred)
names(valid.df) <- c("Views", "Reg.Forecast", 
                     "AR(1)Forecast", "Combined.Forecast")
valid.df


#train.ar3 <- Arima(train.ts, order = c(1,1,1), seasonal = c(1,1,1))
#summary(train.ar3)
#train.ar3.pred <- forecast(train.ar3, h = nValid, level = 0)
#train.ar3.pred


auto.arima1 <- auto.arima(train.ts)
summary(auto.arima1)

auto.arima1.pred <- forecast(auto.arima1, h = nValid, level = 0)
auto.arima1.pred




round(accuracy((snaive(jeff.ts))$fitted, jeff.ts), 3)
round(accuracy(trailing.ma.2.prediction, jeff.ts), 3)
round(accuracy(trailing.ma.6.prediction, jeff.ts), 3)
round(accuracy(trailing.ma.12.prediction, jeff.ts), 3)
round(accuracy(trend.lin.reg.pred$fitted, jeff.ts), 3)
round(accuracy(trend.quad.reg.pred$fitted, jeff.ts), 3)
round(accuracy(trend.seas.reg.pred$fitted, jeff.ts), 3)
round(accuracy(trend.seasandline.reg.pred$fitted, jeff.ts), 3)
round(accuracy(trend.seasandquad.reg.pred$fitted, jeff.ts), 3)
round(accuracy(valid.two.level.pred, jeff.ts), 3)
round(accuracy(trend.seasandquad.reg.pred$mean, jeff.ts), 3)
#round(accuracy(train.ar3.pred, jeff.ts), 3)
round(accuracy(auto.arima1.pred, jeff.ts), 3)


#Emma Stone

emma.ts <- ts(mov$`Emma_Stone_en.wikipedia.org_desktop_all-agents`,start = c(2015, as.numeric(format(inds[1], "%j"))),frequency = 365)
plot(emma.ts, xlab = "Time", ylab = "Views on Wikipedia", main = "Emma Stone Wikipedia Views Time Series")

train.ts <- window(emma.ts, start = c(2015,as.numeric(format(inds[1], "%j"))), end = c(2015, as.numeric(format(inds[1], "%j"))+nTrain -1))


valid.ts <- window(emma.ts, start = c(2015,as.numeric(format(inds[1], "%j"))+nTrain), end = c(2015, as.numeric(format(inds[1], "%j"))+nTrain + nValid))

train.ts
valid.ts
plot(train.ts, xlab = "Time", ylab = "Views on Wikipedia", main = "Emma Stone Wikipedia Views Time Series")

##Emma Stone Case 1 stuff
trailing.ma.2 <- rollmean(train.ts, k = 2, align = "right")
trailing.ma.6 <- rollmean(train.ts, k = 6, align = "right")
trailing.ma.12 <- rollmean(train.ts, k = 12, align = "right")

trailing.ma.2
trailing.ma.6
trailing.ma.12

trailing.ma.2.prediction <- forecast(trailing.ma.2, h=nValid, level = 0)
trailing.ma.6.prediction <- forecast(trailing.ma.6, h=nValid, level = 0)
trailing.ma.12.prediction <- forecast(trailing.ma.12, h=nValid, level = 0)

trailing.ma.2.prediction
trailing.ma.6.prediction
trailing.ma.12.prediction



##Emma Stone Case 2 Stuff
trend.lin.reg <- tslm(train.ts ~ trend)
summary(trend.lin.reg)
#ii
trend.quad.reg <- tslm(train.ts ~ trend + I(trend^2))
summary(trend.quad.reg)
#iii
trend.seas.reg <- tslm(train.ts ~ season)
summary(trend.seas.reg)
#iv
trend.seasandline.reg <- tslm(train.ts ~ trend + season)
summary(trend.seasandline.reg)
#v
trend.seasandquad.reg <- tslm(train.ts ~ trend + I(trend^2) + season)
summary(trend.seasandquad.reg)

trend.lin.reg.pred <- forecast(trend.lin.reg, h = nValid, level = 0)
trend.lin.reg.pred

trend.quad.reg.pred <- forecast(trend.quad.reg, h = nValid, level = 0)
trend.quad.reg.pred

trend.seas.reg.pred <- forecast(trend.seas.reg, h = nValid, level = 0)
trend.seas.reg.pred

trend.seasandline.reg.pred <- forecast(trend.seasandline.reg, h = nValid, level = 0)
trend.seasandline.reg.pred

trend.seasandquad.reg.pred <- forecast(trend.seasandquad.reg, h = nValid, level = 0)
trend.seasandquad.reg.pred


#Emma Stone Case 3 Stuff
plot(trend.seasandquad.reg.pred$residuals)
Acf(trend.seasandquad.reg.pred$residuals, lag.max = 17, 
    main = "Autocorrelation for Emma Stone Wikipedia Views Training Residuals")
res.ar1 <- Arima(trend.seasandquad.reg$residuals, order = c(1,0,0))
summary(res.ar1)


Acf(res.ar1$residuals, lag.max = 17, 
    main = "Autocorrelation for Emma Stone Wikipedia Views Training Residuals of Residuals")
res.ar1.pred <- forecast(res.ar1, h = nValid, level = 0)

valid.two.level.pred <- trend.seasandquad.reg.pred$mean + res.ar1.pred$mean
valid.df <- data.frame(valid.ts, trend.seasandquad.reg.pred$mean, 
                       res.ar1.pred$mean, valid.two.level.pred)
names(valid.df) <- c("Views", "Reg.Forecast", 
                     "AR(1)Forecast", "Combined.Forecast")
valid.df


#train.ar3 <- Arima(train.ts, order = c(1,1,1), seasonal = c(1,1,1))
#summary(train.ar3)
#train.ar3.pred <- forecast(train.ar3, h = nValid, level = 0)
#train.ar3.pred


auto.arima1 <- auto.arima(train.ts)
summary(auto.arima1)

auto.arima1.pred <- forecast(auto.arima1, h = nValid, level = 0)
auto.arima1.pred




round(accuracy((snaive(emma.ts))$fitted, emma.ts), 3)
round(accuracy(trailing.ma.2.prediction, emma.ts), 3)
round(accuracy(trailing.ma.6.prediction, emma.ts), 3)
round(accuracy(trailing.ma.12.prediction, emma.ts), 3)
round(accuracy(trend.lin.reg.pred$fitted, emma.ts), 3)
round(accuracy(trend.quad.reg.pred$fitted, emma.ts), 3)
round(accuracy(trend.seas.reg.pred$fitted, emma.ts), 3)
round(accuracy(trend.seasandline.reg.pred$fitted, emma.ts), 3)
round(accuracy(trend.seasandquad.reg.pred$fitted, emma.ts), 3)
round(accuracy(valid.two.level.pred, emma.ts), 3)
round(accuracy(trend.seasandquad.reg.pred$mean, emma.ts), 3)
round(accuracy(auto.arima1.pred, emma.ts), 3)

##Leonardo DiCaprio
leo.ts <- ts(mov$`Leonardo_DiCaprio_en.wikipedia.org_desktop_all-agents`,start = c(2015, as.numeric(format(inds[1], "%j"))),frequency = 365)
plot(leo.ts, xlab = "Time", ylab = "Views on Wikipedia", main = "Leonardo DiCaprio Wikipedia Views Time Series")

train.ts <- window(leo.ts, start = c(2015,as.numeric(format(inds[1], "%j"))), end = c(2015, as.numeric(format(inds[1], "%j"))+nTrain -1))


valid.ts <- window(leo.ts, start = c(2015,as.numeric(format(inds[1], "%j"))+nTrain), end = c(2015, as.numeric(format(inds[1], "%j"))+nTrain + nValid))

train.ts
valid.ts
plot(train.ts, xlab = "Time", ylab = "Views on Wikipedia", main = "Leonardo DiCaprio Wikipedia Views Time Series")

##Leo Case 1 stuff
trailing.ma.2 <- rollmean(train.ts, k = 2, align = "right")
trailing.ma.6 <- rollmean(train.ts, k = 6, align = "right")
trailing.ma.12 <- rollmean(train.ts, k = 12, align = "right")

trailing.ma.2
trailing.ma.6
trailing.ma.12

trailing.ma.2.prediction <- forecast(trailing.ma.2, h=nValid, level = 0)
trailing.ma.6.prediction <- forecast(trailing.ma.6, h=nValid, level = 0)
trailing.ma.12.prediction <- forecast(trailing.ma.12, h=nValid, level = 0)

trailing.ma.2.prediction
trailing.ma.6.prediction
trailing.ma.12.prediction



##Leo Case 2 Stuff
trend.lin.reg <- tslm(train.ts ~ trend)
summary(trend.lin.reg)
#ii
trend.quad.reg <- tslm(train.ts ~ trend + I(trend^2))
summary(trend.quad.reg)
#iii
trend.seas.reg <- tslm(train.ts ~ season)
summary(trend.seas.reg)
#iv
trend.seasandline.reg <- tslm(train.ts ~ trend + season)
summary(trend.seasandline.reg)
#v
trend.seasandquad.reg <- tslm(train.ts ~ trend + I(trend^2) + season)
summary(trend.seasandquad.reg)

trend.lin.reg.pred <- forecast(trend.lin.reg, h = nValid, level = 0)
trend.lin.reg.pred

trend.quad.reg.pred <- forecast(trend.quad.reg, h = nValid, level = 0)
trend.quad.reg.pred

trend.seas.reg.pred <- forecast(trend.seas.reg, h = nValid, level = 0)
trend.seas.reg.pred

trend.seasandline.reg.pred <- forecast(trend.seasandline.reg, h = nValid, level = 0)
trend.seasandline.reg.pred

trend.seasandquad.reg.pred <- forecast(trend.seasandquad.reg, h = nValid, level = 0)
trend.seasandquad.reg.pred


#Leo Case 3 Stuff
plot(trend.seasandquad.reg.pred$residuals)
Acf(trend.seasandquad.reg.pred$residuals, lag.max = 17, 
    main = "Autocorrelation for Leonardo DiCaprio Wikipedia Views Training Residuals")
res.ar1 <- Arima(trend.seasandquad.reg$residuals, order = c(1,0,0))
summary(res.ar1)


Acf(res.ar1$residuals, lag.max = 17, 
    main = "Autocorrelation for Leonardo DiCaprio Wikipedia Views Training Residuals of Residuals")
res.ar1.pred <- forecast(res.ar1, h = nValid, level = 0)

valid.two.level.pred <- trend.seasandquad.reg.pred$mean + res.ar1.pred$mean
valid.df <- data.frame(valid.ts, trend.seasandquad.reg.pred$mean, 
                       res.ar1.pred$mean, valid.two.level.pred)
names(valid.df) <- c("Views", "Reg.Forecast", 
                     "AR(1)Forecast", "Combined.Forecast")
valid.df


#train.ar3 <- Arima(train.ts, order = c(1,1,1), seasonal = c(1,1,1))
#summary(train.ar3)
#train.ar3.pred <- forecast(train.ar3, h = nValid, level = 0)
#train.ar3.pred


auto.arima1 <- auto.arima(train.ts)
summary(auto.arima1)

auto.arima1.pred <- forecast(auto.arima1, h = nValid, level = 0)
auto.arima1.pred




round(accuracy((snaive(leo.ts))$fitted, leo.ts), 3)
round(accuracy(trailing.ma.2.prediction, leo.ts), 3)
round(accuracy(trailing.ma.6.prediction, leo.ts), 3)
round(accuracy(trailing.ma.12.prediction, leo.ts), 3)
round(accuracy(trend.lin.reg.pred$fitted, leo.ts), 3)
round(accuracy(trend.quad.reg.pred$fitted, leo.ts), 3)
round(accuracy(trend.seas.reg.pred$fitted, leo.ts), 3)
round(accuracy(trend.seasandline.reg.pred$fitted, leo.ts), 3)
round(accuracy(trend.seasandquad.reg.pred$fitted, leo.ts), 3)
round(accuracy(valid.two.level.pred, leo.ts), 3)
round(accuracy(trend.seasandquad.reg.pred$mean, leo.ts), 3)
round(accuracy(auto.arima1.pred, leo.ts), 3)



##Barack Obama
obama.ts <- ts(poli$`Barack_Obama_en.wikipedia.org_desktop_all-agents`,start = c(2015, as.numeric(format(inds[1], "%j"))),frequency = 365)
plot(obama.ts, xlab = "Time", ylab = "Views on Wikipedia", main = "Barack Oba,a Wikipedia Views Time Series")

train.ts <- window(obama.ts, start = c(2015,as.numeric(format(inds[1], "%j"))), end = c(2015, as.numeric(format(inds[1], "%j"))+nTrain -1))


valid.ts <- window(obama.ts, start = c(2015,as.numeric(format(inds[1], "%j"))+nTrain), end = c(2015, as.numeric(format(inds[1], "%j"))+nTrain + nValid))

train.ts
valid.ts
plot(train.ts, xlab = "Time", ylab = "Views on Wikipedia", main = "Barack Obama Wikipedia Views Time Series")

##Obama Case 1 stuff
trailing.ma.2 <- rollmean(train.ts, k = 2, align = "right")
trailing.ma.6 <- rollmean(train.ts, k = 6, align = "right")
trailing.ma.12 <- rollmean(train.ts, k = 12, align = "right")

trailing.ma.2
trailing.ma.6
trailing.ma.12

trailing.ma.2.prediction <- forecast(trailing.ma.2, h=nValid, level = 0)
trailing.ma.6.prediction <- forecast(trailing.ma.6, h=nValid, level = 0)
trailing.ma.12.prediction <- forecast(trailing.ma.12, h=nValid, level = 0)

trailing.ma.2.prediction
trailing.ma.6.prediction
trailing.ma.12.prediction



##Obama Case 2 Stuff
trend.lin.reg <- tslm(train.ts ~ trend)
summary(trend.lin.reg)
#ii
trend.quad.reg <- tslm(train.ts ~ trend + I(trend^2))
summary(trend.quad.reg)
#iii
trend.seas.reg <- tslm(train.ts ~ season)
summary(trend.seas.reg)
#iv
trend.seasandline.reg <- tslm(train.ts ~ trend + season)
summary(trend.seasandline.reg)
#v
trend.seasandquad.reg <- tslm(train.ts ~ trend + I(trend^2) + season)
summary(trend.seasandquad.reg)

trend.lin.reg.pred <- forecast(trend.lin.reg, h = nValid, level = 0)
trend.lin.reg.pred

trend.quad.reg.pred <- forecast(trend.quad.reg, h = nValid, level = 0)
trend.quad.reg.pred

trend.seas.reg.pred <- forecast(trend.seas.reg, h = nValid, level = 0)
trend.seas.reg.pred

trend.seasandline.reg.pred <- forecast(trend.seasandline.reg, h = nValid, level = 0)
trend.seasandline.reg.pred

trend.seasandquad.reg.pred <- forecast(trend.seasandquad.reg, h = nValid, level = 0)
trend.seasandquad.reg.pred


#Barack Obama Case 3 Stuff
plot(trend.seasandquad.reg.pred$residuals)
Acf(trend.seasandquad.reg.pred$residuals, lag.max = 17, 
    main = "Autocorrelation for Barack Obama Wikipedia Views Training Residuals")
res.ar1 <- Arima(trend.seasandquad.reg$residuals, order = c(1,0,0))
summary(res.ar1)


Acf(res.ar1$residuals, lag.max = 17, 
    main = "Autocorrelation for Barack Obama Wikipedia Views Training Residuals of Residuals")
res.ar1.pred <- forecast(res.ar1, h = nValid, level = 0)

valid.two.level.pred <- trend.seasandquad.reg.pred$mean + res.ar1.pred$mean
valid.df <- data.frame(valid.ts, trend.seasandquad.reg.pred$mean, 
                       res.ar1.pred$mean, valid.two.level.pred)
names(valid.df) <- c("Views", "Reg.Forecast", 
                     "AR(1)Forecast", "Combined.Forecast")
valid.df


#train.ar3 <- Arima(train.ts, order = c(1,1,1), seasonal = c(1,1,1))
#summary(train.ar3)
#train.ar3.pred <- forecast(train.ar3, h = nValid, level = 0)
#train.ar3.pred


auto.arima1 <- auto.arima(train.ts)
summary(auto.arima1)

auto.arima1.pred <- forecast(auto.arima1, h = nValid, level = 0)
auto.arima1.pred




round(accuracy((snaive(obama.ts))$fitted, obama.ts), 3)
round(accuracy(trailing.ma.2.prediction, obama.ts), 3)
round(accuracy(trailing.ma.6.prediction, obama.ts), 3)
round(accuracy(trailing.ma.12.prediction, obama.ts), 3)
round(accuracy(trend.lin.reg.pred$fitted, obama.ts), 3)
round(accuracy(trend.quad.reg.pred$fitted, obama.ts), 3)
round(accuracy(trend.seas.reg.pred$fitted, obama.ts), 3)
round(accuracy(trend.seasandline.reg.pred$fitted, obama.ts), 3)
round(accuracy(trend.seasandquad.reg.pred$fitted, obama.ts), 3)
round(accuracy(valid.two.level.pred, obama.ts), 3)
round(accuracy(trend.seasandquad.reg.pred$mean, obama.ts), 3)
#round(accuracy(train.ar3.pred, obama.ts), 3)
round(accuracy(auto.arima1.pred, obama.ts), 3)


#Joe Biden
joe.ts <- ts(poli$`Joe_Biden_en.wikipedia.org_desktop_all-agents`,start = c(2015, as.numeric(format(inds[1], "%j"))),frequency = 365)
plot(joe.ts, xlab = "Time", ylab = "Views on Wikipedia", main = "Joe Biden Wikipedia Views Time Series")

train.ts <- window(joe.ts, start = c(2015,as.numeric(format(inds[1], "%j"))), end = c(2015, as.numeric(format(inds[1], "%j"))+nTrain -1))


valid.ts <- window(joe.ts, start = c(2015,as.numeric(format(inds[1], "%j"))+nTrain), end = c(2015, as.numeric(format(inds[1], "%j"))+nTrain + nValid))

train.ts
valid.ts
plot(train.ts, xlab = "Time", ylab = "Views on Wikipedia", main = "Joe Biden Wikipedia Views Time Series")

##Joe Biden Case 1 stuff
trailing.ma.2 <- rollmean(train.ts, k = 2, align = "right")
trailing.ma.6 <- rollmean(train.ts, k = 6, align = "right")
trailing.ma.12 <- rollmean(train.ts, k = 12, align = "right")

trailing.ma.2
trailing.ma.6
trailing.ma.12

trailing.ma.2.prediction <- forecast(trailing.ma.2, h=nValid, level = 0)
trailing.ma.6.prediction <- forecast(trailing.ma.6, h=nValid, level = 0)
trailing.ma.12.prediction <- forecast(trailing.ma.12, h=nValid, level = 0)

trailing.ma.2.prediction
trailing.ma.6.prediction
trailing.ma.12.prediction



##Joe Biden Case 2 Stuff
trend.lin.reg <- tslm(train.ts ~ trend)
summary(trend.lin.reg)
#ii
trend.quad.reg <- tslm(train.ts ~ trend + I(trend^2))
summary(trend.quad.reg)
#iii
trend.seas.reg <- tslm(train.ts ~ season)
summary(trend.seas.reg)
#iv
trend.seasandline.reg <- tslm(train.ts ~ trend + season)
summary(trend.seasandline.reg)
#v
trend.seasandquad.reg <- tslm(train.ts ~ trend + I(trend^2) + season)
summary(trend.seasandquad.reg)

trend.lin.reg.pred <- forecast(trend.lin.reg, h = nValid, level = 0)
trend.lin.reg.pred

trend.quad.reg.pred <- forecast(trend.quad.reg, h = nValid, level = 0)
trend.quad.reg.pred

trend.seas.reg.pred <- forecast(trend.seas.reg, h = nValid, level = 0)
trend.seas.reg.pred

trend.seasandline.reg.pred <- forecast(trend.seasandline.reg, h = nValid, level = 0)
trend.seasandline.reg.pred

trend.seasandquad.reg.pred <- forecast(trend.seasandquad.reg, h = nValid, level = 0)
trend.seasandquad.reg.pred


#Joe Biden Case 3 Stuff
plot(trend.seasandquad.reg.pred$residuals)
Acf(trend.seasandquad.reg.pred$residuals, lag.max = 17, 
    main = "Autocorrelation for Joe Biden Wikipedia Views Training Residuals")
res.ar1 <- Arima(trend.seasandquad.reg$residuals, order = c(1,0,0))
summary(res.ar1)


Acf(res.ar1$residuals, lag.max = 17, 
    main = "Autocorrelation for Joe Biden Wikipedia Views Training Residuals of Residuals")
res.ar1.pred <- forecast(res.ar1, h = nValid, level = 0)

valid.two.level.pred <- trend.seasandquad.reg.pred$mean + res.ar1.pred$mean
valid.df <- data.frame(valid.ts, trend.seasandquad.reg.pred$mean, 
                       res.ar1.pred$mean, valid.two.level.pred)
names(valid.df) <- c("Views", "Reg.Forecast", 
                     "AR(1)Forecast", "Combined.Forecast")
valid.df


#train.ar3 <- Arima(train.ts, order = c(1,1,1), seasonal = c(1,1,1))
#summary(train.ar3)
#train.ar3.pred <- forecast(train.ar3, h = nValid, level = 0)
#train.ar3.pred


auto.arima1 <- auto.arima(train.ts)
summary(auto.arima1)

auto.arima1.pred <- forecast(auto.arima1, h = nValid, level = 0)
auto.arima1.pred




round(accuracy((snaive(joe.ts))$fitted, joe.ts), 3)
round(accuracy(trailing.ma.2.prediction, joe.ts), 3)
round(accuracy(trailing.ma.6.prediction, joe.ts), 3)
round(accuracy(trailing.ma.12.prediction, joe.ts), 3)
round(accuracy(trend.lin.reg.pred$fitted, joe.ts), 3)
round(accuracy(trend.quad.reg.pred$fitted, joe.ts), 3)
round(accuracy(trend.seas.reg.pred$fitted, joe.ts), 3)
round(accuracy(trend.seasandline.reg.pred$fitted, joe.ts), 3)
round(accuracy(trend.seasandquad.reg.pred$fitted, joe.ts), 3)
round(accuracy(valid.two.level.pred, joe.ts), 3)
round(accuracy(trend.seasandquad.reg.pred$mean, joe.ts), 3)
#round(accuracy(train.ar3.pred, joe.ts), 3)
round(accuracy(auto.arima1.pred, joe.ts), 3)



#Kendrick Lamar
lamar.ts <- ts(singer$`Kendrick_Lamar_en.wikipedia.org_desktop_all-agents`,start = c(2015, as.numeric(format(inds[1], "%j"))),frequency = 365)
plot(lamar.ts, xlab = "Time", ylab = "Views on Wikipedia", main = "Kendrick Lamar Wikipedia Views Time Series")

train.ts <- window(lamar.ts, start = c(2015,as.numeric(format(inds[1], "%j"))), end = c(2015, as.numeric(format(inds[1], "%j"))+nTrain -1))


valid.ts <- window(lamar.ts, start = c(2015,as.numeric(format(inds[1], "%j"))+nTrain), end = c(2015, as.numeric(format(inds[1], "%j"))+nTrain + nValid))

train.ts
valid.ts
plot(train.ts, xlab = "Time", ylab = "Views on Wikipedia", main = "Kendrick Lamar Wikipedia Views Time Series")

##Kendrick Lamar Case 1 stuff
trailing.ma.2 <- rollmean(train.ts, k = 2, align = "right")
trailing.ma.6 <- rollmean(train.ts, k = 6, align = "right")
trailing.ma.12 <- rollmean(train.ts, k = 12, align = "right")

trailing.ma.2
trailing.ma.6
trailing.ma.12

trailing.ma.2.prediction <- forecast(trailing.ma.2, h=nValid, level = 0)
trailing.ma.6.prediction <- forecast(trailing.ma.6, h=nValid, level = 0)
trailing.ma.12.prediction <- forecast(trailing.ma.12, h=nValid, level = 0)

trailing.ma.2.prediction
trailing.ma.6.prediction
trailing.ma.12.prediction



##Kendrick Lamar Case 2 Stuff
trend.lin.reg <- tslm(train.ts ~ trend)
summary(trend.lin.reg)
#ii
trend.quad.reg <- tslm(train.ts ~ trend + I(trend^2))
summary(trend.quad.reg)
#iii
trend.seas.reg <- tslm(train.ts ~ season)
summary(trend.seas.reg)
#iv
trend.seasandline.reg <- tslm(train.ts ~ trend + season)
summary(trend.seasandline.reg)
#v
trend.seasandquad.reg <- tslm(train.ts ~ trend + I(trend^2) + season)
summary(trend.seasandquad.reg)

trend.lin.reg.pred <- forecast(trend.lin.reg, h = nValid, level = 0)
trend.lin.reg.pred

trend.quad.reg.pred <- forecast(trend.quad.reg, h = nValid, level = 0)
trend.quad.reg.pred

trend.seas.reg.pred <- forecast(trend.seas.reg, h = nValid, level = 0)
trend.seas.reg.pred

trend.seasandline.reg.pred <- forecast(trend.seasandline.reg, h = nValid, level = 0)
trend.seasandline.reg.pred

trend.seasandquad.reg.pred <- forecast(trend.seasandquad.reg, h = nValid, level = 0)
trend.seasandquad.reg.pred


#Kendrick Lamar Case 3 Stuff
plot(trend.seasandquad.reg.pred$residuals)
Acf(trend.seasandquad.reg.pred$residuals, lag.max = 17, 
    main = "Autocorrelation for Kendrick Lamar Wikipedia Views Training Residuals")
res.ar1 <- Arima(trend.seasandquad.reg$residuals, order = c(1,0,0))
summary(res.ar1)


Acf(res.ar1$residuals, lag.max = 17, 
    main = "Autocorrelation for Kendrick Lamar Wikipedia Views Training Residuals of Residuals")
res.ar1.pred <- forecast(res.ar1, h = nValid, level = 0)

valid.two.level.pred <- trend.seasandquad.reg.pred$mean + res.ar1.pred$mean
valid.df <- data.frame(valid.ts, trend.seasandquad.reg.pred$mean, 
                       res.ar1.pred$mean, valid.two.level.pred)
names(valid.df) <- c("Views", "Reg.Forecast", 
                     "AR(1)Forecast", "Combined.Forecast")
valid.df


#train.ar3 <- Arima(train.ts, order = c(1,1,1), seasonal = c(1,1,1))
#summary(train.ar3)
#train.ar3.pred <- forecast(train.ar3, h = nValid, level = 0)
#train.ar3.pred


auto.arima1 <- auto.arima(train.ts)
summary(auto.arima1)

auto.arima1.pred <- forecast(auto.arima1, h = nValid, level = 0)
auto.arima1.pred




round(accuracy((snaive(lamar.ts))$fitted, lamar.ts), 3)
round(accuracy(trailing.ma.2.prediction, lamar.ts), 3)
round(accuracy(trailing.ma.6.prediction, lamar.ts), 3)
round(accuracy(trailing.ma.12.prediction, lamar.ts), 3)
round(accuracy(trend.lin.reg.pred$fitted, lamar.ts), 3)
round(accuracy(trend.quad.reg.pred$fitted, lamar.ts), 3)
round(accuracy(trend.seas.reg.pred$fitted, lamar.ts), 3)
round(accuracy(trend.seasandline.reg.pred$fitted, lamar.ts), 3)
round(accuracy(trend.seasandquad.reg.pred$fitted, lamar.ts), 3)
round(accuracy(valid.two.level.pred, lamar.ts), 3)
round(accuracy(trend.seasandquad.reg.pred$mean, lamar.ts), 3)
round(accuracy(auto.arima1.pred, lamar.ts), 3)



#Bruno Mars
bruno.ts <- ts(singer$`Bruno_Mars_en.wikipedia.org_desktop_all-agents`,start = c(2015, as.numeric(format(inds[1], "%j"))),frequency = 365)
plot(bruno.ts, xlab = "Time", ylab = "Views on Wikipedia", main = "Bruno Mars Wikipedia Views Time Series")

train.ts <- window(bruno.ts, start = c(2015,as.numeric(format(inds[1], "%j"))), end = c(2015, as.numeric(format(inds[1], "%j"))+nTrain -1))


valid.ts <- window(bruno.ts, start = c(2015,as.numeric(format(inds[1], "%j"))+nTrain), end = c(2015, as.numeric(format(inds[1], "%j"))+nTrain + nValid))

train.ts
valid.ts
plot(train.ts, xlab = "Time", ylab = "Views on Wikipedia", main = "Bruno Mars Wikipedia Views Time Series")

##Bruno Mars Case 1 stuff
trailing.ma.2 <- rollmean(train.ts, k = 2, align = "right")
trailing.ma.6 <- rollmean(train.ts, k = 6, align = "right")
trailing.ma.12 <- rollmean(train.ts, k = 12, align = "right")

trailing.ma.2
trailing.ma.6
trailing.ma.12

trailing.ma.2.prediction <- forecast(trailing.ma.2, h=nValid, level = 0)
trailing.ma.6.prediction <- forecast(trailing.ma.6, h=nValid, level = 0)
trailing.ma.12.prediction <- forecast(trailing.ma.12, h=nValid, level = 0)

trailing.ma.2.prediction
trailing.ma.6.prediction
trailing.ma.12.prediction



##Bruno Mars Case 2 Stuff
trend.lin.reg <- tslm(train.ts ~ trend)
summary(trend.lin.reg)
#ii
trend.quad.reg <- tslm(train.ts ~ trend + I(trend^2))
summary(trend.quad.reg)
#iii
trend.seas.reg <- tslm(train.ts ~ season)
summary(trend.seas.reg)
#iv
trend.seasandline.reg <- tslm(train.ts ~ trend + season)
summary(trend.seasandline.reg)
#v
trend.seasandquad.reg <- tslm(train.ts ~ trend + I(trend^2) + season)
summary(trend.seasandquad.reg)

trend.lin.reg.pred <- forecast(trend.lin.reg, h = nValid, level = 0)
trend.lin.reg.pred

trend.quad.reg.pred <- forecast(trend.quad.reg, h = nValid, level = 0)
trend.quad.reg.pred

trend.seas.reg.pred <- forecast(trend.seas.reg, h = nValid, level = 0)
trend.seas.reg.pred

trend.seasandline.reg.pred <- forecast(trend.seasandline.reg, h = nValid, level = 0)
trend.seasandline.reg.pred

trend.seasandquad.reg.pred <- forecast(trend.seasandquad.reg, h = nValid, level = 0)
trend.seasandquad.reg.pred


#Bruno Mars Case 3 Stuff
plot(trend.seasandquad.reg.pred$residuals)
Acf(trend.seasandquad.reg.pred$residuals, lag.max = 17, 
    main = "Autocorrelation for Bruno Mars Wikipedia Views Training Residuals")
res.ar1 <- Arima(trend.seasandquad.reg$residuals, order = c(1,0,0))
summary(res.ar1)


Acf(res.ar1$residuals, lag.max = 17, 
    main = "Autocorrelation for Bruno Mars Wikipedia Views Training Residuals of Residuals")
res.ar1.pred <- forecast(res.ar1, h = nValid, level = 0)

valid.two.level.pred <- trend.seasandquad.reg.pred$mean + res.ar1.pred$mean
valid.df <- data.frame(valid.ts, trend.seasandquad.reg.pred$mean, 
                       res.ar1.pred$mean, valid.two.level.pred)
names(valid.df) <- c("Views", "Reg.Forecast", 
                     "AR(1)Forecast", "Combined.Forecast")
valid.df


#train.ar3 <- Arima(train.ts, order = c(1,1,1), seasonal = c(1,1,1))
#summary(train.ar3)
#train.ar3.pred <- forecast(train.ar3, h = nValid, level = 0)
#train.ar3.pred


auto.arima1 <- auto.arima(train.ts)
summary(auto.arima1)

auto.arima1.pred <- forecast(auto.arima1, h = nValid, level = 0)
auto.arima1.pred




round(accuracy((snaive(bruno.ts))$fitted, bruno.ts), 3)
round(accuracy(trailing.ma.2.prediction, bruno.ts), 3)
round(accuracy(trailing.ma.6.prediction, bruno.ts), 3)
round(accuracy(trailing.ma.12.prediction, bruno.ts), 3)
round(accuracy(trend.lin.reg.pred$fitted, bruno.ts), 3)
round(accuracy(trend.quad.reg.pred$fitted, bruno.ts), 3)
round(accuracy(trend.seas.reg.pred$fitted, bruno.ts), 3)
round(accuracy(trend.seasandline.reg.pred$fitted, bruno.ts), 3)
round(accuracy(trend.seasandquad.reg.pred$fitted, bruno.ts), 3)
round(accuracy(valid.two.level.pred, bruno.ts), 3)
round(accuracy(trend.seasandquad.reg.pred$mean, bruno.ts), 3)
round(accuracy(auto.arima1.pred, bruno.ts), 3)



#Michael Jordan
jordan.ts <- ts(sport$`Michael_Jordan_en.wikipedia.org_desktop_all-agents`,start = c(2015, as.numeric(format(inds[1], "%j"))),frequency = 365)
plot(jordan.ts, xlab = "Time", ylab = "Views on Wikipedia", main = "Michael Jordan Wikipedia Views Time Series")

train.ts <- window(jordan.ts, start = c(2015,as.numeric(format(inds[1], "%j"))), end = c(2015, as.numeric(format(inds[1], "%j"))+nTrain -1))


valid.ts <- window(jordan.ts, start = c(2015,as.numeric(format(inds[1], "%j"))+nTrain), end = c(2015, as.numeric(format(inds[1], "%j"))+nTrain + nValid))

train.ts
valid.ts
plot(train.ts, xlab = "Time", ylab = "Views on Wikipedia", main = "Michael Jordan Wikipedia Views Time Series")

##Michael jordan Case 1 stuff
trailing.ma.2 <- rollmean(train.ts, k = 2, align = "right")
trailing.ma.6 <- rollmean(train.ts, k = 6, align = "right")
trailing.ma.12 <- rollmean(train.ts, k = 12, align = "right")

trailing.ma.2
trailing.ma.6
trailing.ma.12

trailing.ma.2.prediction <- forecast(trailing.ma.2, h=nValid, level = 0)
trailing.ma.6.prediction <- forecast(trailing.ma.6, h=nValid, level = 0)
trailing.ma.12.prediction <- forecast(trailing.ma.12, h=nValid, level = 0)

trailing.ma.2.prediction
trailing.ma.6.prediction
trailing.ma.12.prediction



##Jordan Case 2 Stuff
trend.lin.reg <- tslm(train.ts ~ trend)
summary(trend.lin.reg)
#ii
trend.quad.reg <- tslm(train.ts ~ trend + I(trend^2))
summary(trend.quad.reg)
#iii
trend.seas.reg <- tslm(train.ts ~ season)
summary(trend.seas.reg)
#iv
trend.seasandline.reg <- tslm(train.ts ~ trend + season)
summary(trend.seasandline.reg)
#v
trend.seasandquad.reg <- tslm(train.ts ~ trend + I(trend^2) + season)
summary(trend.seasandquad.reg)

trend.lin.reg.pred <- forecast(trend.lin.reg, h = nValid, level = 0)
trend.lin.reg.pred

trend.quad.reg.pred <- forecast(trend.quad.reg, h = nValid, level = 0)
trend.quad.reg.pred

trend.seas.reg.pred <- forecast(trend.seas.reg, h = nValid, level = 0)
trend.seas.reg.pred

trend.seasandline.reg.pred <- forecast(trend.seasandline.reg, h = nValid, level = 0)
trend.seasandline.reg.pred

trend.seasandquad.reg.pred <- forecast(trend.seasandquad.reg, h = nValid, level = 0)
trend.seasandquad.reg.pred


#jordan Case 3 Stuff
plot(trend.seasandquad.reg.pred$residuals)
Acf(trend.seasandquad.reg.pred$residuals, lag.max = 17, 
    main = "Autocorrelation for Michael Jordan Wikipedia Views Training Residuals")
res.ar1 <- Arima(trend.seasandquad.reg$residuals, order = c(1,0,0))
summary(res.ar1)


Acf(res.ar1$residuals, lag.max = 17, 
    main = "Autocorrelation for Michael Jordan Wikipedia Views Training Residuals of Residuals")
res.ar1.pred <- forecast(res.ar1, h = nValid, level = 0)

valid.two.level.pred <- trend.seasandquad.reg.pred$mean + res.ar1.pred$mean
valid.df <- data.frame(valid.ts, trend.seasandquad.reg.pred$mean, 
                       res.ar1.pred$mean, valid.two.level.pred)
names(valid.df) <- c("Views", "Reg.Forecast", 
                     "AR(1)Forecast", "Combined.Forecast")
valid.df


#train.ar3 <- Arima(train.ts, order = c(1,1,1), seasonal = c(1,1,1))
#summary(train.ar3)
#train.ar3.pred <- forecast(train.ar3, h = nValid, level = 0)
#train.ar3.pred


auto.arima1 <- auto.arima(train.ts)
summary(auto.arima1)

auto.arima1.pred <- forecast(auto.arima1, h = nValid, level = 0)
auto.arima1.pred




round(accuracy((snaive(jordan.ts))$fitted, jordan.ts), 3)
round(accuracy(trailing.ma.2.prediction, jordan.ts), 3)
round(accuracy(trailing.ma.6.prediction, jordan.ts), 3)
round(accuracy(trailing.ma.12.prediction, jordan.ts), 3)
round(accuracy(trend.lin.reg.pred$fitted, jordan.ts), 3)
round(accuracy(trend.quad.reg.pred$fitted, jordan.ts), 3)
round(accuracy(trend.seas.reg.pred$fitted, jordan.ts), 3)
round(accuracy(trend.seasandline.reg.pred$fitted, jordan.ts), 3)
round(accuracy(trend.seasandquad.reg.pred$fitted, jordan.ts), 3)
round(accuracy(valid.two.level.pred, jordan.ts), 3)
round(accuracy(trend.seasandquad.reg.pred$mean, jordan.ts), 3)
#round(accuracy(train.ar3.pred, jordan.ts), 3)
round(accuracy(auto.arima1.pred, jordan.ts), 3)



#Maria Sharapova
maria.ts <- ts(sport$`Maria_Sharapova_en.wikipedia.org_desktop_all-agents`,start = c(2015, as.numeric(format(inds[1], "%j"))),frequency = 365)
plot(maria.ts, xlab = "Time", ylab = "Views on Wikipedia", main = "Maria Sharapova Wikipedia Views Time Series")

train.ts <- window(maria.ts, start = c(2015,as.numeric(format(inds[1], "%j"))), end = c(2015, as.numeric(format(inds[1], "%j"))+nTrain -1))


valid.ts <- window(maria.ts, start = c(2015,as.numeric(format(inds[1], "%j"))+nTrain), end = c(2015, as.numeric(format(inds[1], "%j"))+nTrain + nValid))

train.ts
valid.ts
plot(train.ts, xlab = "Time", ylab = "Views on Wikipedia", main = "Maria Sharapova Wikipedia Views Time Series")

##Sharapova Case 1 stuff
trailing.ma.2 <- rollmean(train.ts, k = 2, align = "right")
trailing.ma.6 <- rollmean(train.ts, k = 6, align = "right")
trailing.ma.12 <- rollmean(train.ts, k = 12, align = "right")

trailing.ma.2
trailing.ma.6
trailing.ma.12

trailing.ma.2.prediction <- forecast(trailing.ma.2, h=nValid, level = 0)
trailing.ma.6.prediction <- forecast(trailing.ma.6, h=nValid, level = 0)
trailing.ma.12.prediction <- forecast(trailing.ma.12, h=nValid, level = 0)

trailing.ma.2.prediction
trailing.ma.6.prediction
trailing.ma.12.prediction



##Sharapova Case 2 Stuff
trend.lin.reg <- tslm(train.ts ~ trend)
summary(trend.lin.reg)
#ii
trend.quad.reg <- tslm(train.ts ~ trend + I(trend^2))
summary(trend.quad.reg)
#iii
trend.seas.reg <- tslm(train.ts ~ season)
summary(trend.seas.reg)
#iv
trend.seasandline.reg <- tslm(train.ts ~ trend + season)
summary(trend.seasandline.reg)
#v
trend.seasandquad.reg <- tslm(train.ts ~ trend + I(trend^2) + season)
summary(trend.seasandquad.reg)

trend.lin.reg.pred <- forecast(trend.lin.reg, h = nValid, level = 0)
trend.lin.reg.pred

trend.quad.reg.pred <- forecast(trend.quad.reg, h = nValid, level = 0)
trend.quad.reg.pred

trend.seas.reg.pred <- forecast(trend.seas.reg, h = nValid, level = 0)
trend.seas.reg.pred

trend.seasandline.reg.pred <- forecast(trend.seasandline.reg, h = nValid, level = 0)
trend.seasandline.reg.pred

trend.seasandquad.reg.pred <- forecast(trend.seasandquad.reg, h = nValid, level = 0)
trend.seasandquad.reg.pred


##Sharapova Case 3 Stuff
plot(trend.seasandquad.reg.pred$residuals)
Acf(trend.seasandquad.reg.pred$residuals, lag.max = 17, 
    main = "Autocorrelation for Maria Sharapova Wikipedia Views Training Residuals")
res.ar1 <- Arima(trend.seasandquad.reg$residuals, order = c(1,0,0))
summary(res.ar1)


Acf(res.ar1$residuals, lag.max = 17, 
    main = "Autocorrelation for Maria Sharapova Wikipedia Views Training Residuals of Residuals")
res.ar1.pred <- forecast(res.ar1, h = nValid, level = 0)

valid.two.level.pred <- trend.seasandquad.reg.pred$mean + res.ar1.pred$mean
valid.df <- data.frame(valid.ts, trend.seasandquad.reg.pred$mean, 
                       res.ar1.pred$mean, valid.two.level.pred)
names(valid.df) <- c("Views", "Reg.Forecast", 
                     "AR(1)Forecast", "Combined.Forecast")
valid.df


#train.ar3 <- Arima(train.ts, order = c(1,1,1), seasonal = c(1,1,1))
#summary(train.ar3)
#train.ar3.pred <- forecast(train.ar3, h = nValid, level = 0)
#train.ar3.pred


auto.arima1 <- auto.arima(train.ts)
summary(auto.arima1)

auto.arima1.pred <- forecast(auto.arima1, h = nValid, level = 0)
auto.arima1.pred




round(accuracy((snaive(maria.ts))$fitted, maria.ts), 3)
round(accuracy(trailing.ma.2.prediction, maria.ts), 3)
round(accuracy(trailing.ma.6.prediction, maria.ts), 3)
round(accuracy(trailing.ma.12.prediction, maria.ts), 3)
round(accuracy(trend.lin.reg.pred$fitted, maria.ts), 3)
round(accuracy(trend.quad.reg.pred$fitted, maria.ts), 3)
round(accuracy(trend.seas.reg.pred$fitted, maria.ts), 3)
round(accuracy(trend.seasandline.reg.pred$fitted, maria.ts), 3)
round(accuracy(trend.seasandquad.reg.pred$fitted, maria.ts), 3)
round(accuracy(valid.two.level.pred, maria.ts), 3)
round(accuracy(trend.seasandquad.reg.pred$mean, maria.ts), 3)
#round(accuracy(train.ar3.pred, maria.ts), 3)
round(accuracy(auto.arima1.pred, maria.ts), 3)



##Analysis 
futureDF <- read.csv("Future_2017.csv")
futurefTransposed <- data.frame(t(futureDF[-1]))
colnames(futurefTransposed) <- futureDF[, 1]
future <- futurefTransposed #renaming the transposed data
head(future)
#length(future)
#length(futureDF)
#ncol(future)
#dim(future)
future
inds2 <- seq(as.Date("2017-01-01"), as.Date("2017-09-10"), by = "day")
length(inds2)
as.numeric(format(inds2[1], "%j"))

#Elon Musk Future ANalysis
elon.ts <- ts(bus$`Elon_Musk_en.wikipedia.org_desktop_all-agents`,start = c(2015, as.numeric(format(inds[1], "%j"))),frequency = 365)
futureelon.ts <-ts(future$`Elon_Musk_en.wikipedia.org_desktop_all-agents`,start = c(2017, as.numeric(format(inds2[1], "%j"))),frequency = 365)
plot(futureelon.ts, xlab = "Time", ylab = "Views on Wikipedia", main = "Elon Musk Future Wikipedia Views Time Series")

#seasonal
elon.seas.reg <- tslm(elon.ts ~ season)
summary(elon.seas.reg)

elon.seas.reg.pred <- forecast(elon.seas.reg, h = 253, level = 0)
elon.seas.reg.pred
plot(elon.seas.reg.pred, xlab = "Time", ylab = "Views on Wikipedia", main = "Elon Musk Future Wikipedia Views Time Series Forecasted")

#season and line
elon.seasandline.reg <- tslm( elon.ts ~ trend + season)
summary(elon.seasandline.reg)

elon.seasandline.reg.pred <- forecast(elon.seasandline.reg, h = 253, level = 0)
elon.seasandline.reg.pred
plot(elon.seasandline.reg.pred, xlab = "Time", ylab = "Views on Wikipedia", main = "Elon Musk Future Wikipedia Views Time Series Forecasted")

#seasonal and quadratic
elon.seasandquad.reg <- tslm( elon.ts ~ trend + I(trend^2) + season)
summary(elon.seasandquad.reg)

elon.seasandquad.reg.pred <- forecast(elon.seasandquad.reg, h = 253, level = 0)
elon.seasandquad.reg.pred
plot(elon.seasandquad.reg.pred, xlab = "Time", ylab = "Views on Wikipedia", main = "Elon Musk Future Wikipedia Views Time Series Forecasted")

round(accuracy(elon.seas.reg.pred$fitted, futureelon.ts), 3)
round(accuracy(elon.seasandline.reg.pred$fitted, futureelon.ts), 3)
round(accuracy(elon.seasandquad.reg.pred$fitted, futureelon.ts), 3)


##Jeff Bezos Future ANalysis
jeff.ts <- ts(bus$`Jeff_Bezos_en.wikipedia.org_desktop_all-agents`,start = c(2015, as.numeric(format(inds[1], "%j"))),frequency = 365)
futurejeff.ts <-ts(future$`Jeff_Bezos_en.wikipedia.org_desktop_all-agents`,start = c(2017, as.numeric(format(inds2[1], "%j"))),frequency = 365)
plot(futurejeff.ts, xlab = "Time", ylab = "Views on Wikipedia", main = "Jeff Bezos Future Wikipedia Views Time Series")

#seasonal
jeff.seas.reg <- tslm(jeff.ts ~ season)
summary(jeff.seas.reg)

jeff.seas.reg.pred <- forecast(jeff.seas.reg, h = 253, level = 0)
jeff.seas.reg.pred
plot(jeff.seas.reg.pred, xlab = "Time", ylab = "Views on Wikipedia", main = "Jeff Bezos Future Wikipedia Views Time Series Forecasted")

#season and line
jeff.seasandline.reg <- tslm( jeff.ts ~ trend + season)
summary(jeff.seasandline.reg)

jeff.seasandline.reg.pred <- forecast(jeff.seasandline.reg, h = 253, level = 0)
jeff.seasandline.reg.pred
plot(jeff.seasandline.reg.pred, xlab = "Time", ylab = "Views on Wikipedia", main = "Jeff Bezos Future Wikipedia Views Time Series Forecasted")

jeff.seasandquad.reg <- tslm( jeff.ts ~ trend + I(trend^2) + season)
summary(jeff.seasandquad.reg)

jeff.seasandquad.reg.pred <- forecast(jeff.seasandquad.reg, h = 253, level = 0)
jeff.seasandquad.reg.pred
plot(jeff.seasandquad.reg.pred, xlab = "Time", ylab = "Views on Wikipedia", main = "Jeff Bezos Future Wikipedia Views Time Series Forecasted")

round(accuracy(jeff.seas.reg.pred$fitted, futurejeff.ts), 3)
round(accuracy(jeff.seasandline.reg.pred$fitted, futurejeff.ts), 3)
round(accuracy(jeff.seasandquad.reg.pred$fitted, futurejeff.ts), 3)


##Emma Stone Future ANalysis
emma.ts <- ts(mov$`Emma_Stone_en.wikipedia.org_desktop_all-agents`,start = c(2015, as.numeric(format(inds[1], "%j"))),frequency = 365)
futureemma.ts <-ts(future$`Emma_Stone_en.wikipedia.org_desktop_all-agents`,start = c(2017, as.numeric(format(inds2[1], "%j"))),frequency = 365)
plot(futureemma.ts, xlab = "Time", ylab = "Views on Wikipedia", main = "Emma Stone Future Wikipedia Views Time Series")

#seasonal
emma.seas.reg <- tslm(emma.ts ~ season)
summary(emma.seas.reg)

emma.seas.reg.pred <- forecast(emma.seas.reg, h = 253, level = 0)
emma.seas.reg.pred
plot(emma.seas.reg.pred, xlab = "Time", ylab = "Views on Wikipedia", main = "Emma Stone Future Wikipedia Views Time Series Forecasted")

#season and line
emma.seasandline.reg <- tslm( emma.ts ~ trend + season)
summary(emma.seasandline.reg)

emma.seasandline.reg.pred <- forecast(emma.seasandline.reg, h = 253, level = 0)
emma.seasandline.reg.pred
plot(emma.seasandline.reg.pred, xlab = "Time", ylab = "Views on Wikipedia", main = "Emma Stone Future Wikipedia Views Time Series Forecasted")

emma.seasandquad.reg <- tslm( emma.ts ~ trend + I(trend^2) + season)
summary(emma.seasandquad.reg)

emma.seasandquad.reg.pred <- forecast(emma.seasandquad.reg, h = 253, level = 0)
emma.seasandquad.reg.pred
plot(emma.seasandquad.reg.pred, xlab = "Time", ylab = "Views on Wikipedia", main = "Emma Stone Future Wikipedia Views Time Series Forecasted")

round(accuracy(emma.seas.reg.pred$fitted, futureemma.ts), 3)
round(accuracy(emma.seasandline.reg.pred$fitted, futureemma.ts), 3)
round(accuracy(emma.seasandquad.reg.pred$fitted, futureemma.ts), 3)


##Leonardo DiCaprio Future ANalysis
leo.ts <- ts(mov$`Leonardo_DiCaprio_en.wikipedia.org_desktop_all-agents`,start = c(2015, as.numeric(format(inds[1], "%j"))),frequency = 365)
futureleo.ts <-ts(future$`Leonardo_DiCaprio_en.wikipedia.org_desktop_all-agents`,start = c(2017, as.numeric(format(inds2[1], "%j"))),frequency = 365)
plot(futureleo.ts, xlab = "Time", ylab = "Views on Wikipedia", main = "Leonardo DiCaprio Future Wikipedia Views Time Series")

#seasonal
leo.seas.reg <- tslm(leo.ts ~ season)
summary(leo.seas.reg)

leo.seas.reg.pred <- forecast(leo.seas.reg, h = 253, level = 0)
leo.seas.reg.pred
plot(leo.seas.reg.pred, xlab = "Time", ylab = "Views on Wikipedia", main = "Leonardo DiCaprio Future Wikipedia Views Time Series Forecasted")

#season and line
leo.seasandline.reg <- tslm( leo.ts ~ trend + season)
summary(leo.seasandline.reg)

leo.seasandline.reg.pred <- forecast(leo.seasandline.reg, h = 253, level = 0)
leo.seasandline.reg.pred
plot(leo.seasandline.reg.pred, xlab = "Time", ylab = "Views on Wikipedia", main = "Leonardo DiCaprio Future Wikipedia Views Time Series Forecasted")

leo.seasandquad.reg <- tslm(leo.ts ~ trend + I(trend^2) + season)
summary(leo.seasandquad.reg)

leo.seasandquad.reg.pred <- forecast(leo.seasandquad.reg, h = 253, level = 0)
leo.seasandquad.reg.pred
plot(leo.seasandquad.reg.pred, xlab = "Time", ylab = "Views on Wikipedia", main = "Leonardo DiCaprio Future Wikipedia Views Time Series Forecasted")

round(accuracy(leo.seas.reg.pred$fitted, futureleo.ts), 3)
round(accuracy(leo.seasandline.reg.pred$fitted, futureleo.ts), 3)
round(accuracy(leo.seasandquad.reg.pred$fitted, futureleo.ts), 3)



##Barack Obama Future ANalysis
obama.ts <- ts(poli$`Barack_Obama_en.wikipedia.org_desktop_all-agents`,start = c(2015, as.numeric(format(inds[1], "%j"))),frequency = 365)
futureobama.ts <-ts(future$`Barack_Obama_en.wikipedia.org_desktop_all-agents`,start = c(2017, as.numeric(format(inds2[1], "%j"))),frequency = 365)
plot(futureobama.ts, xlab = "Time", ylab = "Views on Wikipedia", main = "Barack Obama Future Wikipedia Views Time Series")

#seasonal
obama.seas.reg <- tslm(obama.ts ~ season)
summary(obama.seas.reg)

obama.seas.reg.pred <- forecast(obama.seas.reg, h = 253, level = 0)
obama.seas.reg.pred
plot(obama.seas.reg.pred, xlab = "Time", ylab = "Views on Wikipedia", main = "Barack Obama Future Wikipedia Views Time Series Forecasted")

#season and line
obama.seasandline.reg <- tslm(obama.ts ~ trend + season)
summary(obama.seasandline.reg)

obama.seasandline.reg.pred <- forecast(obama.seasandline.reg, h = 253, level = 0)
obama.seasandline.reg.pred
plot(obama.seasandline.reg.pred, xlab = "Time", ylab = "Views on Wikipedia", main = "Barack Obama Future Wikipedia Views Time Series Forecasted")

obama.seasandquad.reg <- tslm(obama.ts ~ trend + I(trend^2) + season)
summary(obama.seasandquad.reg)

obama.seasandquad.reg.pred <- forecast(obama.seasandquad.reg, h = 253, level = 0)
obama.seasandquad.reg.pred
plot(obama.seasandquad.reg.pred, xlab = "Time", ylab = "Views on Wikipedia", main = "Barack Obama Future Wikipedia Views Time Series Forecasted")

round(accuracy(obama.seas.reg.pred$fitted, futureobama.ts), 3)
round(accuracy(obama.seasandline.reg.pred$fitted, futureobama.ts), 3)
round(accuracy(obama.seasandquad.reg.pred$fitted, futureobama.ts), 3)



##Joe Biden Future ANalysis
joe.ts <- ts(poli$`Joe_Biden_en.wikipedia.org_desktop_all-agents`,start = c(2015, as.numeric(format(inds[1], "%j"))),frequency = 365)
futurejoe.ts <-ts(future$`Joe_Biden_en.wikipedia.org_desktop_all-agents`,start = c(2017, as.numeric(format(inds2[1], "%j"))),frequency = 365)
plot(futurejoe.ts, xlab = "Time", ylab = "Views on Wikipedia", main = "Joe Biden Future Wikipedia Views Time Series")

#seasonal
joe.seas.reg <- tslm(joe.ts ~ season)
summary(joe.seas.reg)

joe.seas.reg.pred <- forecast(joe.seas.reg, h = 253, level = 0)
joe.seas.reg.pred
plot(joe.seas.reg.pred, xlab = "Time", ylab = "Views on Wikipedia", main = "JOe Biden Future Wikipedia Views Time Series Forecasted")

#season and line
joe.seasandline.reg <- tslm(joe.ts ~ trend + season)
summary(joe.seasandline.reg)

joe.seasandline.reg.pred <- forecast(joe.seasandline.reg, h = 253, level = 0)
joe.seasandline.reg.pred
plot(joe.seasandline.reg.pred, xlab = "Time", ylab = "Views on Wikipedia", main = "Joe Biden Future Wikipedia Views Time Series Forecasted")

joe.seasandquad.reg <- tslm(joe.ts ~ trend + I(trend^2) + season)
summary(joe.seasandquad.reg)

joe.seasandquad.reg.pred <- forecast(joe.seasandquad.reg, h = 253, level = 0)
joe.seasandquad.reg.pred
plot(joe.seasandquad.reg.pred, xlab = "Time", ylab = "Views on Wikipedia", main = "Joe Biden Future Wikipedia Views Time Series Forecasted")

round(accuracy(joe.seas.reg.pred$fitted, futurejoe.ts), 3)
round(accuracy(joe.seasandline.reg.pred$fitted, futurejoe.ts), 3)
round(accuracy(joe.seasandquad.reg.pred$fitted, futurejoe.ts), 3)



##Kendrick Lamar Future ANalysis
lamar.ts <- ts(singer$`Kendrick_Lamar_en.wikipedia.org_desktop_all-agents`,start = c(2015, as.numeric(format(inds[1], "%j"))),frequency = 365)
futurelamar.ts <-ts(future$`Kendrick_Lamar_en.wikipedia.org_desktop_all-agents`,start = c(2017, as.numeric(format(inds2[1], "%j"))),frequency = 365)
plot(futurelamar.ts, xlab = "Time", ylab = "Views on Wikipedia", main = "Kendrick Lamar Future Wikipedia Views Time Series")

#seasonal
lamar.seas.reg <- tslm(lamar.ts ~ season)
summary(lamar.seas.reg)

lamar.seas.reg.pred <- forecast(lamar.seas.reg, h = 253, level = 0)
lamar.seas.reg.pred
plot(lamar.seas.reg.pred, xlab = "Time", ylab = "Views on Wikipedia", main = "Kendrcik Lamar Future Wikipedia Views Time Series Forecasted")

#season and line
lamar.seasandline.reg <- tslm(lamar.ts ~ trend + season)
summary(lamar.seasandline.reg)

lamar.seasandline.reg.pred <- forecast(lamar.seasandline.reg, h = 253, level = 0)
lamar.seasandline.reg.pred
plot(lamar.seasandline.reg.pred, xlab = "Time", ylab = "Views on Wikipedia", main = "Kendrick Lamar Future Wikipedia Views Time Series Forecasted")

lamar.seasandquad.reg <- tslm(lamar.ts ~ trend + I(trend^2) + season)
summary(lamar.seasandquad.reg)

lamar.seasandquad.reg.pred <- forecast(lamar.seasandquad.reg, h = 253, level = 0)
lamar.seasandquad.reg.pred
plot(lamar.seasandquad.reg.pred, xlab = "Time", ylab = "Views on Wikipedia", main = "Kendrcik Lamar Future Wikipedia Views Time Series Forecasted")

round(accuracy(lamar.seas.reg.pred$fitted, futurelamar.ts), 3)
round(accuracy(lamar.seasandline.reg.pred$fitted, futurelamar.ts), 3)
round(accuracy(lamar.seasandquad.reg.pred$fitted, futurelamar.ts), 3)



##Bruno Mars Future ANalysis
bruno.ts <- ts(singer$`Bruno_Mars_en.wikipedia.org_desktop_all-agents`,start = c(2015, as.numeric(format(inds[1], "%j"))),frequency = 365)
futurebruno.ts <-ts(future$`Bruno_Mars_en.wikipedia.org_desktop_all-agents`,start = c(2017, as.numeric(format(inds2[1], "%j"))),frequency = 365)
plot(futurebruno.ts, xlab = "Time", ylab = "Views on Wikipedia", main = "Bruno Mars Future Wikipedia Views Time Series")

#seasonal
bruno.seas.reg <- tslm(bruno.ts ~ season)
summary(bruno.seas.reg)

bruno.seas.reg.pred <- forecast(bruno.seas.reg, h = 253, level = 0)
bruno.seas.reg.pred
plot(bruno.seas.reg.pred, xlab = "Time", ylab = "Views on Wikipedia", main = "Bruno Mars Future Wikipedia Views Time Series Forecasted")

#season and line
bruno.seasandline.reg <- tslm( bruno.ts ~ trend + season)
summary(bruno.seasandline.reg)

bruno.seasandline.reg.pred <- forecast(bruno.seasandline.reg, h = 253, level = 0)
bruno.seasandline.reg.pred
plot(bruno.seasandline.reg.pred, xlab = "Time", ylab = "Views on Wikipedia", main = "Bruno Mars Future Wikipedia Views Time Series Forecasted")

bruno.seasandquad.reg <- tslm( bruno.ts ~ trend + I(trend^2) + season)
summary(bruno.seasandquad.reg)

bruno.seasandquad.reg.pred <- forecast(bruno.seasandquad.reg, h = 253, level = 0)
bruno.seasandquad.reg.pred
plot(bruno.seasandquad.reg.pred, xlab = "Time", ylab = "Views on Wikipedia", main = "Bruno Mars Future Wikipedia Views Time Series Forecasted")

round(accuracy(bruno.seas.reg.pred$fitted, futurebruno.ts), 3)
round(accuracy(bruno.seasandline.reg.pred$fitted, futurebruno.ts), 3)
round(accuracy(bruno.seasandquad.reg.pred$fitted, futurebruno.ts), 3)


##Michael Jordan Future ANalysis
jordan.ts <- ts(sport$`Michael_Jordan_en.wikipedia.org_desktop_all-agents`,start = c(2015, as.numeric(format(inds[1], "%j"))),frequency = 365)
futurejordan.ts <-ts(future$`Michael_Jordan_en.wikipedia.org_desktop_all-agents`,start = c(2017, as.numeric(format(inds2[1], "%j"))),frequency = 365)
plot(futurejordan.ts, xlab = "Time", ylab = "Views on Wikipedia", main = "Michael Jordan Future Wikipedia Views Time Series")

#seasonal
jordan.seas.reg <- tslm(jordan.ts ~ season)
summary(jordan.seas.reg)

jordan.seas.reg.pred <- forecast(jordan.seas.reg, h = 253, level = 0)
jordan.seas.reg.pred
plot(jordan.seas.reg.pred, xlab = "Time", ylab = "Views on Wikipedia", main = "Michael Jordan Future Wikipedia Views Time Series Forecasted")

#season and line
jordan.seasandline.reg <- tslm( jordan.ts ~ trend + season)
summary(jordan.seasandline.reg)

jordan.seasandline.reg.pred <- forecast(jordan.seasandline.reg, h = 253, level = 0)
jordan.seasandline.reg.pred
plot(jordan.seasandline.reg.pred, xlab = "Time", ylab = "Views on Wikipedia", main = "Michael Jordan Future Wikipedia Views Time Series Forecasted")

jordan.seasandquad.reg <- tslm( jordan.ts ~ trend + I(trend^2) + season)
summary(jordan.seasandquad.reg)

jordan.seasandquad.reg.pred <- forecast(jordan.seasandquad.reg, h = 253, level = 0)
jordan.seasandquad.reg.pred
plot(jordan.seasandquad.reg.pred, xlab = "Time", ylab = "Views on Wikipedia", main = "Michael Jordan Future Wikipedia Views Time Series Forecasted")

round(accuracy(jordan.seas.reg.pred$fitted, futurejordan.ts), 3)
round(accuracy(jordan.seasandline.reg.pred$fitted, futurejordan.ts), 3)
round(accuracy(jordan.seasandquad.reg.pred$fitted, futurejordan.ts), 3)



##maria Sharapova Future ANalysis
maria.ts <- ts(sport$`Maria_Sharapova_en.wikipedia.org_desktop_all-agents`,start = c(2015, as.numeric(format(inds[1], "%j"))),frequency = 365)
futuremaria.ts <-ts(future$`Maria_Sharapova_en.wikipedia.org_desktop_all-agents`,start = c(2017, as.numeric(format(inds2[1], "%j"))),frequency = 365)
plot(futuremaria.ts, xlab = "Time", ylab = "Views on Wikipedia", main = "Maria Sharapova Future Wikipedia Views Time Series")

#seasonal
maria.seas.reg <- tslm(maria.ts ~ season)
summary(maria.seas.reg)

maria.seas.reg.pred <- forecast(maria.seas.reg, h = 253, level = 0)
maria.seas.reg.pred
plot(maria.seas.reg.pred, xlab = "Time", ylab = "Views on Wikipedia", main = "Maria Sharapova Future Wikipedia Views Time Series Forecasted")

#season and line
maria.seasandline.reg <- tslm( maria.ts ~ trend + season)
summary(maria.seasandline.reg)

maria.seasandline.reg.pred <- forecast(maria.seasandline.reg, h = 253, level = 0)
maria.seasandline.reg.pred
plot(maria.seasandline.reg.pred, xlab = "Time", ylab = "Views on Wikipedia", main = "Maria Sharapova Future Wikipedia Views Time Series Forecasted")

maria.seasandquad.reg <- tslm(maria.ts ~ trend + I(trend^2) + season)
summary(maria.seasandquad.reg)

maria.seasandquad.reg.pred <- forecast(maria.seasandquad.reg, h = 253, level = 0)
maria.seasandquad.reg.pred
plot(maria.seasandquad.reg.pred, xlab = "Time", ylab = "Views on Wikipedia", main = "Maria Sharapova Future Wikipedia Views Time Series Forecasted")

round(accuracy(maria.seas.reg.pred$fitted, futuremaria.ts), 3)
round(accuracy(maria.seasandline.reg.pred$fitted, futuremaria.ts), 3)
round(accuracy(maria.seasandquad.reg.pred$fitted, futuremaria.ts), 3)









