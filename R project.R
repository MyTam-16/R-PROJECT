install.packages('dplyr')
library(dplyr)
library(readxl)

#Task 2. Data collection and input

K204141929 <- read_excel("K204141929.xlsx")
View(K204141929)
DMS <- K204141929[3]/K204141929[4]
Liquidity <- K204141929[5]/K204141929[6]
Size <- log(K204141929[7])
Leverage <- K204141929[8]/K204141929[7]
ROA <- K204141929[9]
dt <- data.frame(K204141929[1], K204141929[2],DMS, Liquidity, Size, Leverage, ROA)
dt <- setNames(dt, c("Code","Date","DMS","Liquidity","Size","Leverage","ROA"))
options(scipen=999)
View(dt)

summary(dt)
sum(is.na(dt))

#Task 3. Provide descriptive statistics of all the variables

#Entire period
sta <- data.frame(min=sapply(dt[3:7], min, na.rm = TRUE),
                  max=sapply(dt[3:7], max, na.rm = TRUE),
                  mean=sapply(dt[3:7], mean, na.rm = TRUE),
                  median= sapply(dt[3:7], median, na.rm = TRUE), 
                  sd=sapply(dt[3:7], sd, na.rm = TRUE))
View(sta)

#Before period
dt1 <- data.frame(select(dt, c(1,2,3,4,5,6,7))
                  %>%  filter(dt[2] < "2020-03-31"))
dt1 <- setNames(dt1, c("Code","Date","DMS","Liquidity","Size","Leverage","ROA"))
View(dt1)
sta1 <- data.frame(min=sapply(dt1[3:7], min, na.rm = TRUE),
                   max=sapply(dt1[3:7], max, na.rm = TRUE),
                   mean=sapply(dt1[3:7], mean, na.rm = TRUE),
                   median=sapply(dt1[3:7], median, na.rm = TRUE), 
                   sd=sapply(dt1[3:7], sd, na.rm = TRUE))
View(sta1)

#After period
dt2 <- data.frame(select(dt, c(1,2,3,4,5,6,7))
                  %>%  filter(dt[2] >= "2020-03-31"))
dt2 <- setNames(dt2, c("Code","Date","DMS","Liquidity","Size","Leverage","ROA"))
View(dt2)
sta2 <- data.frame(min=sapply(dt2[3:7], min, na.rm = TRUE),
                   max=sapply(dt2[3:7], max, na.rm = TRUE),
                   mean=sapply(dt2[3:7], mean, na.rm = TRUE),
                   median=sapply(dt2[3:7], median, na.rm = TRUE), 
                   sd=sapply(dt2[3:7], sd, na.rm = TRUE))
View(sta2)

#Task 4.Provide box & whisker plot and histogram of the variable of assigned topic

#Entire period
library(ggplot2)
dt %>%
  filter(!is.na(DMS)) %>%
  ggplot(aes(y = DMS,fill='DMS')) +
  geom_boxplot() +
  coord_flip()+
  scale_fill_manual(values = c('#265493'))+
  scale_y_continuous(labels = scales::comma)

ggplot(dt, aes(x = DMS, fill='DMS')) +
  geom_histogram()+
  scale_fill_manual(values = c('#265493'))+
  geom_histogram(binwidth = 0.05) +
  scale_x_continuous(labels = scales::comma)

#Before period
dt1 %>%
  filter(!is.na(DMS)) %>%
  ggplot(aes(y = DMS,fill='DMS')) +
  geom_boxplot() +
  coord_flip()+
  scale_y_continuous(labels = scales::comma)

ggplot(dt1, aes(x = DMS, fill='DMS')) +
  geom_histogram()+
  geom_histogram(binwidth = 0.05) +
  scale_x_continuous(labels = scales::comma)

#After period
dt2 %>%
  filter(!is.na(DMS)) %>%
  ggplot(aes(y = DMS,fill='DMS')) +
  geom_boxplot() +
  coord_flip()+
  scale_fill_manual(values = c('yellow'))+
  scale_y_continuous(labels = scales::comma)

ggplot(dt2, aes(x = DMS, fill='DMS')) +
  geom_histogram()+
  scale_fill_manual(values = c('yellow'))+
  geom_histogram(binwidth = 0.05) +
  scale_x_continuous(labels = scales::comma)

#Task 5.Perform multiple regression to determine the significant determinants of the variable of assigned topic.

#Model 1
summary(model1<-lm(DMS ~  Liquidity +  Leverage  + Size  + ROA, data = dt))

car::vif(model1) 
par(mfrow=c(2,2))
plot(model1)
install.packages('lmtest')
library(lmtest) 
bptest(model1) 
shapiro.test(resid(model1))

#Model 2
dt$Covid <- ifelse(dt$Date < "2020-03-31", 0, 1)
summary(model2 <-lm(DMS ~ Liquidity +  Leverage  +  Size + ROA + Covid*Liquidity 
                    + Covid*Size +  Covid*Leverage + Covid*ROA  , data = dt))

options(scipen=999)
car::vif(model2)
par(mfrow=c(2,2))
plot(model2)
bptest(model2) 
shapiro.test(resid(model2))

#Predict
predictions <- predict(model1, newdata = dt)
predictions 
dt$Predicted <- predictions
View(dt)
plot(as.vector(unlist(dt[3])), type = 'l', ylab = 'Value', col ="blue")
lines(as.vector(unlist(dt$Predicted)), lty = 'dotted', col='red')
install.packages("Metrics")
library(Metrics)
rmse(dt$DMS,dt$Predicted)
mean(dt$DMS)
var(dt$DMS)


#Task 6.Using Arima to predict debt maturity structure in 2022

install.packages("tseries")
library("tseries")
install.packages("forecast")
library("forecast")
DMSts = ts(dt$DMS, start =c(2010, 6), end =c(2019, 12), frequency = 4)
DMSts

#Check stationary
plot(DMSts, type = "l", main = "Debt Maturity Structure", 
      xlab = "Year")
adf.test(DMSts)
pp.test(DMSts)

DMSts.diff <- diff(DMSts)
plot(DMSts.diff, type = 'l')
adf.test(DMSts.diff)
pp.test(DMSts.diff)

adf.test(diff(DMSts.diff))
pp.test(diff(DMSts.diff))
acf(diff(DMSts.diff))
pacf(diff(DMSts.diff))

#Finding optimal parameter
j <- forecast:::auto.arima(diff(DMSts.diff),ic='aic',trace=TRUE)
j


#Predicting
forecasted <- forecast:::forecast.Arima(j, h=4,level=c(99.5))
forecasted
X2022 <- read_excel("2022.xlsx")
plot(as.vector(unlist(X2022[2])), type = 'l', ylab = 'Value', col ="blue")
lines(as.vector(unlist(forecasted$mean[1:4])), lty = 'dotted', col='red')

#Evaluation Matrix
X2022$forecast <- forecasted$mean[1:4]
y <- as.vector(unlist(X2022[2]))
x<- as.vector(unlist(X2022$forecast))
y
x
# Mean Absolute Error (MAE)
mae <- mean(abs(y - x))
# Mean Squared Error (MSE)
mse <- mean((y - x)^2)
# Root Mean Squared Error (RMSE)
rmse <- sqrt(mse)
# Mean Absolute Percentage Error (MAPE)
mape <- mean(abs((y - x) / y)) * 100
mae
mse
rmse
mape

# Check white noise
Box.test(diff(DMSts.diff), lag = 2, type = "Ljung-Box")
shapiro.test(diff(DMSts.diff))

#Impoving arima model
t <- arima(DMSts, order =c(0,0,3))
t
k <- forecast:::forecast.Arima(t, h=4,level=c(99.5))
k

plot(as.vector(unlist(X2022[2])), type = 'l', ylab = 'Value', col ="blue")
lines(as.vector(unlist(k$mean[1:4])), lty = 'dotted', col='red')

X2022$forecast1 <- k$mean[1:4]
y1 <- as.vector(unlist(X2022[2]))
x1<- as.vector(unlist(X2022$forecast1))
y1
x1
# Mean Absolute Error (MAE)
mae1 <- mean(abs(y1 - x1))
# Mean Squared Error (MSE)
mse1 <- mean((y1 - x1)^2)
# Root Mean Squared Error (RMSE)
rmse1 <- sqrt(mse1)
# Mean Absolute Percentage Error (MAPE)
mape1 <- mean(abs((y1 - x1) / y1)) * 100
mae1
mse1
rmse1
mape1

