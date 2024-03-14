library(readxl)
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(readxl)
library(lubridate)
library(CausalImpact)
library(MMWRweek)
library(forecast)
library(tseries)

setwd("/Users/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Original Data/")
data_ts <- read_csv(unzip("MAPFRE_Weekly_data.zip", "MAPFRE_Weekly_data.csv"))
colnames(data_ts) <- c("Year", "WeekNum", "Code_Real", "Code_Esp", "Code_Group", "Id", "Province", "Sex", "Age", "Acts")

global <- data_ts %>% group_by(Year, WeekNum) %>%
  summarise(Acts=sum(Acts))

global$Date <- seq(as.Date("2019-01-01"), as.Date("2023-01-07"), "weeks")

### Remove first week of 2023
global <- global[year(global$Date)!=2023, ]

### DESCRIPTIVE
ref <- sum(global$Acts[global$Year==2019])
y2020 <- sum(global$Acts[global$Year==2020])
y2021 <- sum(global$Acts[global$Year==2021])
y2022 <- sum(global$Acts[global$Year==2022])

(y2020-ref)/ref*100
(y2021-ref)/ref*100
(y2022-ref)/ref*100

### ARIMA modelling
y <- global[global$Date<="2022-06-01", ]
y_pred <- global[global$Date>"2022-06-01", ]
### Stationarity test
kpss.test(y$Acts) ### Not trend stationary (p-value = 0.01)

fit <- arima(y$Acts, order=c(1, 0, 0), seasonal=list(order=c(1, 0, 0), frequency=52))

pred <- predict(fit, n.ahead=30, interval="prediction")

rmse <- sqrt(mean((y_pred$Acts-as.numeric(pred$pred))^2)) ### 57422.57
mape <- mean(abs((y_pred$Acts-as.numeric(pred$pred))/y_pred$Acts)) * 100 ### 19.33439%

lower95 <- pred$pred - qnorm(0.975)*pred$se
upper95 <- pred$pred + qnorm(0.975)*pred$se

lower90 <- pred$pred - qnorm(0.95)*pred$se
upper90 <- pred$pred + qnorm(0.95)*pred$se

lower75 <- pred$pred - qnorm(0.875)*pred$se
upper75 <- pred$pred + qnorm(0.875)*pred$se

png("/home/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Mapfre/Papers/BSTS_HealthInsurance/Results/arima_global_prediction.png", width=1280)
ggplot(global, aes(x=Date, y=Acts)) +
  geom_line() + ylim(19600, 365019) + xlab("")+geom_point()+scale_x_date(date_labels = "%m-%Y", date_breaks = "3 months") + ggtitle("") + theme(plot.title = element_text(hjust = 0.5))+geom_line(y = c(rep(NA, 179), pred$pred), color = "red")+
  geom_ribbon(aes(ymin=c(rep(NA, 179), lower95),ymax=c(rep(NA, 179), upper95)), fill="blue", alpha=0.2) +
  geom_ribbon(aes(ymin=c(rep(NA, 179), lower90),ymax=c(rep(NA, 179), upper90)), fill="orange", alpha=0.2) + 
  geom_ribbon(aes(ymin=c(rep(NA, 179), lower75),ymax=c(rep(NA, 179), upper75)), fill="red", alpha=0.2)
dev.off()