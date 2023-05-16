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

data_ts <- read_csv(unzip("Weekly_data.zip", "Weekly_data.csv"))
colnames(data_ts) <- c("Year", "WeekNum", "Code_Real", "Code_Esp", "Code_Group", "Id", "Province", "Sex", "Age", "Acts")
totals <- read_xlsx("S67_IAL_CUBO_CARTERA_HISTORICA_SALUD_FAMILIAS_2023_febrero.xlsx",
                    sheet="cartera 2023-febrero")
colnames(totals) <- c("Date", "No.Fun", "Fun", "Dental", "Reimb", "Ind", "Total")
totals$Date <- paste0("01/", totals$Date)
totals$Date <- as.Date(totals$Date, "%d/%m/%Y")
totals <- totals[totals$Date >= "2019-01-01" &
                 totals$Date <= "2022-12-31", ]

df <- approx(x=totals$Date,y=totals$Total, xout=seq(as.Date("2019-01-01"), as.Date("2022-12-01"), "weeks"))
totals <- data.frame(Date=df$x, Total=df$y)

global <- data_ts %>% group_by(Year, WeekNum) %>%
          summarise(Acts=sum(Acts))

global$Date <- seq(as.Date("2019-01-01"), as.Date("2023-01-07"), "weeks")

### DESCRIPTIVE
ref <- sum(global$Acts[global$Year==2019])
y2020 <- sum(global$Acts[global$Year==2020])
y2021 <- sum(global$Acts[global$Year==2021])
y2022 <- sum(global$Acts[global$Year==2022])

(y2020-ref)/ref*100
(y2021-ref)/ref*100
(y2022-ref)/ref*100

### ARIMA modelling
y <- global$Acts[global$Date<="2022-06-01"]

### Stationarity test
kpss.test(y, null="Trend") ### Not trend stationary (p-value = 0.02074)

x <- totals$Total[totals$Date<="2022-06-01"]
fit2 <- auto.arima(y, xreg=x)

pred <- predict(fit2, n.ahead=31, newxreg=data.frame(x=rep(621493.0, 31)), interval="prediction")

rmse <- sqrt(mean((global$Acts[global$Date>"2022-06-01"]-pred$pred)^2)) ### 50094.03
mape <- mean(abs((global$Acts[global$Date>"2022-06-01"]-pred$pred)/global$Acts[global$Date>"2022-06-01"])) * 100 ### 17.28%

lower95 <- pred$pred - qnorm(0.975)*pred$se
upper95 <- pred$pred + qnorm(0.975)*pred$se

lower90 <- pred$pred - qnorm(0.95)*pred$se
upper90 <- pred$pred + qnorm(0.95)*pred$se

lower75 <- pred$pred - qnorm(0.875)*pred$se
upper75 <- pred$pred + qnorm(0.875)*pred$se

png("arima_global_prediction.png", width=1280)
ggplot(global, aes(x=Date, y=Acts)) +
  geom_line() + ylim(10000, 450000) + xlab("")+geom_point()+scale_x_date(date_labels = "%m-%Y", date_breaks = "3 months") + ggtitle("") + theme(plot.title = element_text(hjust = 0.5))+geom_line(y = c(rep(NA, 179), pred$pred), color = "red")+
  geom_ribbon(aes(ymin=c(rep(NA, 179), lower95),ymax=c(rep(NA, 179), upper95)), fill="blue", alpha=0.2) +
  geom_ribbon(aes(ymin=c(rep(NA, 179), lower90),ymax=c(rep(NA, 179), upper90)), fill="orange", alpha=0.2) + 
  geom_ribbon(aes(ymin=c(rep(NA, 179), lower75),ymax=c(rep(NA, 179), upper75)), fill="red", alpha=0.2)
dev.off()