library(readxl)
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(readxl)
library(lubridate)
library(CausalImpact)
library(MMWRweek)

setwd("/home/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Original Data/")
data_ts <- read_csv(unzip("MAPFRE_Weekly_data.zip", "MAPFRE_Weekly_data.csv"))
colnames(data_ts) <- c("Year", "WeekNum", "Code_Real", "Code_Esp", "Code_Group", "Id", "Province", "Sex", "Age", "Acts")

global <- data_ts %>% group_by(Year, WeekNum) %>%
  summarise(Acts=sum(Acts))

global$Date <- seq(as.Date("2019-01-01"), as.Date("2023-01-07"), "weeks")

### Remove first week of 2023
global <- global[year(global$Date)!=2023, ]

### BSTS model 1 (local linear trend state component)
y <- zoo(global$Acts[global$Date<="2022-06-01"], global$Date[global$Date<="2022-06-01"])
PeriodHol <- DateRangeHoliday("PeriodHolidays",
                              start = as.Date(c("2019-08-01", "2020-08-01", "2021-08-01", "2019-12-25", "2020-12-25", "2021-12-25")),
                              end = as.Date(c("2019-09-01", "2020-09-01", "2021-09-01", "2020-01-07", "2021-01-07", "2022-01-07")))
ss <- AddStaticIntercept(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 52)
ss <- AddRandomWalkHoliday(ss, y, holiday=PeriodHol)
bsts.model1 <- bsts(y, ss, niter = 50000, seed = 20240222)

### Predictions
pred95_1 <- predict(bsts.model1, horizon = 30, quantiles = c(0.025, 0.975), burn = 100, seed = 20240222)
pred90_1 <- predict(bsts.model1, horizon = 30, quantiles = c(0.05, 0.95), burn = 100, seed = 20240222)
pred75_1 <- predict(bsts.model1, horizon = 30, quantiles = c(0.125, 0.875), burn = 100, seed = 20240222)

rmse1 <- sqrt(mean(global$Acts[global$Date>"2022-06-01"]-pred95_1$mean)^2) ### 33869.96
mape1 <- mean(abs(global$Acts[global$Date>"2022-06-01"]-pred95_1$mean)/(global$Acts[global$Date>"2022-06-01"])) * 100 ### 18.46756%

### Counterfactual predictions
y <- zoo(global$Acts[global$Date<="2022-06-01"], global$Date[global$Date<="2022-06-01"])
PeriodHol <- DateRangeHoliday("PeriodHolidays",
                              start = as.Date(c("2019-08-01", "2020-08-01", "2021-08-01", "2019-12-25", "2020-12-25", "2021-12-25")),
                              end = as.Date(c("2019-09-01", "2020-09-01", "2021-09-01", "2020-01-07", "2021-01-07", "2022-01-07")))
ss <- AddStaticIntercept(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 52)
ss <- AddRandomWalkHoliday(ss, y, holiday=PeriodHol)
post.period <- c(which(global$Date=="2020-03-17"), length(y))
post.period.response <- y[post.period[1] : post.period[2]]
y[post.period[1] : post.period[2]] <- NA
bsts.model1 <- bsts(y, ss, niter = 50000, seed=20240222)

pr <- CausalImpact(bsts.model=bsts.model1, post.period.response = as.numeric(post.period.response))

png("/home/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Mapfre/Papers/BSTS_HealthInsurance/Results/bsts_global_prediction.png", width=1280)
ggplot(global, aes(x=Date, y=Acts)) +
  geom_line() + ylim(19600, 365019) + xlab("") + geom_point() + scale_x_date(date_labels = "%m-%Y", date_breaks = "3 months") + ggtitle("") + 
  theme(plot.title = element_text(hjust = 0.5)) + geom_line(y = c(rep(NA, 179), pred95_1$mean), color = "red") +
  geom_ribbon(aes(ymin=c(rep(NA, 179), pred95_1$interval[1, ]), ymax=c(rep(NA, 179), pred95_1$interval[2, ])), fill="blue", alpha=0.2) + 
  geom_ribbon(aes(ymin=c(rep(NA, 179), pred90_1$interval[1, ]), ymax=c(rep(NA, 179), pred90_1$interval[2, ])), fill="orange", alpha=0.2) + 
  geom_ribbon(aes(ymin=c(rep(NA, 179), pred75_1$interval[1, ]), ymax=c(rep(NA, 179), pred75_1$interval[2, ])), fill="red", alpha=0.2) + 
  geom_line(y = c(rep(NA, 63), as.numeric(pr$series$point.pred[64:209])), color = "blue")
dev.off()
