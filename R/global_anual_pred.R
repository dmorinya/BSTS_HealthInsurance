library(readxl)
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(readxl)
library(lubridate)
library(CausalImpact)
library(MMWRweek)

mapfre_ts <- read_csv(unzip("/home/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Original MAPFRE Data/MAPFRE_Weekly_data.zip", "MAPFRE_Weekly_data.csv"))
colnames(mapfre_ts) <- c("Year", "WeekNum", "Code_Real", "Code_Esp", "Code_Group", "Id", "Province", "Sex", "Age", "Acts")
totals <- read_xlsx("/home/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Original MAPFRE Data/S67_IAL_CUBO_CARTERA_HISTORICA_SALUD_FAMILIAS_2023_febrero.xlsx",
                    sheet="cartera 2023-febrero")
colnames(totals) <- c("Date", "No.Fun", "Fun", "Dental", "Reimb", "Ind", "Total")
totals$Date <- paste0("01/", totals$Date)
totals$Date <- as.Date(totals$Date, "%d/%m/%Y")
totals <- totals[totals$Date >= "2019-01-01" &
                   totals$Date <= "2022-12-31", ]

df <- approx(x=totals$Date,y=totals$Total, xout=seq(as.Date("2019-01-01"), as.Date("2022-12-01"), "weeks"))
totals <- data.frame(Date=df$x, Total=df$y)

global <- mapfre_ts %>% group_by(Year, WeekNum) %>%
  summarise(Acts=sum(Acts))

global$Date <- seq(as.Date("2019-01-01"), as.Date("2023-01-07"), "weeks")

y <- global$Acts[global$Date<="2022-06-01"]
x <- totals$Total[totals$Date<="2022-06-01"]
ss <- AddStaticIntercept(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 52)
bsts.model <- bsts(y~x, ss, niter = 50000)

### Predictions
pred95 <- predict(bsts.model, newdata=data.frame(x=rep(621493.0, 31)), quantiles=c(0.025, 0.975), burn = 100, seed=14042023)
pred90 <- predict(bsts.model, newdata=data.frame(x=rep(621493.0, 31)), quantiles=c(0.05, 0.95), burn = 100, seed=14042023)
pred75 <- predict(bsts.model, newdata=data.frame(x=rep(621493.0, 31)), quantiles=c(0.125, 0.875), burn = 100, seed=14042023)

rmse <- sqrt(mean((global$Acts[global$Date>"2022-06-01"]-pred95$mean)^2)) ### 48388.5
mape <- mean(abs((global$Acts[global$Date>"2022-06-01"]-pred95$mean)/global$Acts[global$Date>"2022-06-01"])) * 100 ### 15.59%

png("bsts_global_prediction.png", width=1280)
ggplot(global, aes(x=Date, y=Acts)) +
  geom_line() + ylim(10000, 450000) + xlab("")+geom_point()+scale_x_date(date_labels = "%m-%Y", date_breaks = "3 months") + ggtitle("") + theme(plot.title = element_text(hjust = 0.5))+geom_line(y = c(rep(NA, 179), pred95$mean), color = "red")+
  geom_ribbon(aes(ymin=c(rep(NA, 179), pred95$interval[1,]), ymax=c(rep(NA, 179), pred95$interval[2,])), fill="blue", alpha=0.2) + 
  geom_ribbon(aes(ymin=c(rep(NA, 179), pred90$interval[1,]), ymax=c(rep(NA, 179), pred90$interval[2,])), fill="orange", alpha=0.2) + 
  geom_ribbon(aes(ymin=c(rep(NA, 179), pred75$interval[1,]), ymax=c(rep(NA, 179), pred75$interval[2,])), fill="red", alpha=0.2)
dev.off()