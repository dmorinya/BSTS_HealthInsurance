library(readxl)
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(readxl)
library(lubridate)
library(CausalImpact)
library(MMWRweek)

mapfre_ts <- read_csv(unzip("Data/MAPFRE_Weekly_data.zip", "MAPFRE_Weekly_data.csv"))
totals <- read_xlsx("Data/S67_IAL_CUBO_CARTERA_HISTORICA_SALUD_FAMILIAS.xlsx")
colnames(mapfre_ts) <- c("Year", "WeekNum", "Code_Real", "Code_Esp", "Code_Group", "Id", "Province", "Sex", "Age", "Acts")
colnames(totals) <- c("Date", "No.Fun", "Fun", "Dental", "Reimb", "Ind", "Total")
totals$Date <- paste0("01/", totals$Date)
totals$Date <- as.Date(totals$Date, "%d/%m/%Y")
totals <- totals[totals$Date >= "2019-01-01" &
                 totals$Date <= "2022-01-01", ]

df <- approx(x=totals$Date,y=totals$Total, xout=seq(as.Date("2019/1/1"), as.Date("2022/01/07"), "weeks"))
totals <- data.frame(Date=df$x, Total=df$y)

totals <- totals[-158, ]

oncology <- mapfre_ts[mapfre_ts$Code_Real==33 & mapfre_ts$Code_Esp==33 & mapfre_ts$Code_Group=="10100" & mapfre_ts$Id== 1, ]

oncology <- oncology %>% group_by(Year, WeekNum) %>% summarise(Acts=sum(Acts)) ### Remove duplicates

### La darrera setmana de 2021 té uns valors molt baixos en tots els casos, no semblen correctes
oncology <- oncology[-158, ]

oncology$Date <- totals$Date

y <- oncology$Acts[oncology$Date<="2021-06-01"]
x <- totals$Total[totals$Date<="2021-06-01"]
ss <- AddStaticIntercept(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 52)
bsts.model <- bsts(y~x, ss, niter = 50000)

### Predictions
pred <- predict(bsts.model, newdata=data.frame(x=rep(607852.9, 30)), burn = 100)

rmse <- sqrt(mean((oncology$Acts[oncology$Date>"2021-06-01"]-pred$mean)^2)) ### 38374.26
mape <- mean(abs((oncology$Acts[oncology$Date>"2021-06-01"]-pred$mean)/oncology$Acts[oncology$Date>"2021-06-01"])) * 100 ### 14.35%

png("Results/bsts_oncology_prediction.png", width=1280)
ggplot(oncology, aes(x=Date, y=Acts)) +
  geom_line() + ylim(min(pred$interval[1,])-50, max(pred$interval[2,])+50) + xlab("")+geom_point()+scale_x_date(date_labels = "%m-%Y", date_breaks = "3 months") + ggtitle("ONCOLOGY") + theme(plot.title = element_text(hjust = 0.5))+geom_line(y = c(rep(NA, 127), pred$mean), color = "red")+
  geom_ribbon(aes(ymin=c(rep(NA, 127), pred$interval[1,]),ymax=c(rep(NA, 127), pred$interval[2,])), fill="blue", alpha=0.2)
dev.off()