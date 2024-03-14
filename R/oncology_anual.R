library(readxl)
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(readxl)
library(lubridate)
library(CausalImpact)
library(MMWRweek)

setwd("/Users/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Original Data/")
data_ts <- read_csv(unzip("MAPFRE_Weekly_data.zip", "MAPFRE_Weekly_data.csv"))
colnames(data_ts) <- c("Year", "WeekNum", "Code_Real", "Code_Esp", "Code_Group", "Id", "Province", "Sex", "Age", "Acts")
totals <- read_xlsx("S67_IAL_CUBO_CARTERA_HISTORICA_SALUD_FAMILIAS_2023_febrero.xlsx",
                    sheet="cartera 2023-febrero")
colnames(totals) <- c("Date", "No.Fun", "Fun", "Dental", "Reimb", "Ind", "Total")
totals$Date <- paste0("01/", totals$Date)
totals$Date <- as.Date(totals$Date, "%d/%m/%Y")
totals <- totals[totals$Date >= "2019-01-01" &
                   totals$Date <= "2023-01-07", ]

df <- approx(x=totals$Date,y=totals$Total, xout=seq(as.Date("2019-01-01"), as.Date("2023-01-07"), "weeks"))
totals <- data.frame(Date=df$x, Total=df$y)

oncology <- data_ts[data_ts$Code_Real==33 & data_ts$Code_Esp==33 & data_ts$Code_Group=="10100" & data_ts$Id== 1, ]
oncology <- oncology %>% group_by(Year, WeekNum) %>% summarise(Acts=sum(Acts)) ### Remove duplicates

### Remove first week of 2023
totals <- totals[year(totals$Date)!=2023, ]

oncology$Date <- totals$Date

### 2019 vs 2020
y <- zoo(oncology$Acts[year(oncology$Date)<=2020 ], oncology$Date[year(oncology$Date)<=2020 ])
x <- totals$Total[year(totals$Date)<=2020 ]
y <- y/x*1000
PeriodHol <- DateRangeHoliday("PeriodHolidays",
                              start = as.Date(c("2019-08-01", "2020-08-01", "2019-12-25", "2020-12-25")),
                              end = as.Date(c("2019-09-01", "2020-09-01", "2020-01-07", "2021-01-07")))
ss <- AddStaticIntercept(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 52)
ss <- AddRandomWalkHoliday(ss, y, holiday=PeriodHol)

post.period <- c(which(year(oncology$Date)==2020)[1], which(year(oncology$Date)==2020)[length(which(year(oncology$Date)==2020))])
post.period.response <- y[post.period[1] : post.period[2]]
y[post.period[1] : post.period[2]] <- NA
bsts.model <- bsts(y, ss, niter = 50000, seed = 14042023)
impact <- CausalImpact(bsts.model = bsts.model,
                       post.period.response = as.numeric(post.period.response))
png("/Users/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Mapfre/Papers/BSTS_HealthInsurance/Material Suplementari/oncology_2019_2021.png")
plot(impact)
dev.off()
summary(impact)

### 2019 vs 2021
y <- oncology$Acts[year(oncology$Date)!=2022]
x <- totals$Total[year(totals$Date)!=2022]
y <- y/x*1000
PeriodHol <- DateRangeHoliday("PeriodHolidays",
                              start = as.Date(c("2019-08-01", "2019-12-25", "2020-08-01", "2020-12-25", "2021-08-01", "2021-12-25")),
                              end = as.Date(c("2019-09-01", "2020-01-07", "2020-09-01", "2021-01-07", "2021-09-01", "2022-01-07")))
ss <- AddStaticIntercept(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 52)
ss <- AddRandomWalkHoliday(ss, y, holiday=PeriodHol, time0="2019-01-01")

post.period <- c(106, 157)
post.period.response <- y[post.period[1] : post.period[2]]
y[post.period[1] : post.period[2]] <- NA
bsts.model <- bsts(y, ss, niter = 50000, seed = 14042023)
impact <- CausalImpact(bsts.model = bsts.model,
                       post.period.response = as.numeric(post.period.response))
png("/Users/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Mapfre/Papers/BSTS_HealthInsurance/Material Suplementari/oncology_2019_2021.png")
  plot(impact)
dev.off()
summary(impact)

### 2019 vs 2022
y <- oncology$Acts
x <- totals$Total
y <- y/x*1000
PeriodHol <- DateRangeHoliday("PeriodHolidays",
                              start = as.Date(c("2019-08-01", "2019-12-25", "2020-08-01", "2020-12-25", "2021-08-01", "2021-12-25", "2022-08-01", "2022-12-25")),
                              end = as.Date(c("2019-09-01", "2020-01-07", "2020-09-01", "2021-01-07", "2021-09-01", "2022-01-07", "2022-09-01", "2023-01-07")))
post.period <- c(159, 209)
post.period.response <- y[post.period[1] : post.period[2]]
y[post.period[1] : post.period[2]] <- NA
ss <- AddStaticIntercept(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 52)
ss <- AddRandomWalkHoliday(ss, y, holiday=PeriodHol, time0="2019-01-01")
bsts.model <- bsts(y, ss, niter = 50000)
impact <- CausalImpact(bsts.model = bsts.model,
                       post.period.response = post.period.response)
png("/Users/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Mapfre/Papers/BSTS_HealthInsurance/Material Suplementari/oncology_2019_2022.png")
plot(impact)
dev.off()
summary(impact)
