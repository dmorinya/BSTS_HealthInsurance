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

setwd("/Users/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Original Data/")
data_ts <- read_csv(unzip("MAPFRE_Weekly_data.zip", "MAPFRE_Weekly_data.csv"))
colnames(data_ts) <- c("Year", "WeekNum", "Code_Real", "Code_Esp", "Code_Group", "Id", "Province", "Sex", "Age", "Acts")
totals <- read_xlsx("S67_IAL_CUBO_CARTERA_HISTORICA_SALUD_FAMILIAS_2023_febrero.xlsx",
                    sheet="cartera 2023-febrero")
colnames(totals) <- c("Date", "No.Fun", "Fun", "Dental", "Reimb", "Ind", "Total")
totals$Date <- paste0("01/", totals$Date)
totals$Date <- as.Date(totals$Date, "%d/%m/%Y")
totals <- totals[totals$Date >= "2019-01-01", ]
df <- approx(x=totals$Date, y=totals$Total, xout=seq(as.Date("2019-01-01"), as.Date("2023-02-01"), "weeks"))
totals <- data.frame(Date=df$x, Total=df$y)

global <- data_ts %>% group_by(Year, WeekNum) %>%
  summarise(Acts=sum(Acts))

global$Date <- seq(as.Date("2019-01-01"), as.Date("2023-01-07"), "weeks")

### Remove first week of 2023
global <- global[year(global$Date)!=2023, ]
totals <- totals[year(totals$Date)!=2023, ]

### TOTAL
### 2019 vs 2020
y <- zoo(global$Acts[year(global$Date)<=2020], global$Date[year(global$Date)<=2020])
x <- totals$Total[year(totals$Date)<=2020]
y <- y/x*1000
PeriodHol <- DateRangeHoliday("PeriodHolidays",
                              start = as.Date(c("2019-08-01", "2020-08-01", "2019-12-25", "2020-12-25")),
                              end = as.Date(c("2019-09-01", "2020-09-01", "2020-01-07", "2021-01-07")))
ss <- AddStaticIntercept(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 52)
ss <- AddRandomWalkHoliday(ss, y, holiday=PeriodHol)

post.period <- c(which(year(global$Date)==2020)[1], which(year(global$Date)==2020)[length(which(year(global$Date)==2020))])
post.period.response <- y[post.period[1] : post.period[2]]
y[post.period[1] : post.period[2]] <- NA
bsts.model <- bsts(y, ss, niter = 50000, seed = 14042023)
impact <- CausalImpact(bsts.model = bsts.model,
                       post.period.response = as.numeric(post.period.response))
png("/Users/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Mapfre/Papers/BSTS_HealthInsurance/Material Suplementari/global_2019_2020.png")
plot(impact)
dev.off()
summary(impact)

### 2019 vs 2021
y <- global$Acts[year(global$Date)!=2022]
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
png("/home/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Mapfre/Papers/BSTS_HealthInsurance/Material Suplementari/global_2019_2021.png")
plot(impact)
dev.off()
summary(impact)

### 2019 vs 2022
y <- global$Acts
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
png("/home/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Mapfre/Papers/BSTS_HealthInsurance/Material Suplementari/global_2019_2022.png")
plot(impact)
dev.off()
summary(impact)

rm(global)

### ONLY FEMALES
females <- data_ts %>% filter(Sex==1) %>% group_by(Year, WeekNum) %>% 
  summarise(Acts=sum(Acts))

### Remove first week of 2023
females$Date <- seq(as.Date("2019-01-01"), as.Date("2023-01-07"), "weeks")
females <- females[year(females$Date)!=2023, ]

### 2019 vs 2020
y <- females$Acts[year(females$Date)<=2020 ]
x <- totals$Total[year(totals$Date)<=2020 ]
y <- y/x*1000
PeriodHol <- DateRangeHoliday("PeriodHolidays",
                              start = as.Date(c("2019-08-01", "2019-12-25", "2020-08-01", "2020-12-25")),
                              end = as.Date(c("2019-09-01", "2020-01-07", "2020-09-01", "2021-01-07")))

post.period <- c(which(year(females$Date)==2020)[1], which(year(females$Date)==2020)[length(which(year(females$Date)==2020))])
post.period.response <- y[post.period[1] : post.period[2]]
y[post.period[1] : post.period[2]] <- NA
ss <- AddStaticIntercept(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 52)
ss <- AddRandomWalkHoliday(ss, y, holiday=PeriodHol, time0="2019-01-01")
bsts.model <- bsts(y, ss, niter = 50000)
impact <- CausalImpact(bsts.model = bsts.model,
                       post.period.response = post.period.response)
png("/home/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Mapfre/Papers/BSTS_HealthInsurance/Material Suplementari/females_2019_2020.png")
plot(impact)
dev.off()
summary(impact)

### 2019 vs 2021
y <- females$Acts[year(females$Date)!=2022]
x <- totals$Total[year(totals$Date)!=2022]
y <- y/x*1000
PeriodHol <- DateRangeHoliday("PeriodHolidays",
                              start = as.Date(c("2019-08-01", "2019-12-25", "2020-08-01", "2020-12-25", "2021-08-01", "2021-12-25")),
                              end = as.Date(c("2019-09-01", "2020-01-07", "2020-09-01", "2021-01-07", "2021-09-01", "2022-01-07")))
post.period <- c(106, 157)
post.period.response <- y[post.period[1] : post.period[2]]
y[post.period[1] : post.period[2]] <- NA
ss <- AddStaticIntercept(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 52)
ss <- AddRandomWalkHoliday(ss, y, holiday=PeriodHol, time0="2019-01-01")

bsts.model <- bsts(y, ss, niter = 50000)
impact <- CausalImpact(bsts.model = bsts.model,
                       post.period.response = post.period.response)
png("/home/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Mapfre/Papers/BSTS_HealthInsurance/Material Suplementari/females_2019_2021.png")
plot(impact)
dev.off()
summary(impact)

### 2019 vs 2022
y <- females$Acts
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
png("/home/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Mapfre/Papers/BSTS_HealthInsurance/Material Suplementari/females_2019_2022.png")
plot(impact)
dev.off()
summary(impact)

### ONLY MALES (Sex=2)
males <- data_ts %>% filter(Sex==2) %>% group_by(Year, WeekNum) %>% 
  summarise(Acts=sum(Acts))

males$Date <- seq(as.Date("2019-01-01"), as.Date("2023-01-07"), "weeks")

### Remove first week of 2023
males <- males[year(males$Date)!=2023, ]

### 2019 vs 2020
y <- males$Acts[year(males$Date)<=2020 ]
x <- totals$Total[year(totals$Date)<=2020 ]
y <- y/x*1000
PeriodHol <- DateRangeHoliday("PeriodHolidays",
                              start = as.Date(c("2019-08-01", "2019-12-25", "2020-08-01", "2020-12-25")),
                              end = as.Date(c("2019-09-01", "2020-01-07", "2020-09-01", "2021-01-07")))

post.period <- c(which(year(males$Date)==2020)[1], which(year(males$Date)==2020)[length(which(year(males$Date)==2020))])
post.period.response <- y[post.period[1] : post.period[2]]
y[post.period[1] : post.period[2]] <- NA
ss <- AddStaticIntercept(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 52)
ss <- AddRandomWalkHoliday(ss, y, holiday=PeriodHol, time0="2019-01-01")
bsts.model <- bsts(y, ss, niter = 50000)
impact <- CausalImpact(bsts.model = bsts.model,
                       post.period.response = post.period.response)

png("/home/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Mapfre/Papers/BSTS_HealthInsurance/Material Suplementari/males_2019_2020.png")
plot(impact)
dev.off()
summary(impact)

### 2019 vs 2021
y <- males$Acts[year(males$Date)!=2022]
x <- totals$Total[year(totals$Date)!=2022]
y <- y/x*1000
PeriodHol <- DateRangeHoliday("PeriodHolidays",
                              start = as.Date(c("2019-08-01", "2019-12-25", "2020-08-01", "2020-12-25", "2021-08-01", "2021-12-25")),
                              end = as.Date(c("2019-09-01", "2020-01-07", "2020-09-01", "2021-01-07", "2021-09-01", "2022-01-07")))
post.period <- c(106, 157)
post.period.response <- y[post.period[1] : post.period[2]]
y[post.period[1] : post.period[2]] <- NA
ss <- AddStaticIntercept(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 52)
ss <- AddRandomWalkHoliday(ss, y, holiday=PeriodHol, time0="2019-01-01")

bsts.model <- bsts(y, ss, niter = 50000)
impact <- CausalImpact(bsts.model = bsts.model,
                       post.period.response = post.period.response)
png("/home/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Mapfre/Papers/BSTS_HealthInsurance/Material Suplementari/males_2019_2021.png")
plot(impact)
dev.off()
summary(impact)

### 2019 vs 2022
y <- males$Acts
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
png("/home/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Mapfre/Papers/BSTS_HealthInsurance/Material Suplementari/males_2019_2022.png")
plot(impact)
dev.off()
summary(impact)

### OVER 60
over60 <- data_ts %>% filter(Age=="(59, 200]") %>% group_by(Year, WeekNum) %>% 
  summarise(Acts=sum(Acts))

over60$Date <- seq(as.Date("2019-01-01"), as.Date("2023-01-07"), "weeks")

### Remove first week of 2023
over60 <- over60[year(over60$Date)!=2023, ]

### 2019 vs 2020
y <- over60$Acts[year(over60$Date)<=2020 ]
x <- totals$Total[year(totals$Date)<=2020 ]
y <- y/x*1000
PeriodHol <- DateRangeHoliday("PeriodHolidays",
                              start = as.Date(c("2019-08-01", "2019-12-25", "2020-08-01", "2020-12-25")),
                              end = as.Date(c("2019-09-01", "2020-01-07", "2020-09-01", "2021-01-07")))

post.period <- c(which(year(over60$Date)==2020)[1], which(year(over60$Date)==2020)[length(which(year(over60$Date)==2020))])
post.period.response <- y[post.period[1] : post.period[2]]
y[post.period[1] : post.period[2]] <- NA
ss <- AddStaticIntercept(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 52)
ss <- AddRandomWalkHoliday(ss, y, holiday=PeriodHol, time0="2019-01-01")
bsts.model <- bsts(y, ss, niter = 50000)
impact <- CausalImpact(bsts.model = bsts.model,
                       post.period.response = post.period.response)
png("/home/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Mapfre/Papers/BSTS_HealthInsurance/Material Suplementari/over60_2019_2020.png")
plot(impact)
dev.off()
summary(impact)

### 2019 vs 2021
y <- over60$Acts[year(over60$Date)!=2022]
x <- totals$Total[year(totals$Date)!=2022]
y <- y/x*1000
PeriodHol <- DateRangeHoliday("PeriodHolidays",
                              start = as.Date(c("2019-08-01", "2019-12-25", "2020-08-01", "2020-12-25", "2021-08-01", "2021-12-25")),
                              end = as.Date(c("2019-09-01", "2020-01-07", "2020-09-01", "2021-01-07", "2021-09-01", "2022-01-07")))
post.period <- c(106, 157)
post.period.response <- y[post.period[1] : post.period[2]]
y[post.period[1] : post.period[2]] <- NA
ss <- AddStaticIntercept(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 52)
ss <- AddRandomWalkHoliday(ss, y, holiday=PeriodHol, time0="2019-01-01")

bsts.model <- bsts(y, ss, niter = 50000)
impact <- CausalImpact(bsts.model = bsts.model,
                       post.period.response = post.period.response)
png("/home/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Mapfre/Papers/BSTS_HealthInsurance/Material Suplementari/over60_2019_2021.png")
plot(impact)
dev.off()
summary(impact)

### 2019 vs 2022
y <- over60$Acts
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
png("/home/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Mapfre/Papers/BSTS_HealthInsurance/Material Suplementari/over60_2019_2022.png")
plot(impact)
dev.off()
summary(impact)

### MADRID
madrid <- data_ts %>% filter(Province==28) %>% group_by(Year, WeekNum) %>% 
  summarise(Acts=sum(Acts))

madrid$Date <- seq(as.Date("2019-01-01"), as.Date("2023-01-07"), "weeks")

### Remove first week of 2023
madrid <- madrid[year(madrid$Date)!=2023, ]

### 2019 vs 2020
y <- madrid$Acts[year(madrid$Date)<=2020 ]
x <- totals$Total[year(totals$Date)<=2020 ]
y <- y/x*1000
PeriodHol <- DateRangeHoliday("PeriodHolidays",
                              start = as.Date(c("2019-08-01", "2019-12-25", "2020-08-01", "2020-12-25")),
                              end = as.Date(c("2019-09-01", "2020-01-07", "2020-09-01", "2021-01-07")))

post.period <- c(which(year(madrid$Date)==2020)[1], which(year(madrid$Date)==2020)[length(which(year(madrid$Date)==2020))])
post.period.response <- y[post.period[1] : post.period[2]]
y[post.period[1] : post.period[2]] <- NA
ss <- AddStaticIntercept(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 52)
ss <- AddRandomWalkHoliday(ss, y, holiday=PeriodHol, time0="2019-01-01")
bsts.model <- bsts(y, ss, niter = 50000)
impact <- CausalImpact(bsts.model = bsts.model,
                       post.period.response = post.period.response)
png("/home/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Mapfre/Papers/BSTS_HealthInsurance/Material Suplementari/madrid_2019_2020.png")
plot(impact)
dev.off()
summary(impact)

### 2019 vs 2021
y <- madrid$Acts[year(madrid$Date)!=2022]
x <- totals$Total[year(totals$Date)!=2022]
y <- y/x*1000
PeriodHol <- DateRangeHoliday("PeriodHolidays",
                              start = as.Date(c("2019-08-01", "2019-12-25", "2020-08-01", "2020-12-25", "2021-08-01", "2021-12-25")),
                              end = as.Date(c("2019-09-01", "2020-01-07", "2020-09-01", "2021-01-07", "2021-09-01", "2022-01-07")))
post.period <- c(106, 157)
post.period.response <- y[post.period[1] : post.period[2]]
y[post.period[1] : post.period[2]] <- NA
ss <- AddStaticIntercept(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 52)
ss <- AddRandomWalkHoliday(ss, y, holiday=PeriodHol, time0="2019-01-01")

bsts.model <- bsts(y, ss, niter = 50000)
impact <- CausalImpact(bsts.model = bsts.model,
                       post.period.response = post.period.response)
png("/home/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Mapfre/Papers/BSTS_HealthInsurance/Material Suplementari/madrid_2019_2021.png")
plot(impact)
dev.off()
summary(impact)

### 2019 vs 2022
y <- madrid$Acts
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
png("/home/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Mapfre/Papers/BSTS_HealthInsurance/Material Suplementari/madrid_2019_2022.png")
plot(impact)
dev.off()
summary(impact)

### BARCELONA (Province=08)
barcelona <- data_ts %>% filter(Province==08) %>% group_by(Year, WeekNum) %>% 
  summarise(Acts=sum(Acts))

barcelona$Date <- seq(as.Date("2019-01-01"), as.Date("2023-01-07"), "weeks")

### Remove first week of 2023
barcelona <- barcelona[year(barcelona$Date)!=2023, ]

### 2019 vs 2020
y <- barcelona$Acts[year(barcelona$Date)<=2020 ]
x <- totals$Total[year(totals$Date)<=2020 ]
y <- y/x*1000
PeriodHol <- DateRangeHoliday("PeriodHolidays",
                              start = as.Date(c("2019-08-01", "2019-12-25", "2020-08-01", "2020-12-25")),
                              end = as.Date(c("2019-09-01", "2020-01-07", "2020-09-01", "2021-01-07")))

post.period <- c(which(year(barcelona$Date)==2020)[1], which(year(barcelona$Date)==2020)[length(which(year(barcelona$Date)==2020))])
post.period.response <- y[post.period[1] : post.period[2]]
y[post.period[1] : post.period[2]] <- NA
ss <- AddStaticIntercept(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 52)
ss <- AddRandomWalkHoliday(ss, y, holiday=PeriodHol, time0="2019-01-01")
bsts.model <- bsts(y, ss, niter = 50000)
impact <- CausalImpact(bsts.model = bsts.model,
                       post.period.response = post.period.response)
png("/home/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Mapfre/Papers/BSTS_HealthInsurance/Material Suplementari/barcelona_2019_2020.png")
plot(impact)
dev.off()
summary(impact)

### 2019 vs 2021
y <- barcelona$Acts[year(barcelona$Date)!=2022]
x <- totals$Total[year(totals$Date)!=2022]
y <- y/x*1000
PeriodHol <- DateRangeHoliday("PeriodHolidays",
                              start = as.Date(c("2019-08-01", "2019-12-25", "2020-08-01", "2020-12-25", "2021-08-01", "2021-12-25")),
                              end = as.Date(c("2019-09-01", "2020-01-07", "2020-09-01", "2021-01-07", "2021-09-01", "2022-01-07")))
post.period <- c(106, 157)
post.period.response <- y[post.period[1] : post.period[2]]
y[post.period[1] : post.period[2]] <- NA
ss <- AddStaticIntercept(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 52)
ss <- AddRandomWalkHoliday(ss, y, holiday=PeriodHol, time0="2019-01-01")

bsts.model <- bsts(y, ss, niter = 50000)
impact <- CausalImpact(bsts.model = bsts.model,
                       post.period.response = post.period.response)
png("/home/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Mapfre/Papers/BSTS_HealthInsurance/Material Suplementari/barcelona_2019_2021.png")
plot(impact)
dev.off()
summary(impact)

### 2019 vs 2022
y <- barcelona$Acts
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
png("/home/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Mapfre/Papers/BSTS_HealthInsurance/Material Suplementari/barcelona_2019_2022.png")
plot(impact)
dev.off()
summary(impact)

### VALENCIA (Province=46)
valencia <- data_ts %>% filter(Province==46) %>% group_by(Year, WeekNum) %>% 
  summarise(Acts=sum(Acts))

valencia$Date <- seq(as.Date("2019-01-01"), as.Date("2023-01-07"), "weeks")

### Remove first week of 2023
valencia <- valencia[year(valencia$Date)!=2023, ]

### 2019 vs 2020
y <- valencia$Acts[year(valencia$Date)<=2020 ]
x <- totals$Total[year(totals$Date)<=2020 ]
y <- y/x*1000
PeriodHol <- DateRangeHoliday("PeriodHolidays",
                              start = as.Date(c("2019-08-01", "2019-12-25", "2020-08-01", "2020-12-25")),
                              end = as.Date(c("2019-09-01", "2020-01-07", "2020-09-01", "2021-01-07")))

post.period <- c(which(year(valencia$Date)==2020)[1], which(year(valencia$Date)==2020)[length(which(year(valencia$Date)==2020))])
post.period.response <- y[post.period[1] : post.period[2]]
y[post.period[1] : post.period[2]] <- NA
ss <- AddStaticIntercept(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 52)
ss <- AddRandomWalkHoliday(ss, y, holiday=PeriodHol, time0="2019-01-01")
bsts.model <- bsts(y, ss, niter = 50000)
impact <- CausalImpact(bsts.model = bsts.model,
                       post.period.response = post.period.response)
png("/home/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Mapfre/Papers/BSTS_HealthInsurance/Material Suplementari/valencia_2019_2020.png")
plot(impact)
dev.off()
summary(impact)

### 2019 vs 2021
y <- valencia$Acts[year(valencia$Date)!=2022]
x <- totals$Total[year(totals$Date)!=2022]
y <- y/x*1000
PeriodHol <- DateRangeHoliday("PeriodHolidays",
                              start = as.Date(c("2019-08-01", "2019-12-25", "2020-08-01", "2020-12-25", "2021-08-01", "2021-12-25")),
                              end = as.Date(c("2019-09-01", "2020-01-07", "2020-09-01", "2021-01-07", "2021-09-01", "2022-01-07")))
post.period <- c(106, 157)
post.period.response <- y[post.period[1] : post.period[2]]
y[post.period[1] : post.period[2]] <- NA
ss <- AddStaticIntercept(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 52)
ss <- AddRandomWalkHoliday(ss, y, holiday=PeriodHol, time0="2019-01-01")

bsts.model <- bsts(y, ss, niter = 50000)
impact <- CausalImpact(bsts.model = bsts.model,
                       post.period.response = post.period.response)
png("/home/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Mapfre/Papers/BSTS_HealthInsurance/Material Suplementari/valencia_2019_2021.png")
plot(impact)
dev.off()
summary(impact)

### 2019 vs 2022
y <- valencia$Acts
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
png("/home/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Mapfre/Papers/BSTS_HealthInsurance/Material Suplementari/valencia_2019_2022.png")
plot(impact)
dev.off()
summary(impact)
