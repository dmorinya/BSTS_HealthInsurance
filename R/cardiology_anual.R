library(readxl)
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(readxl)
library(lubridate)
library(CausalImpact)
library(MMWRweek)

data_ts <- read_csv(unzip("Weekly_data.zip", "Weekly_data.csv"))
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

cardiology <- data_ts[data_ts$Code_Real==9 & data_ts$Code_Esp==9 & data_ts$Code_Group=="10100" & data_ts$Id== 1, ]

cardiology <- cardiology %>% group_by(Year, WeekNum) %>% summarise(Acts=sum(Acts)) ### Remove duplicates

cardiology$Date <- totals$Date

### Remove first week of 2023
cardiology <- cardiology[year(cardiology$Date)!=2023, ]
totals <- totals[year(totals$Date)!=2023, ]

### 2019 vs 2020
y <- cardiology$Acts[year(cardiology$Date)<=2020 ]
x <- totals$Total[year(totals$Date)<=2020 ]
post.period <- c(which(year(cardiology$Date)==2020)[1], which(year(cardiology$Date)==2020)[length(which(year(cardiology$Date)==2020))])
post.period.response <- y[post.period[1] : post.period[2]]
y[post.period[1] : post.period[2]] <- NA
ss <- AddStaticIntercept(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 52)
bsts.model <- bsts(y~x, ss, niter = 50000)
impact <- CausalImpact(bsts.model = bsts.model,
                       post.period.response = post.period.response)
png("Results/cardiology_2019_2020.png")
plot(impact)
dev.off()
summary(impact)

### 2019 vs 2021
y <- cardiology$Acts[year(cardiology$Date)!=2020 & year(cardiology$Date)!=2022]
x <- totals$Total[year(totals$Date)!=2020 & year(cardiology$Date)!=2022]
post.period <- c(54,105)
post.period.response <- y[post.period[1] : post.period[2]]
y[post.period[1] : post.period[2]] <- NA
ss <- AddStaticIntercept(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 52)
bsts.model <- bsts(y~x, ss, niter = 50000)
impact <- CausalImpact(bsts.model = bsts.model,
                       post.period.response = post.period.response)
png("Results/cardiology_2019_2021.png")
plot(impact)
dev.off()
summary(impact)

### 2019 vs 2022
y <- cardiology$Acts[year(cardiology$Date)!=2020 & year(cardiology$Date)!=2021]
x <- totals$Total[year(totals$Date)!=2020 & year(totals$Date)!=2021]
post.period <- c(54,105)
post.period.response <- y[post.period[1] : post.period[2]]
y[post.period[1] : post.period[2]] <- NA
ss <- AddStaticIntercept(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 52)
bsts.model <- bsts(y~x, ss, niter = 50000)
impact <- CausalImpact(bsts.model = bsts.model,
                       post.period.response = post.period.response)
png("Papers/Paper 4 (BSTS)/Results/cardiology_2019_2022.png")
plot(impact)
dev.off()
summary(impact)

### Cardiology by age group and sex
cardiology <- data_ts[data_ts$Code_Real==9 & data_ts$Code_Esp==9 & data_ts$Code_Group=="10100" & data_ts$Id== 1, ]
cardiology$AgeCAT <- ifelse(cardiology$Age=="(59, 200]", 1, 0)

### females under 60
cardiology <- cardiology %>% filter(Sex==1 & AgeCAT==0) %>% group_by(Year, WeekNum) %>% summarise(Acts=sum(Acts)) ### Remove duplicates
cardiology$Date <- totals$Date
### 2019 vs 2020
y <- cardiology$Acts[year(cardiology$Date)<=2020 ]
x <- totals$Total[year(totals$Date)<=2020 ]
post.period <- c(which(year(cardiology$Date)==2020)[1], which(year(cardiology$Date)==2020)[length(which(year(cardiology$Date)==2020))])
post.period.response <- y[post.period[1] : post.period[2]]
y[post.period[1] : post.period[2]] <- NA
ss <- AddStaticIntercept(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 52)
bsts.model <- bsts(y~x, ss, niter = 50000)
impact <- CausalImpact(bsts.model = bsts.model,
                       post.period.response = post.period.response)
summary(impact)

### 2019 vs 2021
y <- cardiology$Acts[year(cardiology$Date)!=2020 & year(cardiology$Date)!=2022]
x <- totals$Total[year(totals$Date)!=2020 & year(cardiology$Date)!=2022]
post.period <- c(54,105)
post.period.response <- y[post.period[1] : post.period[2]]
y[post.period[1] : post.period[2]] <- NA
ss <- AddStaticIntercept(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 52)
bsts.model <- bsts(y~x, ss, niter = 50000)
impact <- CausalImpact(bsts.model = bsts.model,
                       post.period.response = post.period.response)
summary(impact)

### 2019 vs 2022
y <- cardiology$Acts[year(cardiology$Date)!=2020 & year(cardiology$Date)!=2021]
x <- totals$Total[year(totals$Date)!=2020 & year(totals$Date)!=2021]
post.period <- c(54,105)
post.period.response <- y[post.period[1] : post.period[2]]
y[post.period[1] : post.period[2]] <- NA
ss <- AddStaticIntercept(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 52)
bsts.model <- bsts(y~x, ss, niter = 50000)
impact <- CausalImpact(bsts.model = bsts.model,
                       post.period.response = post.period.response)
summary(impact)

### Cardiology by age group and sex
cardiology <- data_ts[data_ts$Code_Real==9 & data_ts$Code_Esp==9 & data_ts$Code_Group=="10100" & data_ts$Id== 1, ]
cardiology$AgeCAT <- ifelse(cardiology$Age=="(59, 200]", 1, 0)

### females over 60
cardiology <- cardiology %>% filter(Sex==1 & AgeCAT==1) %>% group_by(Year, WeekNum) %>% summarise(Acts=sum(Acts)) ### Remove duplicates
cardiology$Date <- totals$Date
### 2019 vs 2020
y <- cardiology$Acts[year(cardiology$Date)<=2020 ]
x <- totals$Total[year(totals$Date)<=2020 ]
post.period <- c(which(year(cardiology$Date)==2020)[1], which(year(cardiology$Date)==2020)[length(which(year(cardiology$Date)==2020))])
post.period.response <- y[post.period[1] : post.period[2]]
y[post.period[1] : post.period[2]] <- NA
ss <- AddStaticIntercept(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 52)
bsts.model <- bsts(y~x, ss, niter = 50000)
impact <- CausalImpact(bsts.model = bsts.model,
                       post.period.response = post.period.response)
summary(impact)

### 2019 vs 2021
y <- cardiology$Acts[year(cardiology$Date)!=2020 & year(cardiology$Date)!=2022]
x <- totals$Total[year(totals$Date)!=2020 & year(cardiology$Date)!=2022]
post.period <- c(54,105)
post.period.response <- y[post.period[1] : post.period[2]]
y[post.period[1] : post.period[2]] <- NA
ss <- AddStaticIntercept(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 52)
bsts.model <- bsts(y~x, ss, niter = 50000)
impact <- CausalImpact(bsts.model = bsts.model,
                       post.period.response = post.period.response)
summary(impact)

### 2019 vs 2022
y <- cardiology$Acts[year(cardiology$Date)!=2020 & year(cardiology$Date)!=2021]
x <- totals$Total[year(totals$Date)!=2020 & year(totals$Date)!=2021]
post.period <- c(54,105)
post.period.response <- y[post.period[1] : post.period[2]]
y[post.period[1] : post.period[2]] <- NA
ss <- AddStaticIntercept(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 52)
bsts.model <- bsts(y~x, ss, niter = 50000)
impact <- CausalImpact(bsts.model = bsts.model,
                       post.period.response = post.period.response)
summary(impact)

### Cardiology by age group and sex
cardiology <- data_ts[data_ts$Code_Real==9 & data_ts$Code_Esp==9 & data_ts$Code_Group=="10100" & data_ts$Id== 1, ]
cardiology$AgeCAT <- ifelse(cardiology$Age=="(59, 200]", 1, 0)

### males under 60
cardiology <- cardiology %>% filter(Sex==2 & AgeCAT==0) %>% group_by(Year, WeekNum) %>% summarise(Acts=sum(Acts)) ### Remove duplicates
cardiology$Date <- totals$Date
### 2019 vs 2020
y <- cardiology$Acts[year(cardiology$Date)<=2020 ]
x <- totals$Total[year(totals$Date)<=2020 ]
post.period <- c(which(year(cardiology$Date)==2020)[1], which(year(cardiology$Date)==2020)[length(which(year(cardiology$Date)==2020))])
post.period.response <- y[post.period[1] : post.period[2]]
y[post.period[1] : post.period[2]] <- NA
ss <- AddStaticIntercept(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 52)
bsts.model <- bsts(y~x, ss, niter = 50000)
impact <- CausalImpact(bsts.model = bsts.model,
                       post.period.response = post.period.response)
summary(impact)

### 2019 vs 2021
y <- cardiology$Acts[year(cardiology$Date)!=2020 & year(cardiology$Date)!=2022]
x <- totals$Total[year(totals$Date)!=2020 & year(cardiology$Date)!=2022]
post.period <- c(54,105)
post.period.response <- y[post.period[1] : post.period[2]]
y[post.period[1] : post.period[2]] <- NA
ss <- AddStaticIntercept(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 52)
bsts.model <- bsts(y~x, ss, niter = 50000)
impact <- CausalImpact(bsts.model = bsts.model,
                       post.period.response = post.period.response)
summary(impact)

### 2019 vs 2022
y <- cardiology$Acts[year(cardiology$Date)!=2020 & year(cardiology$Date)!=2021]
x <- totals$Total[year(totals$Date)!=2020 & year(totals$Date)!=2021]
post.period <- c(54,105)
post.period.response <- y[post.period[1] : post.period[2]]
y[post.period[1] : post.period[2]] <- NA
ss <- AddStaticIntercept(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 52)
bsts.model <- bsts(y~x, ss, niter = 50000)
impact <- CausalImpact(bsts.model = bsts.model,
                       post.period.response = post.period.response)
summary(impact)

### Cardiology by age group and sex
cardiology <- data_ts[data_ts$Code_Real==9 & data_ts$Code_Esp==9 & data_ts$Code_Group=="10100" & data_ts$Id== 1, ]
cardiology$AgeCAT <- ifelse(cardiology$Age=="(59, 200]", 1, 0)

### males over 60
cardiology <- cardiology %>% filter(Sex==2 & AgeCAT==1) %>% group_by(Year, WeekNum) %>% summarise(Acts=sum(Acts)) ### Remove duplicates
totals <- read_xlsx("S67_IAL_CUBO_CARTERA_HISTORICA_SALUD_FAMILIAS_2023_febrero.xlsx",
                    sheet="cartera 2023-febrero")
colnames(totals) <- c("Date", "No.Fun", "Fun", "Dental", "Reimb", "Ind", "Total")
totals$Date <- paste0("01/", totals$Date)
totals$Date <- as.Date(totals$Date, "%d/%m/%Y")
totals <- totals[totals$Date >= "2019-01-01" &
                   totals$Date <= "2023-01-07", ]

df <- approx(x=totals$Date,y=totals$Total, xout=seq(as.Date("2019-01-01"), as.Date("2023-01-07"), "weeks"))
totals <- data.frame(Date=df$x, Total=df$y)

cardiology$Date <- totals$Date

### Remove first week of 2023
cardiology <- cardiology[year(cardiology$Date)!=2023, ]
totals <- totals[year(totals$Date)!=2023, ]

### 2019 vs 2020
y <- cardiology$Acts[year(cardiology$Date)<=2020 ]
x <- totals$Total[year(totals$Date)<=2020 ]
post.period <- c(which(year(cardiology$Date)==2020)[1], which(year(cardiology$Date)==2020)[length(which(year(cardiology$Date)==2020))])
post.period.response <- y[post.period[1] : post.period[2]]
y[post.period[1] : post.period[2]] <- NA
ss <- AddStaticIntercept(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 52)
bsts.model <- bsts(y~x, ss, niter = 50000)
impact <- CausalImpact(bsts.model = bsts.model,
                       post.period.response = post.period.response)
summary(impact)

### 2019 vs 2021
y <- cardiology$Acts[year(cardiology$Date)!=2020 & year(cardiology$Date)!=2022]
x <- totals$Total[year(totals$Date)!=2020 & year(cardiology$Date)!=2022]
post.period <- c(54,105)
post.period.response <- y[post.period[1] : post.period[2]]
y[post.period[1] : post.period[2]] <- NA
ss <- AddStaticIntercept(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 52)
bsts.model <- bsts(y~x, ss, niter = 50000)
impact <- CausalImpact(bsts.model = bsts.model,
                       post.period.response = post.period.response)
summary(impact)

### 2019 vs 2022
y <- cardiology$Acts[year(cardiology$Date)!=2020 & year(cardiology$Date)!=2021]
x <- totals$Total[year(totals$Date)!=2020 & year(totals$Date)!=2021]
post.period <- c(54,105)
post.period.response <- y[post.period[1] : post.period[2]]
y[post.period[1] : post.period[2]] <- NA
ss <- AddStaticIntercept(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 52)
bsts.model <- bsts(y~x, ss, niter = 50000)
impact <- CausalImpact(bsts.model = bsts.model,
                       post.period.response = post.period.response)
summary(impact)
