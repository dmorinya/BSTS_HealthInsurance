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
                   totals$Date <= "2023-01-07", ]

df <- approx(x=totals$Date,y=totals$Total, xout=seq(as.Date("2019-01-01"), as.Date("2023-01-07"), "weeks"))
totals <- data.frame(Date=df$x, Total=df$y)

urology <- mapfre_ts[mapfre_ts$Code_Real==41 & mapfre_ts$Code_Esp==41 & mapfre_ts$Code_Group=="10100" & mapfre_ts$Id== 1, ]

urology <- urology %>% group_by(Year, WeekNum) %>% summarise(Acts=sum(Acts)) ### Remove duplicates

urology$Date <- totals$Date

### Remove first week of 2023
urology <- urology[year(urology$Date)!=2023, ]
totals <- totals[year(totals$Date)!=2023, ]

### 2019 vs 2020
y <- urology$Acts[year(urology$Date)<=2020 ]
x <- totals$Total[year(totals$Date)<=2020 ]
post.period <- c(which(year(urology$Date)==2020)[1], which(year(urology$Date)==2020)[length(which(year(urology$Date)==2020))])
post.period.response <- y[post.period[1] : post.period[2]]
y[post.period[1] : post.period[2]] <- NA
ss <- AddStaticIntercept(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 52)
bsts.model <- bsts(y~x, ss, niter = 50000)
impact <- CausalImpact(bsts.model = bsts.model,
                       post.period.response = post.period.response)
png("Papers/Paper 4 (BSTS)/MapfreBSTS/Results/urology_2019_2020.png")
plot(impact)
dev.off()
summary(impact)

### 2019 vs 2021
y <- urology$Acts[year(urology$Date)!=2020 & year(urology$Date)!=2022]
x <- totals$Total[year(totals$Date)!=2020 & year(totals$Date)!=2022]
post.period <- c(54,105)
post.period.response <- y[post.period[1] : post.period[2]]
y[post.period[1] : post.period[2]] <- NA
ss <- AddStaticIntercept(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 52)
bsts.model <- bsts(y~x, ss, niter = 50000)
impact <- CausalImpact(bsts.model = bsts.model,
                       post.period.response = post.period.response)
png("Papers/Paper 4 (BSTS)/MapfreBSTS/Results/urology_2019_2021.png")
plot(impact)
dev.off()
summary(impact)

### 2019 vs 2022
y <- urology$Acts[year(urology$Date)!=2020 & year(urology$Date)!=2021]
x <- totals$Total[year(totals$Date)!=2020 & year(totals$Date)!=2021]
post.period <- c(54,105)
post.period.response <- y[post.period[1] : post.period[2]]
y[post.period[1] : post.period[2]] <- NA
ss <- AddStaticIntercept(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 52)
bsts.model <- bsts(y~x, ss, niter = 50000)
impact <- CausalImpact(bsts.model = bsts.model,
                       post.period.response = post.period.response)
png("Papers/Paper 4 (BSTS)/MapfreBSTS/Results/urology_2019_2022.png")
plot(impact)
dev.off()
summary(impact)
