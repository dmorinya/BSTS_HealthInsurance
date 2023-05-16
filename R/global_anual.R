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

global <- mapfre_ts %>% group_by(Year, WeekNum) %>%
  summarise(Acts=sum(Acts))

global$Date <- totals$Date

### Remove first week of 2023
global <- global[year(global$Date)!=2023, ]
totals <- totals[year(totals$Date)!=2023, ]

### TOTAL
### 2019 vs 2020
y <- global$Acts[year(global$Date)<=2020 ]
x <- totals$Total[year(totals$Date)<=2020 ]
post.period <- c(which(year(global$Date)==2020)[1], which(year(global$Date)==2020)[length(which(year(global$Date)==2020))])
post.period.response <- y[post.period[1] : post.period[2]]
y[post.period[1] : post.period[2]] <- NA
ss <- AddStaticIntercept(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 52)
bsts.model <- bsts(y~x, ss, niter = 50000)
impact <- CausalImpact(bsts.model = bsts.model,
                       post.period.response = post.period.response)
png("Papers/Paper 4 (BSTS)/MapfreBSTS/Results/global_2019_2020.png")
plot(impact)
dev.off()
summary(impact)

### 2019 vs 2021
y <- global$Acts[year(global$Date)!=2020 & year(global$Date)!=2022]
x <- totals$Total[year(totals$Date)!=2020 & year(totals$Date)!=2022]
post.period <- c(54,105)
post.period.response <- y[post.period[1] : post.period[2]]
y[post.period[1] : post.period[2]] <- NA
ss <- AddStaticIntercept(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 52)
bsts.model <- bsts(y~x, ss, niter = 50000)
impact <- CausalImpact(bsts.model = bsts.model,
                       post.period.response = post.period.response)
png("Papers/Paper 4 (BSTS)/MapfreBSTS/Results/global_2019_2021.png")
plot(impact)
dev.off()
summary(impact)

### 2019 vs 2022
y <- global$Acts[year(global$Date)!=2020 & year(global$Date)!=2021]
x <- totals$Total[year(totals$Date)!=2020 & year(totals$Date)!=2021]
post.period <- c(54,105)
post.period.response <- y[post.period[1] : post.period[2]]
y[post.period[1] : post.period[2]] <- NA
ss <- AddStaticIntercept(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 52)
bsts.model <- bsts(y~x, ss, niter = 50000)
impact <- CausalImpact(bsts.model = bsts.model,
                       post.period.response = post.period.response)
png("Papers/Paper 4 (BSTS)/MapfreBSTS/Results/global_2019_2022.png")
plot(impact)
dev.off()
summary(impact)

rm(global)

### ONLY FEMALES
females <- mapfre_ts %>% filter(Sex==1) %>% group_by(Year, WeekNum) %>% 
  summarise(Acts=sum(Acts))

females$Date <- seq(as.Date("2019-01-01"), as.Date("2023-01-07"), "weeks")

### Remove first week of 2023
females <- females[year(females$Date)!=2023, ]

### 2019 vs 2020
y <- females$Acts[year(females$Date)<=2020 ]
x <- totals$Total[year(totals$Date)<=2020 ]
post.period <- c(which(year(females$Date)==2020)[1], which(year(females$Date)==2020)[length(which(year(females$Date)==2020))])
post.period.response <- y[post.period[1] : post.period[2]]
y[post.period[1] : post.period[2]] <- NA
ss <- AddStaticIntercept(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 52)
bsts.model <- bsts(y~x, ss, niter = 50000)
impact <- CausalImpact(bsts.model = bsts.model,
                       post.period.response = post.period.response)
png("Papers/Paper 4 (BSTS)/MapfreBSTS/Results/females_2019_2020.png")
plot(impact)
dev.off()
summary(impact)

### 2019 vs 2021
y <- females$Acts[year(females$Date)!=2020 & year(females$Date)!=2022]
x <- totals$Total[year(totals$Date)!=2020 & year(totals$Date)!=2022]
post.period <- c(54,105)
post.period.response <- y[post.period[1] : post.period[2]]
y[post.period[1] : post.period[2]] <- NA
ss <- AddStaticIntercept(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 52)
bsts.model <- bsts(y~x, ss, niter = 50000)
impact <- CausalImpact(bsts.model = bsts.model,
                       post.period.response = post.period.response)
png("Papers/Paper 4 (BSTS)/MapfreBSTS/Results/females_2019_2021.png")
plot(impact)
dev.off()
summary(impact)

### 2019 vs 2022
y <- females$Acts[year(females$Date)!=2020 & year(females$Date)!=2021]
x <- totals$Total[year(totals$Date)!=2020 & year(totals$Date)!=2021]
post.period <- c(54,105)
post.period.response <- y[post.period[1] : post.period[2]]
y[post.period[1] : post.period[2]] <- NA
ss <- AddStaticIntercept(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 52)
bsts.model <- bsts(y~x, ss, niter = 50000)
impact <- CausalImpact(bsts.model = bsts.model,
                       post.period.response = post.period.response)
png("Papers/Paper 4 (BSTS)/MapfreBSTS/Results/females_2019_2022.png")
plot(impact)
dev.off()
summary(impact)

### ONLY MALES (Sex=2)
males <- mapfre_ts %>% filter(Sex==2) %>% group_by(Year, WeekNum) %>% 
  summarise(Acts=sum(Acts))

males$Date <- seq(as.Date("2019-01-01"), as.Date("2023-01-07"), "weeks")

### Remove first week of 2023
males <- males[year(males$Date)!=2023, ]

### 2019 vs 2020
y <- males$Acts[year(males$Date)<=2020 ]
x <- totals$Total[year(totals$Date)<=2020 ]
post.period <- c(which(year(males$Date)==2020)[1], which(year(males$Date)==2020)[length(which(year(males$Date)==2020))])
post.period.response <- y[post.period[1] : post.period[2]]
y[post.period[1] : post.period[2]] <- NA
ss <- AddStaticIntercept(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 52)
bsts.model <- bsts(y~x, ss, niter = 50000)
impact <- CausalImpact(bsts.model = bsts.model,
                       post.period.response = post.period.response)
png("Papers/Paper 4 (BSTS)/MapfreBSTS/Results/males_2019_2020.png")
plot(impact)
dev.off()
summary(impact)

### 2019 vs 2021
y <- males$Acts[year(males$Date)!=2020 & year(males$Date)!=2022]
x <- totals$Total[year(totals$Date)!=2020 & year(totals$Date)!=2022]
post.period <- c(54,105)
post.period.response <- y[post.period[1] : post.period[2]]
y[post.period[1] : post.period[2]] <- NA
ss <- AddStaticIntercept(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 52)
bsts.model <- bsts(y~x, ss, niter = 50000)
impact <- CausalImpact(bsts.model = bsts.model,
                       post.period.response = post.period.response)
png("Papers/Paper 4 (BSTS)/MapfreBSTS/Results/males_2019_2021.png")
plot(impact)
dev.off()
summary(impact)

### 2019 vs 2022
y <- males$Acts[year(males$Date)!=2020 & year(males$Date)!=2021]
x <- totals$Total[year(totals$Date)!=2020 & year(totals$Date)!=2021]
post.period <- c(54,105)
post.period.response <- y[post.period[1] : post.period[2]]
y[post.period[1] : post.period[2]] <- NA
ss <- AddStaticIntercept(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 52)
bsts.model <- bsts(y~x, ss, niter = 50000)
impact <- CausalImpact(bsts.model = bsts.model,
                       post.period.response = post.period.response)
png("Papers/Paper 4 (BSTS)/MapfreBSTS/Results/males_2019_2022.png")
plot(impact)
dev.off()
summary(impact)

### OVER 60
over60 <- mapfre_ts %>% filter(Age=="(59, 200]") %>% group_by(Year, WeekNum) %>% 
  summarise(Acts=sum(Acts))

over60$Date <- seq(as.Date("2019-01-01"), as.Date("2023-01-07"), "weeks")

### Remove first week of 2023
over60 <- over60[year(over60$Date)!=2023, ]

### 2019 vs 2020
y <- over60$Acts[year(over60$Date)<=2020 ]
x <- totals$Total[year(totals$Date)<=2020 ]
post.period <- c(which(year(over60$Date)==2020)[1], which(year(over60$Date)==2020)[length(which(year(over60$Date)==2020))])
post.period.response <- y[post.period[1] : post.period[2]]
y[post.period[1] : post.period[2]] <- NA
ss <- AddStaticIntercept(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 52)
bsts.model <- bsts(y~x, ss, niter = 50000)
impact <- CausalImpact(bsts.model = bsts.model,
                       post.period.response = post.period.response)
png("Papers/Paper 4 (BSTS)/MapfreBSTS/Results/over60_2019_2020.png")
plot(impact)
dev.off()
summary(impact)

### 2019 vs 2021
y <- over60$Acts[year(over60$Date)!=2020 & year(over60$Date)!=2022]
x <- totals$Total[year(totals$Date)!=2020 & year(totals$Date)!=2022]
post.period <- c(54,105)
post.period.response <- y[post.period[1] : post.period[2]]
y[post.period[1] : post.period[2]] <- NA
ss <- AddStaticIntercept(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 52)
bsts.model <- bsts(y~x, ss, niter = 50000)
impact <- CausalImpact(bsts.model = bsts.model,
                       post.period.response = post.period.response)
png("Papers/Paper 4 (BSTS)/MapfreBSTS/Results/over60_2019_2021.png")
plot(impact)
dev.off()
summary(impact)

### 2019 vs 2022
y <- over60$Acts[year(over60$Date)!=2020 & year(over60$Date)!=2021]
x <- totals$Total[year(totals$Date)!=2020 & year(totals$Date)!=2021]
post.period <- c(54,105)
post.period.response <- y[post.period[1] : post.period[2]]
y[post.period[1] : post.period[2]] <- NA
ss <- AddStaticIntercept(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 52)
bsts.model <- bsts(y~x, ss, niter = 50000)
impact <- CausalImpact(bsts.model = bsts.model,
                       post.period.response = post.period.response)
png("Papers/Paper 4 (BSTS)/MapfreBSTS/Results/over60_2019_2022.png")
plot(impact)
dev.off()
summary(impact)

### MADRID
madrid <- mapfre_ts %>% filter(Province==28) %>% group_by(Year, WeekNum) %>% 
  summarise(Acts=sum(Acts))

madrid$Date <- seq(as.Date("2019-01-01"), as.Date("2023-01-07"), "weeks")

### Remove first week of 2023
madrid <- madrid[year(madrid$Date)!=2023, ]

### 2019 vs 2020
y <- madrid$Acts[year(madrid$Date)<=2020 ]
x <- totals$Total[year(totals$Date)<=2020 ]
post.period <- c(which(year(madrid$Date)==2020)[1], which(year(madrid$Date)==2020)[length(which(year(madrid$Date)==2020))])
post.period.response <- y[post.period[1] : post.period[2]]
y[post.period[1] : post.period[2]] <- NA
ss <- AddStaticIntercept(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 52)
bsts.model <- bsts(y~x, ss, niter = 50000)
impact <- CausalImpact(bsts.model = bsts.model,
                       post.period.response = post.period.response)
png("Papers/Paper 4 (BSTS)/MapfreBSTS/Results/madrid_2019_2020.png")
plot(impact)
dev.off()
summary(impact)

### 2019 vs 2021
y <- madrid$Acts[year(madrid$Date)!=2020 & year(madrid$Date)!=2022]
x <- totals$Total[year(totals$Date)!=2020 & year(totals$Date)!=2022]
post.period <- c(54,105)
post.period.response <- y[post.period[1] : post.period[2]]
y[post.period[1] : post.period[2]] <- NA
ss <- AddStaticIntercept(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 52)
bsts.model <- bsts(y~x, ss, niter = 50000)
impact <- CausalImpact(bsts.model = bsts.model,
                       post.period.response = post.period.response)
png("Papers/Paper 4 (BSTS)/MapfreBSTS/Results/madrid_2019_2021.png")
plot(impact)
dev.off()
summary(impact)

### 2019 vs 2022
y <- madrid$Acts[year(madrid$Date)!=2020 & year(madrid$Date)!=2021]
x <- totals$Total[year(totals$Date)!=2020 & year(totals$Date)!=2021]
post.period <- c(54,105)
post.period.response <- y[post.period[1] : post.period[2]]
y[post.period[1] : post.period[2]] <- NA
ss <- AddStaticIntercept(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 52)
bsts.model <- bsts(y~x, ss, niter = 50000)
impact <- CausalImpact(bsts.model = bsts.model,
                       post.period.response = post.period.response)
png("Papers/Paper 4 (BSTS)/MapfreBSTS/Results/madrid_2019_2022.png")
plot(impact)
dev.off()
summary(impact)

### BARCELONA (Province=08)
barcelona <- mapfre_ts %>% filter(Province==08) %>% group_by(Year, WeekNum) %>% 
  summarise(Acts=sum(Acts))

barcelona$Date <- seq(as.Date("2019-01-01"), as.Date("2023-01-07"), "weeks")

### Remove first week of 2023
barcelona <- barcelona[year(barcelona$Date)!=2023, ]

### 2019 vs 2020
y <- barcelona$Acts[year(barcelona$Date)<=2020 ]
x <- totals$Total[year(totals$Date)<=2020 ]
post.period <- c(which(year(barcelona$Date)==2020)[1], which(year(barcelona$Date)==2020)[length(which(year(barcelona$Date)==2020))])
post.period.response <- y[post.period[1] : post.period[2]]
y[post.period[1] : post.period[2]] <- NA
ss <- AddStaticIntercept(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 52)
bsts.model <- bsts(y~x, ss, niter = 50000)
impact <- CausalImpact(bsts.model = bsts.model,
                       post.period.response = post.period.response)
png("Papers/Paper 4 (BSTS)/MapfreBSTS/Results/barcelona_2019_2020.png")
plot(impact)
dev.off()
summary(impact)

### 2019 vs 2021
y <- barcelona$Acts[year(barcelona$Date)!=2020 & year(barcelona$Date)!=2022]
x <- totals$Total[year(totals$Date)!=2020 & year(totals$Date)!=2022]
post.period <- c(54,105)
post.period.response <- y[post.period[1] : post.period[2]]
y[post.period[1] : post.period[2]] <- NA
ss <- AddStaticIntercept(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 52)
bsts.model <- bsts(y~x, ss, niter = 50000)
impact <- CausalImpact(bsts.model = bsts.model,
                       post.period.response = post.period.response)
png("Papers/Paper 4 (BSTS)/MapfreBSTS/Results/barcelona_2019_2021.png")
plot(impact)
dev.off()
summary(impact)

### 2019 vs 2022
y <- barcelona$Acts[year(barcelona$Date)!=2020 & year(barcelona$Date)!=2021]
x <- totals$Total[year(totals$Date)!=2020 & year(totals$Date)!=2021]
post.period <- c(54,105)
post.period.response <- y[post.period[1] : post.period[2]]
y[post.period[1] : post.period[2]] <- NA
ss <- AddStaticIntercept(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 52)
bsts.model <- bsts(y~x, ss, niter = 50000)
impact <- CausalImpact(bsts.model = bsts.model,
                       post.period.response = post.period.response)
png("Papers/Paper 4 (BSTS)/MapfreBSTS/Results/barcelona_2019_2022.png")
plot(impact)
dev.off()
summary(impact)

### VALENCIA (Province=46)
valencia <- mapfre_ts %>% filter(Province==46) %>% group_by(Year, WeekNum) %>% 
  summarise(Acts=sum(Acts))

valencia$Date <- seq(as.Date("2019-01-01"), as.Date("2023-01-07"), "weeks")

### Remove first week of 2023
valencia <- valencia[year(valencia$Date)!=2023, ]

### 2019 vs 2020
y <- valencia$Acts[year(valencia$Date)<=2020 ]
x <- totals$Total[year(totals$Date)<=2020 ]
post.period <- c(which(year(valencia$Date)==2020)[1], which(year(valencia$Date)==2020)[length(which(year(valencia$Date)==2020))])
post.period.response <- y[post.period[1] : post.period[2]]
y[post.period[1] : post.period[2]] <- NA
ss <- AddStaticIntercept(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 52)
bsts.model <- bsts(y~x, ss, niter = 50000)
impact <- CausalImpact(bsts.model = bsts.model,
                       post.period.response = post.period.response)
png("Papers/Paper 4 (BSTS)/MapfreBSTS/Results/valencia_2019_2020.png")
plot(impact)
dev.off()
summary(impact)

### 2019 vs 2021
y <- valencia$Acts[year(valencia$Date)!=2020 & year(valencia$Date)!=2022]
x <- totals$Total[year(totals$Date)!=2020 & year(totals$Date)!=2022]
post.period <- c(54,105)
post.period.response <- y[post.period[1] : post.period[2]]
y[post.period[1] : post.period[2]] <- NA
ss <- AddStaticIntercept(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 52)
bsts.model <- bsts(y~x, ss, niter = 50000)
impact <- CausalImpact(bsts.model = bsts.model,
                       post.period.response = post.period.response)
png("Papers/Paper 4 (BSTS)/MapfreBSTS/Results/valencia_2019_2021.png")
plot(impact)
dev.off()
summary(impact)

### 2019 vs 2022
y <- valencia$Acts[year(valencia$Date)!=2020 & year(valencia$Date)!=2021]
x <- totals$Total[year(totals$Date)!=2020 & year(totals$Date)!=2021]
post.period <- c(54,105)
post.period.response <- y[post.period[1] : post.period[2]]
y[post.period[1] : post.period[2]] <- NA
ss <- AddStaticIntercept(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 52)
bsts.model <- bsts(y~x, ss, niter = 50000)
impact <- CausalImpact(bsts.model = bsts.model,
                       post.period.response = post.period.response)
png("Papers/Paper 4 (BSTS)/MapfreBSTS/Results/valencia_2019_2022.png")
plot(impact)
dev.off()
summary(impact)
