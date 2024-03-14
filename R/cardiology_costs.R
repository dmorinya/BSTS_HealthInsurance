library(readxl)
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(readxl)
library(lubridate)
library(MMWRweek)
library(VGAM)

setwd("/home/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Original Data/")
data_poiss <- read_csv("MAPFRE_Poisson_cardiology_data.csv")
colnames(data_poiss) <- c("Year", "Policy", "Sex", "Age", "Acts")
global <- data_poiss %>% group_by(Year, Policy, Sex, Age) %>%
  summarise(Acts=sum(Acts))
global <- global[!is.na(global$Sex), ]

### Keep only individuals over (or equal) 30
global <- global[global$Age >= 30, ]

### Categorize age in <= 60 / >60
global$AgeCAT <- ifelse(global$Age <= 60, 0, 1)
global$Age <- NULL
global$Sex <- global$Sex - 1

### Policy number as a string
global$Policy <- as.character(global$Policy)

### Reorder column
global <- global[, c(1, 2, 3, 5, 4)]

### Generate the zeros
### Model for each year 
mod_glob_year2019 <- vglm(Acts~Sex+AgeCAT, family = pospoisson(), data = global[global$Year==2019,]); summary(mod_glob_year2019)
mod_glob_year2020 <- vglm(Acts~Sex+AgeCAT, family = pospoisson(), data = global[global$Year==2020,]); summary(mod_glob_year2020)
mod_glob_year2021 <- vglm(Acts~Sex+AgeCAT, family = pospoisson(), data = global[global$Year==2021,]); summary(mod_glob_year2021)
mod_glob_year2022 <- vglm(Acts~Sex+AgeCAT, family = pospoisson(), data = global[global$Year==2022,]); summary(mod_glob_year2022)
#### Males over 60
mu_1_2019 <- exp(coef(mod_glob_year2019)[1]+coef(mod_glob_year2019)[2]+coef(mod_glob_year2019)[3])
mu_1_2020 <- exp(coef(mod_glob_year2020)[1]+coef(mod_glob_year2020)[2]+coef(mod_glob_year2020)[3])
mu_1_2021 <- exp(coef(mod_glob_year2021)[1]+coef(mod_glob_year2021)[2]+coef(mod_glob_year2021)[3])
mu_1_2022 <- exp(coef(mod_glob_year2022)[1]+coef(mod_glob_year2022)[2]+coef(mod_glob_year2022)[3])

n0_1_2019 <- (exp(-mu_1_2019)/(1-exp(-mu_1_2019)))*dim(global[global$Sex==1 & global$AgeCAT==1 & global$Year==2019, ])[1]
n0_1_2020 <- (exp(-mu_1_2020)/(1-exp(-mu_1_2020)))*dim(global[global$Sex==1 & global$AgeCAT==1 & global$Year==2020, ])[1]
n0_1_2021 <- (exp(-mu_1_2021)/(1-exp(-mu_1_2021)))*dim(global[global$Sex==1 & global$AgeCAT==1 & global$Year==2021, ])[1]
n0_1_2022 <- (exp(-mu_1_2022)/(1-exp(-mu_1_2022)))*dim(global[global$Sex==1 & global$AgeCAT==1 & global$Year==2022, ])[1]

#### Males under 60
mu_2_2019 <- exp(coef(mod_glob_year2019)[1]+coef(mod_glob_year2019)[2])
mu_2_2020 <- exp(coef(mod_glob_year2020)[1]+coef(mod_glob_year2020)[2])
mu_2_2021 <- exp(coef(mod_glob_year2021)[1]+coef(mod_glob_year2021)[2])
mu_2_2022 <- exp(coef(mod_glob_year2022)[1]+coef(mod_glob_year2022)[2])

n0_2_2019 <- (exp(-mu_2_2019)/(1-exp(-mu_2_2019)))*dim(global[global$Sex==1 & global$AgeCAT==0 & global$Year==2019, ])[1]
n0_2_2020 <- (exp(-mu_2_2020)/(1-exp(-mu_2_2020)))*dim(global[global$Sex==1 & global$AgeCAT==0 & global$Year==2020, ])[1]
n0_2_2021 <- (exp(-mu_2_2021)/(1-exp(-mu_2_2021)))*dim(global[global$Sex==1 & global$AgeCAT==0 & global$Year==2021, ])[1]
n0_2_2022 <- (exp(-mu_2_2022)/(1-exp(-mu_2_2022)))*dim(global[global$Sex==1 & global$AgeCAT==0 & global$Year==2022, ])[1]

#### Females over 60
mu_3_2019 <- exp(coef(mod_glob_year2019)[1]+coef(mod_glob_year2019)[3])
mu_3_2020 <- exp(coef(mod_glob_year2020)[1]+coef(mod_glob_year2020)[3])
mu_3_2021 <- exp(coef(mod_glob_year2021)[1]+coef(mod_glob_year2021)[3])
mu_3_2022 <- exp(coef(mod_glob_year2022)[1]+coef(mod_glob_year2022)[3])

n0_3_2019 <- (exp(-mu_3_2019)/(1-exp(-mu_3_2019)))*dim(global[global$Sex==0 & global$AgeCAT==1 & global$Year==2019, ])[1]
n0_3_2020 <- (exp(-mu_3_2020)/(1-exp(-mu_3_2020)))*dim(global[global$Sex==0 & global$AgeCAT==1 & global$Year==2020, ])[1]
n0_3_2021 <- (exp(-mu_3_2021)/(1-exp(-mu_3_2021)))*dim(global[global$Sex==0 & global$AgeCAT==1 & global$Year==2021, ])[1]
n0_3_2022 <- (exp(-mu_3_2022)/(1-exp(-mu_3_2022)))*dim(global[global$Sex==0 & global$AgeCAT==1 & global$Year==2022, ])[1]

#### Females under 60
mu_4_2019 <- exp(coef(mod_glob_year2019)[1])
mu_4_2020 <- exp(coef(mod_glob_year2020)[1])
mu_4_2021 <- exp(coef(mod_glob_year2021)[1])
mu_4_2022 <- exp(coef(mod_glob_year2022)[1])

n0_4_2019 <- (exp(-mu_4_2019)/(1-exp(-mu_4_2019)))*dim(global[global$Sex==0 & global$AgeCAT==0 & global$Year==2019, ])[1]
n0_4_2020 <- (exp(-mu_4_2020)/(1-exp(-mu_4_2020)))*dim(global[global$Sex==0 & global$AgeCAT==0 & global$Year==2020, ])[1]
n0_4_2021 <- (exp(-mu_4_2021)/(1-exp(-mu_4_2021)))*dim(global[global$Sex==0 & global$AgeCAT==0 & global$Year==2021, ])[1]
n0_4_2022 <- (exp(-mu_4_2022)/(1-exp(-mu_4_2022)))*dim(global[global$Sex==0 & global$AgeCAT==0 & global$Year==2022, ])[1]

n0_2019 <- round(n0_1_2019)+round(n0_2_2019)+round(n0_3_2019)+round(n0_4_2019)
n0_2020 <- round(n0_1_2020)+round(n0_2_2020)+round(n0_3_2020)+round(n0_4_2020)
n0_2021 <- round(n0_1_2021)+round(n0_2_2021)+round(n0_3_2021)+round(n0_4_2021)
n0_2022 <- round(n0_1_2022)+round(n0_2_2022)+round(n0_3_2022)+round(n0_4_2022)

#### Merge zeros with usage data
zeros_2019 <- data.frame(Year=rep(2019, n0_2019), Policy=paste0("A", seq(1:n0_2019)),
                         Sex=c(rep(1, round(n0_1_2019)+round(n0_2_2019)), rep(0, round(n0_3_2019)+round(n0_4_2019))),
                         AgeCAT=c(rep(1, round(n0_1_2019)), rep(0, round(n0_2_2019)), rep(1, round(n0_3_2019)), rep(0, round(n0_4_2019))),
                         Acts=rep(0, n0_2019))

zeros_2020 <- data.frame(Year=rep(2020, n0_2020), Policy=paste0("A", seq(1:n0_2020)),
                         Sex=c(rep(1, round(n0_1_2020)+round(n0_2_2020)), rep(0, round(n0_3_2020)+round(n0_4_2020))),
                         AgeCAT=c(rep(1, round(n0_1_2020)), rep(0, round(n0_2_2020)), rep(1, round(n0_3_2020)), rep(0, round(n0_4_2020))),
                         Acts=rep(0, n0_2020))
zeros_2021 <- data.frame(Year=rep(2021, n0_2021), Policy=paste0("A", seq(1:n0_2021)),
                         Sex=c(rep(1, round(n0_1_2021)+round(n0_2_2021)), rep(0, round(n0_3_2021)+round(n0_4_2021))),
                         AgeCAT=c(rep(1, round(n0_1_2021)), rep(0, round(n0_2_2021)), rep(1, round(n0_3_2021)), rep(0, round(n0_4_2021))),
                         Acts=rep(0, n0_2021))

zeros_2022 <- data.frame(Year=rep(2022, n0_2022), Policy=paste0("A", seq(1:n0_2022)),
                         Sex=c(rep(1, round(n0_1_2022)+round(n0_2_2022)), rep(0, round(n0_3_2022)+round(n0_4_2022))),
                         AgeCAT=c(rep(1, round(n0_1_2022)), rep(0, round(n0_2_2022)), rep(1, round(n0_3_2022)), rep(0, round(n0_4_2022))),
                         Acts=rep(0, n0_2022))

global$Policy <- as.character(global$Policy)

global <- rbind(zeros_2019, zeros_2020, zeros_2021, zeros_2022, global)
global <- global[order(global$Year, global$Policy), ]

#### Global Poisson regression model by year
mod2019 <- glm(Acts~Sex+AgeCAT, family = poisson(), data = global[global$Year==2019, ]); summary(mod2019)
mod2020 <- glm(Acts~Sex+AgeCAT, family = poisson(), data = global[global$Year==2020, ]); summary(mod2020)
mod2021 <- glm(Acts~Sex+AgeCAT, family = poisson(), data = global[global$Year==2021, ]); summary(mod2021)
mod2022 <- glm(Acts~Sex+AgeCAT, family = poisson(), data = global[global$Year==2022, ]); summary(mod2022)
mod_FS  <- glm(Acts~Sex+AgeCAT, family = poisson(), data = global); summary(mod_FS)

#### With global impact
global$GlobalCorr <- 1
global$GlobalCorr[global$Year==2020] <- (1-0.15)
global$GlobalCorr[global$Year==2021] <- (1+0.11)
global$GlobalCorr[global$Year==2022] <- (1+0.081)

mod_global_correction2019 <- glm(Acts~Sex+AgeCAT+offset(log(GlobalCorr)), family = poisson(), data = global[global$Year==2019,]); summary(mod_global_correction2019)
mod_global_correction2020 <- glm(Acts~Sex+AgeCAT+offset(log(GlobalCorr)), family = poisson(), data = global[global$Year==2020,]); summary(mod_global_correction2020)
mod_global_correction2021 <- glm(Acts~Sex+AgeCAT+offset(log(GlobalCorr)), family = poisson(), data = global[global$Year==2021,]); summary(mod_global_correction2021)
mod_global_correction2022 <- glm(Acts~Sex+AgeCAT+offset(log(GlobalCorr)), family = poisson(), data = global[global$Year==2022,]); summary(mod_global_correction2022)
mod_global_correctionFS <- glm(Acts~Sex+AgeCAT+offset(log(GlobalCorr)), family = poisson(), data = global); summary(mod_global_correctionFS)

#### With specific impact
global$SpecificCorr <- 1
global$SpecificCorr[global$Year==2020 & global$Sex==0 & global$AgeCAT==0] <- (1-0.06) ## Females under 60
global$SpecificCorr[global$Year==2021 & global$Sex==0 & global$AgeCAT==0] <- (1+0.29)  ## Females under 60
global$SpecificCorr[global$Year==2022 & global$Sex==0 & global$AgeCAT==0] <- (1+0.19)  ## Females under 60
global$SpecificCorr[global$Year==2020 & global$Sex==0 & global$AgeCAT==1] <- (1-0.17)  ## Females over 60
global$SpecificCorr[global$Year==2021 & global$Sex==0 & global$AgeCAT==1] <- (1-0.0096)  ## Females over 60
global$SpecificCorr[global$Year==2022 & global$Sex==0 & global$AgeCAT==1] <- (1+0.038)  ## Females over 60
global$SpecificCorr[global$Year==2020 & global$Sex==1 & global$AgeCAT==0] <- (1-0.099) ## Males under 60
global$SpecificCorr[global$Year==2021 & global$Sex==1 & global$AgeCAT==0] <- (1+0.22)  ## Males under 60
global$SpecificCorr[global$Year==2022 & global$Sex==1 & global$AgeCAT==0] <- (1+0.19)  ## Males under 60
global$SpecificCorr[global$Year==2020 & global$Sex==1 & global$AgeCAT==1] <- (1-0.21)  ## Males over 60
global$SpecificCorr[global$Year==2021 & global$Sex==1 & global$AgeCAT==1] <- (1-0.046) ## Males over 60
global$SpecificCorr[global$Year==2022 & global$Sex==1 & global$AgeCAT==1] <- (1+0.03) ## Males over 60

mod_spec_correction2019 <- glm(Acts~Sex+AgeCAT+offset(log(SpecificCorr)), family = poisson(), data = global[global$Year==2019,]); summary(mod_spec_correction2019)
mod_spec_correction2020 <- glm(Acts~Sex+AgeCAT+offset(log(SpecificCorr)), family = poisson(), data = global[global$Year==2020,]); summary(mod_spec_correction2020)
mod_spec_correction2021 <- glm(Acts~Sex+AgeCAT+offset(log(SpecificCorr)), family = poisson(), data = global[global$Year==2021,]); summary(mod_spec_correction2021)
mod_spec_correction2022 <- glm(Acts~Sex+AgeCAT+offset(log(SpecificCorr)), family = poisson(), data = global[global$Year==2022,]); summary(mod_spec_correction2022)
mod_spec_correctionFS <- glm(Acts~Sex+AgeCAT+offset(log(SpecificCorr)), family = poisson(), data = global); summary(mod_spec_correctionFS)

#### Observed vs expected distribution of claims
table(global$Acts)
prop.table(table(global$Acts, global$Year), 2)*100

### modGlobal 
b0=mod_FS$coefficients[1]
b1=mod_FS$coefficients[2]
b2=mod_FS$coefficients[3]

fg0=dpois(0, exp(b0+b1+b2))*dim(global[global$Sex==1 & global$AgeCAT==1, ])[1] + 
  dpois(0, exp(b0+b1))*dim(global[global$Sex==1 & global$AgeCAT==0, ])[1] + 
  dpois(0, exp(b0))*dim(global[global$Sex==0 & global$AgeCAT==0, ])[1] + 
  dpois(0, exp(b0+b2))*dim(global[global$Sex==0 & global$AgeCAT==1, ])[1]
fg1=dpois(1, exp(b0+b1+b2))*dim(global[global$Sex==1 & global$AgeCAT==1, ])[1] + 
  dpois(1, exp(b0+b1))*dim(global[global$Sex==1 & global$AgeCAT==0, ])[1] + 
  dpois(1, exp(b0))*dim(global[global$Sex==0 & global$AgeCAT==0, ])[1] + 
  dpois(1, exp(b0+b2))*dim(global[global$Sex==0 & global$AgeCAT==1, ])[1]
fg2=dpois(2, exp(b0+b1+b2))*dim(global[global$Sex==1 & global$AgeCAT==1, ])[1] + 
  dpois(2, exp(b0+b1))*dim(global[global$Sex==1 & global$AgeCAT==0, ])[1] + 
  dpois(2, exp(b0))*dim(global[global$Sex==0 & global$AgeCAT==0, ])[1] + 
  dpois(2, exp(b0+b2))*dim(global[global$Sex==0 & global$AgeCAT==1, ])[1]
fg3=dpois(3, exp(b0+b1+b2))*dim(global[global$Sex==1 & global$AgeCAT==1, ])[1] + 
  dpois(3, exp(b0+b1))*dim(global[global$Sex==1 & global$AgeCAT==0, ])[1] + 
  dpois(3, exp(b0))*dim(global[global$Sex==0 & global$AgeCAT==0, ])[1] + 
  dpois(3, exp(b0+b2))*dim(global[global$Sex==0 & global$AgeCAT==1, ])[1]
fg4=dpois(4, exp(b0+b1+b2))*dim(global[global$Sex==1 & global$AgeCAT==1, ])[1] + 
  dpois(4, exp(b0+b1))*dim(global[global$Sex==1 & global$AgeCAT==0, ])[1] + 
  dpois(4, exp(b0))*dim(global[global$Sex==0 & global$AgeCAT==0, ])[1] + 
  dpois(4, exp(b0+b2))*dim(global[global$Sex==0 & global$AgeCAT==1, ])[1]
fg5=ppois(4, exp(b0+b1+b2), lower.tail = FALSE)*dim(global[global$Sex==1 & global$AgeCAT==1, ])[1] + 
  ppois(4, exp(b0+b1), lower.tail = FALSE)*dim(global[global$Sex==1 & global$AgeCAT==0, ])[1] + 
  ppois(4, exp(b0), lower.tail = FALSE)*dim(global[global$Sex==0 & global$AgeCAT==0, ])[1] + 
  ppois(4, exp(b0+b2), lower.tail = FALSE)*dim(global[global$Sex==0 & global$AgeCAT==1, ])[1]
table(global$Acts)
c(fg0,fg1,fg2,fg3,fg4,fg5)

### mod_global_correction
b0=mod_global_correctionFS $coefficients[1]
b1=mod_global_correctionFS $coefficients[2]
b2=mod_global_correctionFS $coefficients[3]

fgc0=dpois(0, exp(b0+b1+b2))*dim(global[global$Sex==1 & global$AgeCAT==1, ])[1] + 
  dpois(0, exp(b0+b1))*dim(global[global$Sex==1 & global$AgeCAT==0, ])[1] + 
  dpois(0, exp(b0))*dim(global[global$Sex==0 & global$AgeCAT==0, ])[1] + 
  dpois(0, exp(b0+b2))*dim(global[global$Sex==0 & global$AgeCAT==1, ])[1]
fgc1=dpois(1, exp(b0+b1+b2))*dim(global[global$Sex==1 & global$AgeCAT==1, ])[1] + 
  dpois(1, exp(b0+b1))*dim(global[global$Sex==1 & global$AgeCAT==0, ])[1] + 
  dpois(1, exp(b0))*dim(global[global$Sex==0 & global$AgeCAT==0, ])[1] + 
  dpois(1, exp(b0+b2))*dim(global[global$Sex==0 & global$AgeCAT==1, ])[1]
fgc2=dpois(2, exp(b0+b1+b2))*dim(global[global$Sex==1 & global$AgeCAT==1, ])[1] + 
  dpois(2, exp(b0+b1))*dim(global[global$Sex==1 & global$AgeCAT==0, ])[1] + 
  dpois(2, exp(b0))*dim(global[global$Sex==0 & global$AgeCAT==0, ])[1] + 
  dpois(2, exp(b0+b2))*dim(global[global$Sex==0 & global$AgeCAT==1, ])[1]
fgc3=dpois(3, exp(b0+b1+b2))*dim(global[global$Sex==1 & global$AgeCAT==1, ])[1] + 
  dpois(3, exp(b0+b1))*dim(global[global$Sex==1 & global$AgeCAT==0, ])[1] + 
  dpois(3, exp(b0))*dim(global[global$Sex==0 & global$AgeCAT==0, ])[1] + 
  dpois(3, exp(b0+b2))*dim(global[global$Sex==0 & global$AgeCAT==1, ])[1]
fgc4=dpois(4, exp(b0+b1+b2))*dim(global[global$Sex==1 & global$AgeCAT==1, ])[1] + 
  dpois(4, exp(b0+b1))*dim(global[global$Sex==1 & global$AgeCAT==0, ])[1] + 
  dpois(4, exp(b0))*dim(global[global$Sex==0 & global$AgeCAT==0, ])[1] + 
  dpois(4, exp(b0+b2))*dim(global[global$Sex==0 & global$AgeCAT==1, ])[1]
fgc5=ppois(4, exp(b0+b1+b2), lower.tail=FALSE)*dim(global[global$Sex==1 & global$AgeCAT==1, ])[1] + 
  ppois(4, exp(b0+b1), lower.tail=FALSE)*dim(global[global$Sex==1 & global$AgeCAT==0, ])[1] + 
  ppois(4, exp(b0), lower.tail=FALSE)*dim(global[global$Sex==0 & global$AgeCAT==0, ])[1] + 
  ppois(4, exp(b0+b2), lower.tail=FALSE)*dim(global[global$Sex==0 & global$AgeCAT==1, ])[1]
table(global$Acts)
c(fgc0,fgc1,fgc2,fgc3,fgc4,fgc5)

### mod_spec_correction 
### mod_global_correction

b0=mod_spec_correctionFS $coefficients[1]
b1=mod_spec_correctionFS $coefficients[2]
b2=mod_spec_correctionFS $coefficients[3]

fgsc0=dpois(0, exp(b0+b1+b2))*dim(global[global$Sex==1 & global$AgeCAT==1, ])[1] + 
  dpois(0, exp(b0+b1))*dim(global[global$Sex==1 & global$AgeCAT==0, ])[1] + 
  dpois(0, exp(b0))*dim(global[global$Sex==0 & global$AgeCAT==0, ])[1] + 
  dpois(0, exp(b0+b2))*dim(global[global$Sex==0 & global$AgeCAT==1, ])[1]
fgsc1=dpois(1, exp(b0+b1+b2))*dim(global[global$Sex==1 & global$AgeCAT==1, ])[1] + 
  dpois(1, exp(b0+b1))*dim(global[global$Sex==1 & global$AgeCAT==0, ])[1] + 
  dpois(1, exp(b0))*dim(global[global$Sex==0 & global$AgeCAT==0, ])[1] + 
  dpois(1, exp(b0+b2))*dim(global[global$Sex==0 & global$AgeCAT==1, ])[1]
fgsc2=dpois(2, exp(b0+b1+b2))*dim(global[global$Sex==1 & global$AgeCAT==1, ])[1] + 
  dpois(2, exp(b0+b1))*dim(global[global$Sex==1 & global$AgeCAT==0, ])[1] + 
  dpois(2, exp(b0))*dim(global[global$Sex==0 & global$AgeCAT==0, ])[1] + 
  dpois(2, exp(b0+b2))*dim(global[global$Sex==0 & global$AgeCAT==1, ])[1]
fgsc3=dpois(3, exp(b0+b1+b2))*dim(global[global$Sex==1 & global$AgeCAT==1, ])[1] + 
  dpois(3, exp(b0+b1))*dim(global[global$Sex==1 & global$AgeCAT==0, ])[1] + 
  dpois(3, exp(b0))*dim(global[global$Sex==0 & global$AgeCAT==0, ])[1] + 
  dpois(3, exp(b0+b2))*dim(global[global$Sex==0 & global$AgeCAT==1, ])[1]
fgsc4=dpois(4, exp(b0+b1+b2))*dim(global[global$Sex==1 & global$AgeCAT==1, ])[1] + 
  dpois(4, exp(b0+b1))*dim(global[global$Sex==1 & global$AgeCAT==0, ])[1] + 
  dpois(4, exp(b0))*dim(global[global$Sex==0 & global$AgeCAT==0, ])[1] + 
  dpois(4, exp(b0+b2))*dim(global[global$Sex==0 & global$AgeCAT==1, ])[1]
fgsc5=ppois(4, exp(b0+b1+b2), lower.tail=FALSE)*dim(global[global$Sex==1 & global$AgeCAT==1, ])[1] + 
  ppois(4, exp(b0+b1), lower.tail=FALSE)*dim(global[global$Sex==1 & global$AgeCAT==0, ])[1] + 
  ppois(4, exp(b0), lower.tail=FALSE)*dim(global[global$Sex==0 & global$AgeCAT==0, ])[1] + 
  ppois(4, exp(b0+b2), lower.tail=FALSE)*dim(global[global$Sex==0 & global$AgeCAT==1, ])[1]
table(global$Acts)
c(fgsc0,fgsc1,fgsc2,fgsc3,fgsc4,fgsc5)

sum(table(global$Acts))-table(global$Acts)[1]
sum(c(fgc0,fgc1,fgc2,fgc3,fgc4,fgc5))-fgc0
sum(c(fgsc0,fgsc1,fgsc2,fgsc3,fgsc4,fgsc5))-fgsc0
sum(c(fgsc0,fgsc1,fgsc2,fgsc3,fgsc4,fgsc5))-fgsc0

#### Costs
50*sum(table(global$Acts, global$Year)[2:14, 1]*seq(1:13))  ### 2019
50*sum(table(global$Acts, global$Year)[2:14, 2]*seq(1:13))  ### 2020
50*sum(table(global$Acts, global$Year)[2:14, 3]*seq(1:13))  ### 2021
50*sum(table(global$Acts, global$Year)[2:14, 4]*seq(1:13))  ### 2022

### Total cost: 12,975,500€

### mod_global_correction 2019
b0=mod_global_correction2019 $coefficients[1]
b1=mod_global_correction2019 $coefficients[2]
b2=mod_global_correction2019 $coefficients[3]

fgc <- vector()
for (i in 1:50)
{
  fgc[i] <- dpois(i, exp(b0+b1+b2))*dim(global[global$Year==2019 & global$Sex==1 & global$AgeCAT==1, ])[1] + 
    dpois(i, exp(b0+b1))*dim(global[global$Year==2019 & global$Sex==1 & global$AgeCAT==0, ])[1] + 
    dpois(i, exp(b0))*dim(global[global$Year==2019 & global$Sex==0 & global$AgeCAT==0, ])[1] + 
    dpois(i, exp(b0+b2))*dim(global[global$Year==2019 & global$Sex==0 & global$AgeCAT==1, ])[1]
}

50*sum(fgc*seq(1:50))  ### 2019

### mod_global_correction 2020
b0=mod_global_correction2020$coefficients[1]
b1=mod_global_correction2020$coefficients[2]
b2=mod_global_correction2020$coefficients[3]

fgc <- vector()
for (i in 1:50)
{
  fgc[i] <- dpois(i, exp(b0+b1+b2))*dim(global[global$Year==2020 & global$Sex==1 & global$AgeCAT==1, ])[1] + 
    dpois(i, exp(b0+b1))*dim(global[global$Year==2020 & global$Sex==1 & global$AgeCAT==0, ])[1] + 
    dpois(i, exp(b0))*dim(global[global$Year==2020 & global$Sex==0 & global$AgeCAT==0, ])[1] + 
    dpois(i, exp(b0+b2))*dim(global[global$Year==2020 & global$Sex==0 & global$AgeCAT==1, ])[1]
}

50*sum(fgc*seq(1:50))  ### 2020

### mod_global_correction 2021
b0=mod_global_correction2021$coefficients[1]
b1=mod_global_correction2021$coefficients[2]
b2=mod_global_correction2021$coefficients[3]

fgc <- vector()
for (i in 1:50)
{
  fgc[i] <- dpois(i, exp(b0+b1+b2))*dim(global[global$Year==2021 & global$Sex==1 & global$AgeCAT==1, ])[1] + 
    dpois(i, exp(b0+b1))*dim(global[global$Year==2021 & global$Sex==1 & global$AgeCAT==0, ])[1] + 
    dpois(i, exp(b0))*dim(global[global$Year==2021 & global$Sex==0 & global$AgeCAT==0, ])[1] + 
    dpois(i, exp(b0+b2))*dim(global[global$Year==2021 & global$Sex==0 & global$AgeCAT==1, ])[1]
}

50*sum(fgc*seq(1:50))  ### 2021

### mod_global_correction 2022
b0=mod_global_correction2022$coefficients[1]
b1=mod_global_correction2022$coefficients[2]
b2=mod_global_correction2022$coefficients[3]

fgc <- vector()
for (i in 1:50)
{
  fgc[i] <- dpois(i, exp(b0+b1+b2))*dim(global[global$Year==2022 & global$Sex==1 & global$AgeCAT==1, ])[1] + 
    dpois(i, exp(b0+b1))*dim(global[global$Year==2022 & global$Sex==1 & global$AgeCAT==0, ])[1] + 
    dpois(i, exp(b0))*dim(global[global$Year==2022 & global$Sex==0 & global$AgeCAT==0, ])[1] + 
    dpois(i, exp(b0+b2))*dim(global[global$Year==2022 & global$Sex==0 & global$AgeCAT==1, ])[1]
}

50*sum(fgc*seq(1:50))  ### 2022

### Total cost using global correction: 12,825,174€

### mod_spec_correction  2019
b0=mod_spec_correction2019$coefficients[1]
b1=mod_spec_correction2019$coefficients[2]
b2=mod_spec_correction2019$coefficients[3]

fgsc <- vector()
for (i in 1:50)
{
  fgsc[i] <- dpois(i, exp(b0+b1+b2))*dim(global[global$Year==2019 & global$Sex==1 & global$AgeCAT==1, ])[1] + 
    dpois(i, exp(b0+b1))*dim(global[global$Year==2019 & global$Sex==1 & global$AgeCAT==0, ])[1] + 
    dpois(i, exp(b0))*dim(global[global$Year==2019 & global$Sex==0 & global$AgeCAT==0, ])[1] + 
    dpois(i, exp(b0+b2))*dim(global[global$Year==2019 & global$Sex==0 & global$AgeCAT==1, ])[1]
}

50*sum(fgsc*seq(1:50)) ### 2019

### mod_spec_correction  2020

b0=mod_spec_correction2020$coefficients[1]
b1=mod_spec_correction2020$coefficients[2]
b2=mod_spec_correction2020$coefficients[3]

fgsc <- vector()
for (i in 1:50)
{
  fgsc[i] <- dpois(i, exp(b0+b1+b2))*dim(global[global$Year==2020 & global$Sex==1 & global$AgeCAT==1, ])[1] + 
    dpois(i, exp(b0+b1))*dim(global[global$Year==2020 & global$Sex==1 & global$AgeCAT==0, ])[1] + 
    dpois(i, exp(b0))*dim(global[global$Year==2020 & global$Sex==0 & global$AgeCAT==0, ])[1] + 
    dpois(i, exp(b0+b2))*dim(global[global$Year==2020 & global$Sex==0 & global$AgeCAT==1, ])[1]
}

50*sum(fgsc*seq(1:50)) ### 2020

### mod_spec_correction  2021

b0=mod_spec_correction2021$coefficients[1]
b1=mod_spec_correction2021$coefficients[2]
b2=mod_spec_correction2021$coefficients[3]

fgsc <- vector()
for (i in 1:50)
{
  fgsc[i] <- dpois(i, exp(b0+b1+b2))*dim(global[global$Year==2021 & global$Sex==1 & global$AgeCAT==1, ])[1] + 
    dpois(i, exp(b0+b1))*dim(global[global$Year==2021 & global$Sex==1 & global$AgeCAT==0, ])[1] + 
    dpois(i, exp(b0))*dim(global[global$Year==2021 & global$Sex==0 & global$AgeCAT==0, ])[1] + 
    dpois(i, exp(b0+b2))*dim(global[global$Year==2021 & global$Sex==0 & global$AgeCAT==1, ])[1]
}

50*sum(fgsc*seq(1:50)) ### 2021

### mod_spec_correction  2022

b0=mod_spec_correction2022$coefficients[1]
b1=mod_spec_correction2022$coefficients[2]
b2=mod_spec_correction2022$coefficients[3]

fgsc <- vector()
for (i in 1:50)
{
  fgsc[i] <- dpois(i, exp(b0+b1+b2))*dim(global[global$Year==2022 & global$Sex==1 & global$AgeCAT==1, ])[1] + 
    dpois(i, exp(b0+b1))*dim(global[global$Year==2022 & global$Sex==1 & global$AgeCAT==0, ])[1] + 
    dpois(i, exp(b0))*dim(global[global$Year==2022 & global$Sex==0 & global$AgeCAT==0, ])[1] + 
    dpois(i, exp(b0+b2))*dim(global[global$Year==2022 & global$Sex==0 & global$AgeCAT==1, ])[1]
}

50*sum(fgsc*seq(1:50)) ### 2022

### Total cost using specific correction: 12,562,331€
