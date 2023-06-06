library(readxl)
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(readxl)
library(lubridate)
library(MMWRweek)
library(sjPlot)
library(sjmisc)
library(sjlabelled)

data_poiss <- read_csv("Poisson_cardiology_data.csv")

#### Poisson regression model by year
mod2019 <- glm(Acts~Sex+AgeCAT, family = poisson(), data = global[global$Year==2019, ]); summary(mod2019)
mod2020 <- glm(Acts~Sex+AgeCAT, family = poisson(), data = global[global$Year==2020, ]); summary(mod2020)
mod2021 <- glm(Acts~Sex+AgeCAT, family = poisson(), data = global[global$Year==2021, ]); summary(mod2021)
mod2022 <- glm(Acts~Sex+AgeCAT, family = poisson(), data = global[global$Year==2022, ]); summary(mod2022)


tab_model(mod2019)
tab_model(mod2020)
tab_model(mod2021)
tab_model(mod2022)
### Global Poisson regression 
modGlobal <- glm(Acts~Sex+AgeCAT, family = poisson(), data = global); summary(modGlobal)
tab_model(modGlobal)
modGlobalYear <- glm(Acts~Sex+AgeCAT+factor(Year), family = poisson(), data = global); summary(modGlobalYear)
tab_model(modGlobalYear)
#### With global impact
global$GlobalCorr <- 1
global$GlobalCorr[global$Year==2020] <- (1-0.12)
global$GlobalCorr[global$Year==2021] <- (1+0.16)
global$GlobalCorr[global$Year==2022] <- (1+0.22)
mod_global_correction <- glm(Acts~Sex+AgeCAT+offset(log(GlobalCorr)), family = poisson(), data = global); summary(mod_global_correction)
tab_model(mod_global_correction)

#### With specific impact
global$SpecificCorr <- 1
global$SpecificCorr[global$Year==2020 & global$Sex==0 & global$AgeCAT==0] <- (1-0.029)
global$SpecificCorr[global$Year==2021 & global$Sex==0 & global$AgeCAT==0] <- (1+0.39)
global$SpecificCorr[global$Year==2022 & global$Sex==0 & global$AgeCAT==0] <- (1+0.47)
global$SpecificCorr[global$Year==2020 & global$Sex==0 & global$AgeCAT==1] <- (1-0.15)
global$SpecificCorr[global$Year==2021 & global$Sex==0 & global$AgeCAT==1] <- (1+0.01)
global$SpecificCorr[global$Year==2022 & global$Sex==0 & global$AgeCAT==1] <- (1+0.10)
global$SpecificCorr[global$Year==2020 & global$Sex==1 & global$AgeCAT==0] <- (1-0.073)
global$SpecificCorr[global$Year==2021 & global$Sex==1 & global$AgeCAT==0] <- (1+0.28)
global$SpecificCorr[global$Year==2022 & global$Sex==1 & global$AgeCAT==0] <- (1+0.38)
global$SpecificCorr[global$Year==2020 & global$Sex==1 & global$AgeCAT==1] <- (1-0.19)
global$SpecificCorr[global$Year==2021 & global$Sex==1 & global$AgeCAT==1] <- (1-0.046)
global$SpecificCorr[global$Year==2022 & global$Sex==1 & global$AgeCAT==1] <- (1+0.056)

mod_spec_correction <- glm(Acts~Sex+AgeCAT+offset(log(SpecificCorr)), family = poisson(), data = global); summary(mod_spec_correction)
tab_model(mod_spec_correction)

mod_spec_correction2019 <- glm(Acts~Sex+AgeCAT+offset(log(SpecificCorr)), family = poisson(), data = global[global$Year==2019,]); summary(mod_spec_correction2019)
tab_model(mod_spec_correction2019)

mod_spec_correction2020 <- glm(Acts~Sex+AgeCAT+offset(log(SpecificCorr)), family = poisson(), data = global[global$Year==2020,]); summary(mod_spec_correction2020)
tab_model(mod_spec_correction2020)

mod_spec_correction2021 <- glm(Acts~Sex+AgeCAT+offset(log(SpecificCorr)), family = poisson(), data = global[global$Year==2021,]); summary(mod_spec_correction2021)
tab_model(mod_spec_correction2021)

mod_spec_correction2022 <- glm(Acts~Sex+AgeCAT+offset(log(SpecificCorr)), family = poisson(), data = global[global$Year==2022,]); summary(mod_spec_correction2022)
tab_model(mod_spec_correction2022)

#### Observed vs expected distribution of claims
table(global$Acts)
prop.table(table(global$Acts, global$Year), 2)*100
### modGlobal 
b0=modGlobal$coefficients[1]
b1=modGlobal$coefficients[2]
b2=modGlobal$coefficients[3]

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
b0=mod_global_correction $coefficients[1]
b1=mod_global_correction $coefficients[2]
b2=mod_global_correction $coefficients[3]

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

b0=mod_spec_correction $coefficients[1]
b1=mod_spec_correction $coefficients[2]
b2=mod_spec_correction $coefficients[3]

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
