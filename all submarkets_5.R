
library(dplyr)
library(ggplot2)
library(lmtest)
library(multiwayvcov)
library(tmap)

rm(list=ls())

setwd("C:/Users/syedm/Desktop/atlanta_submarket")

data <- read.csv("atlanta single family 6116_new.csv")
data$BlockGroup <- paste0("1500US", data$GEOID)

data %>%
  mutate(quantile = ntile(pct_black, 20)) -> data

tapply(I((data$tax_last)/(data$salesprice)), data$quantile, summary)

#10 quantile, choose 8
#20 quantile, choose 14

data$bin <- ifelse(data$quantile >= 14, 1, 0)

mod1 <- lm(log(I((tax_last)/salesprice)) ~ bin + over65_perc + pct_renter_occupied + I(median_income/1000) + 
             pct_HSdiploma + I(median_house_value/1000) + pct_above_f + pct_below_f +
             revcode + deedtype + saleval + saletype + saledtyearyyyy + saledtmonthmon +
             calcacres + stories + age + age_sq + sqft4 + centheat + totbath + years_since_remodel + 
             rmtot + fixtot + style + extwall + fuel + attic + fronting +
             street1 + topo1 + util1 + parktype + parkprox + dcdu + as.factor(location) +
             as.factor(luc) + zoning + xdist + xdist_sq + ydist + ydist_sq +
             as.factor(parcel_district) + as.factor(middlesch), data=data)

summary(mod1)

data$census <- as.factor(data$BlockGroup)

vcov_firm <- cluster.vcov(mod1, as.factor(data$census))
coeftest(mod1, vcov_firm)




I((tax_last)/salesprice)
I((tax_last)/ass_last)
I((ass_last)/salesprice)

mod1 <- lm(log(I((ass_last)/salesprice)) ~ pct_black + over65_perc + pct_renter_occupied + I(median_income/1000) + 
             pct_HSdiploma + I(median_house_value/1000) + pct_above_f + pct_below_f +
             revcode + deedtype + saleval + saletype + saledtyearyyyy + 
             saledtmonthmon, data=data)

summary(mod1)

data$census <- as.factor(data$BlockGroup)

vcov_firm <- cluster.vcov(mod1, as.factor(data$census))
coeftest(mod1, vcov_firm)

mod1 <- lm(log(I((ass_last)/salesprice)) ~ pct_black + over65_perc + pct_renter_occupied + I(median_income/1000) + 
             pct_HSdiploma + I(median_house_value/1000) + pct_above_f + pct_below_f +
             revcode + deedtype + saleval + saletype + saledtyearyyyy + saledtmonthmon +
             calcacres + stories + age + age_sq + sqft4 + centheat + totbath + years_since_remodel + 
             rmtot + fixtot + style + extwall + fuel + attic + fronting +
             street1 + topo1 + util1 + parktype + parkprox + dcdu, data=data)

summary(mod1)

data$census <- as.factor(data$BlockGroup)

vcov_firm <- cluster.vcov(mod1, as.factor(data$census))
coeftest(mod1, vcov_firm)

mod1 <- lm(log(I((ass_last)/salesprice)) ~ pct_black + over65_perc + pct_renter_occupied + I(median_income/1000) + 
             pct_HSdiploma + I(median_house_value/1000) + pct_above_f + pct_below_f +
             revcode + deedtype + saleval + saletype + saledtyearyyyy + saledtmonthmon +
             calcacres + stories + age + age_sq + sqft4 + centheat + totbath + years_since_remodel + 
             rmtot + fixtot + style + extwall + fuel + attic + fronting +
             street1 + topo1 + util1 + parktype + parkprox + dcdu + as.factor(location) +
             as.factor(luc) + zoning, data=data)

summary(mod1)

data$census <- as.factor(data$BlockGroup)

vcov_firm <- cluster.vcov(mod1, as.factor(data$census))
coeftest(mod1, vcov_firm)

mod1 <- lm(log(I((ass_last)/salesprice)) ~ pct_black + over65_perc + pct_renter_occupied + I(median_income/1000) + 
             pct_HSdiploma + I(median_house_value/1000) + pct_above_f + pct_below_f +
             revcode + deedtype + saleval + saletype + saledtyearyyyy + saledtmonthmon +
             calcacres + stories + age + age_sq + sqft4 + centheat + totbath + years_since_remodel + 
             rmtot + fixtot + style + extwall + fuel + attic + fronting +
             street1 + topo1 + util1 + parktype + parkprox + dcdu + as.factor(location) +
             as.factor(luc) + zoning + xdist + xdist_sq + ydist + ydist_sq, data=data)

summary(mod1)

data$census <- as.factor(data$BlockGroup)

vcov_firm <- cluster.vcov(mod1, as.factor(data$census))
coeftest(mod1, vcov_firm)

mod1 <- lm(log(I((ass_last)/salesprice)) ~ pct_black + over65_perc + pct_renter_occupied + I(median_income/1000) + 
             pct_HSdiploma + I(median_house_value/1000) + pct_above_f + pct_below_f +
             revcode + deedtype + saleval + saletype + saledtyearyyyy + saledtmonthmon +
             calcacres + stories + age + age_sq + sqft4 + centheat + totbath + years_since_remodel + 
             rmtot + fixtot + style + extwall + fuel + attic + fronting +
             street1 + topo1 + util1 + parktype + parkprox + dcdu + as.factor(location) +
             as.factor(luc) + zoning + xdist + xdist_sq + ydist + ydist_sq +
             as.factor(parcel_district) + as.factor(middlesch), data=data)

summary(mod1)

data$census <- as.factor(data$BlockGroup)

vcov_firm <- cluster.vcov(mod1, as.factor(data$census))
coeftest(mod1, vcov_firm)

########## Submarkets ##########

########## 2 Submarkets ##########

rm(list=ls())

setwd("C:/Users/syedm/Desktop/atlanta_submarket")

data <- read.csv("atlanta single family 6116_new.csv")

data$lon <- -1*data$lon
data$plon <- 0
data$plat <- 0

data$BlockGroup <- paste0("1500US", data$GEOID)

data %>% select(X.1, salesprice, calcacres, ENGMeanScaleScore15, age, totbath, median_income, over65_perc, 
                fourthquart, pct_renter_occupied, pct_above_f, pct_below_f, MATHMeanScaleScore15, pct_HSdiploma, log_median_income,
                sqft4, lon, plon, lat, plat, pct_white, pct_black, pct_collegeDegree, HHsize, tract_cover, 
                total_crime_house, tax_last, ass_last, BlockGroup) -> data

data <- data[complete.cases(data), ]

prob <- read.csv("prob2.csv")
datanew <- cbind(data, prob)
datanew$prob <- ifelse(datanew$V1 > datanew$V2, 1, 2)

data2 <- read.csv("atlanta single family 6116_new.csv")

data2 %>% select(X.1, median_house_value, revcode, deedtype, saleval, saletype, saledtyearyyyy, saledtmonthmon, 
                 stories, centheat, years_since_remodel,  rmtot, fixtot, style, extwall, fuel, attic, fronting, street1, topo1, 
                 util1, parktype, parkprox, dcdu, location, luc, zoning, xdist, xdist_sq, ydist, ydist_sq, parcel_district,  middlesch) -> data2
                
data <- merge(datanew, data2, by=c("X.1"), all.x=FALSE)

#I((tax_last)/salesprice)
#I((tax_last)/ass_last)
#I((ass_last)/salesprice)

data %>%
  mutate(quantile = ntile(pct_black, 20)) -> data

data$bin <- ifelse(data$quantile >= 14, 1, 0)

mod1 <- lm(log(I((tax_last)/salesprice)) ~ bin + over65_perc + pct_renter_occupied + I(median_income/1000) + 
             pct_HSdiploma + I(median_house_value/1000) + pct_above_f + pct_below_f +
             revcode + deedtype + saleval + saletype + saledtyearyyyy + saledtmonthmon +
             calcacres + stories + age + I(age*age) + sqft4 + centheat + totbath + years_since_remodel + 
             rmtot + fixtot + style + extwall + fuel + attic + fronting +
             street1 + topo1 + util1 + parktype + parkprox + dcdu + as.factor(location) +
             as.factor(luc) + zoning + xdist + xdist_sq + ydist + ydist_sq +
             as.factor(parcel_district) + as.factor(middlesch), data=data)

summary(mod1)

vcov_firm <- cluster.vcov(mod1, as.factor(data$BlockGroup))
coeftest(mod1, vcov_firm)



data_sub <- subset(data, prob==2)

data_sub %>%
  mutate(quantile = ntile(pct_black, 20)) -> data_sub

data_sub$bin <- ifelse(data_sub$quantile >= 14, 1, 0)

mod1 <- lm(log(I((tax_last)/salesprice)) ~ bin + over65_perc + pct_renter_occupied + I(median_income/1000) + 
             pct_HSdiploma + I(median_house_value/1000) + pct_above_f + pct_below_f +
             revcode + deedtype + saleval + saletype + saledtyearyyyy + saledtmonthmon +
             calcacres + stories + age + I(age*age) + sqft4 + centheat + totbath + years_since_remodel + 
             rmtot + fixtot + style + extwall + fuel + attic + fronting +
             street1 + topo1 + util1 + parktype + parkprox + dcdu + as.factor(location) +
             as.factor(luc) + zoning + xdist + xdist_sq + ydist + ydist_sq +
             as.factor(parcel_district) + as.factor(middlesch), data=data_sub)

summary(mod1)

vcov_firm <- cluster.vcov(mod1, as.factor(data_sub$BlockGroup))
coeftest(mod1, vcov_firm)

########## 3 Submarkets ##########

rm(list=ls())

setwd("C:/Users/syedm/Desktop/atlanta_submarket")

data <- read.csv("atlanta single family 6116_new.csv")

data$lon <- -1*data$lon
data$plon <- 0
data$plat <- 0

data$BlockGroup <- paste0("1500US", data$GEOID)

data %>% select(X.1, salesprice, calcacres, ENGMeanScaleScore15, age, totbath, median_income, over65_perc, 
                fourthquart, pct_renter_occupied, pct_above_f, pct_below_f, MATHMeanScaleScore15, pct_HSdiploma, log_median_income,
                sqft4, lon, plon, lat, plat, pct_white, pct_black, pct_collegeDegree, HHsize, tract_cover, 
                total_crime_house, tax_last, ass_last, BlockGroup) -> data

data <- data[complete.cases(data), ]

prob <- read.csv("prob3.csv")
datanew <- cbind(data, prob)
datanew$prob <- colnames(datanew[, c(31:33)])[apply(datanew[, c(31:33)],1,which.max)]
datanew$prob <- ifelse(datanew$prob=="V1", 1, ifelse(datanew$prob=="V2", 2, 3))

data2 <- read.csv("atlanta single family 6116_new.csv")

data2 %>% select(X.1, median_house_value, revcode, deedtype, saleval, saletype, saledtyearyyyy, saledtmonthmon, 
                 stories, centheat, years_since_remodel,  rmtot, fixtot, style, extwall, fuel, attic, fronting, street1, topo1, 
                 util1, parktype, parkprox, dcdu, location, luc, zoning, xdist, xdist_sq, ydist, ydist_sq, parcel_district,  middlesch) -> data2

data <- merge(datanew, data2, by=c("X.1"), all.x=FALSE)

#I((tax_last)/salesprice)
#I((tax_last)/ass_last)
#I((ass_last)/salesprice)

data %>%
  mutate(quantile = ntile(pct_black, 20)) -> data

data$bin <- ifelse(data$quantile >= 14, 1, 0)

mod1 <- lm(log(I((tax_last)/salesprice)) ~ bin + over65_perc + pct_renter_occupied + I(median_income/1000) + 
             pct_HSdiploma + I(median_house_value/1000) + pct_above_f + pct_below_f +
             revcode + deedtype + saleval + saletype + saledtyearyyyy + saledtmonthmon +
             calcacres + stories + age + I(age*age) + sqft4 + centheat + totbath + years_since_remodel + 
             rmtot + fixtot + style + extwall + fuel + attic + fronting +
             street1 + topo1 + util1 + parktype + parkprox + dcdu + as.factor(location) +
             as.factor(luc) + zoning + xdist + xdist_sq + ydist + ydist_sq +
             as.factor(parcel_district) + as.factor(middlesch), data=data)

summary(mod1)

vcov_firm <- cluster.vcov(mod1, as.factor(data$BlockGroup))
coeftest(mod1, vcov_firm)



data_sub <- subset(data, prob==1)

data_sub %>%
  mutate(quantile = ntile(pct_black, 20)) -> data_sub

data_sub$bin <- ifelse(data_sub$quantile >= 14, 1, 0)

mod1 <- lm(log(I((ass_last)/salesprice)) ~ pct_black + pct_renter_occupied + I(median_income/1000) + 
             pct_HSdiploma + I(median_house_value/1000) + pct_above_f + pct_below_f +
             revcode + deedtype + saleval + saletype + saledtyearyyyy + saledtmonthmon +
             calcacres + stories + age + I(age*age) + sqft4 + centheat + totbath + years_since_remodel + 
             rmtot + fixtot + style + extwall + fuel + attic + fronting +
             street1 + topo1 + util1 + parktype + parkprox + dcdu + 
             as.factor(location) + as.factor(luc) +
             zoning + xdist + xdist_sq + ydist + ydist_sq +
             as.factor(parcel_district) + as.factor(middlesch), data=data_sub)

summary(mod1)

vcov_firm <- cluster.vcov(mod1, as.factor(data_sub$BlockGroup))
coeftest(mod1, vcov_firm)



data_sub <- subset(data, prob==2)

data_sub %>%
  mutate(quantile = ntile(pct_black, 20)) -> data_sub

data_sub$bin <- ifelse(data_sub$quantile >= 14, 1, 0)

mod1 <- lm(log(I((ass_last)/salesprice)) ~ pct_black + pct_renter_occupied + I(median_income/1000) + 
             pct_HSdiploma + I(median_house_value/1000) + pct_above_f + pct_below_f +
             revcode + deedtype + saleval + saletype + saledtyearyyyy + saledtmonthmon +
             calcacres + stories + age + I(age*age) + sqft4 + centheat + totbath + years_since_remodel + 
             rmtot + fixtot + style + extwall + fuel + attic + fronting +
             street1 + topo1 + util1 + parktype + parkprox + dcdu + 
             zoning + xdist + xdist_sq + ydist + ydist_sq +
             as.factor(parcel_district) + as.factor(middlesch), data=data_sub)

summary(mod1)

vcov_firm <- cluster.vcov(mod1, as.factor(data_sub$BlockGroup))
coeftest(mod1, vcov_firm)



data_sub <- subset(data, prob==3)

data_sub %>%
  mutate(quantile = ntile(pct_black, 20)) -> data_sub

data_sub$bin <- ifelse(data_sub$quantile >= 14, 1, 0)

mod1 <- lm(log(I((ass_last)/salesprice)) ~ pct_black + pct_renter_occupied + I(median_income/1000) + 
             pct_HSdiploma + I(median_house_value/1000) + pct_above_f + pct_below_f +
             revcode + deedtype + saleval + saletype + saledtyearyyyy + saledtmonthmon +
             calcacres + stories + age + I(age*age) + sqft4 + centheat + totbath + years_since_remodel + 
             rmtot + fixtot + style + extwall + fuel + attic + fronting +
             street1 + topo1 + util1 + parktype + parkprox + dcdu + 
             zoning + xdist + xdist_sq + ydist + ydist_sq +
             as.factor(parcel_district) + as.factor(middlesch), data=data_sub)

summary(mod1)

vcov_firm <- cluster.vcov(mod1, as.factor(data_sub$BlockGroup))
coeftest(mod1, vcov_firm)

########## 4 Submarkets ##########

rm(list=ls())

setwd("C:/Users/syedm/Desktop/atlanta_submarket")

data <- read.csv("atlanta single family 6116_new.csv")

data$lon <- -1*data$lon
data$plon <- 0
data$plat <- 0

data$BlockGroup <- paste0("1500US", data$GEOID)

data %>% select(X.1, salesprice, calcacres, ENGMeanScaleScore15, age, totbath, median_income, over65_perc, 
                fourthquart, pct_renter_occupied, pct_above_f, pct_below_f, MATHMeanScaleScore15, pct_HSdiploma, log_median_income,
                sqft4, lon, plon, lat, plat, pct_white, pct_black, pct_collegeDegree, HHsize, tract_cover, 
                total_crime_house, tax_last, ass_last, BlockGroup) -> data

data <- data[complete.cases(data), ]

prob <- read.csv("prob4.csv")
datanew <- cbind(data, prob)
datanew$prob <- colnames(datanew[, c(31:34)])[apply(datanew[, c(31:34)],1,which.max)]
datanew$prob <- ifelse(datanew$prob=="V1", 1, ifelse(datanew$prob=="V2", 2, ifelse(datanew$prob=="V3", 3, 4)))

data2 <- read.csv("atlanta single family 6116_new.csv")

data2 %>% select(X.1, median_house_value, revcode, deedtype, saleval, saletype, saledtyearyyyy, saledtmonthmon, 
                 stories, centheat, years_since_remodel,  rmtot, fixtot, style, extwall, fuel, attic, fronting, street1, topo1, 
                 util1, parktype, parkprox, dcdu, location, luc, zoning, xdist, xdist_sq, ydist, ydist_sq, parcel_district,  middlesch) -> data2

data <- merge(datanew, data2, by=c("X.1"), all.x=FALSE)

#I((tax_last)/salesprice)
#I((tax_last)/ass_last)
#I((ass_last)/salesprice)

data %>%
  mutate(quantile = ntile(pct_black, 20)) -> data

data$bin <- ifelse(data$quantile >= 14, 1, 0)

mod1 <- lm(log(I((tax_last)/salesprice)) ~ bin + over65_perc + pct_renter_occupied + I(median_income/1000) + 
             pct_HSdiploma + I(median_house_value/1000) + pct_above_f + pct_below_f +
             revcode + deedtype + saleval + saletype + saledtyearyyyy + saledtmonthmon +
             calcacres + stories + age + I(age*age) + sqft4 + centheat + totbath + years_since_remodel + 
             rmtot + fixtot + style + extwall + fuel + attic + fronting +
             street1 + topo1 + util1 + parktype + parkprox + dcdu + as.factor(location) +
             as.factor(luc) + zoning + xdist + xdist_sq + ydist + ydist_sq +
             as.factor(parcel_district) + as.factor(middlesch), data=data)

summary(mod1)

vcov_firm <- cluster.vcov(mod1, as.factor(data$BlockGroup))
coeftest(mod1, vcov_firm)



data_sub <- subset(data, prob==1)

data_sub %>%
  mutate(quantile = ntile(pct_black, 20)) -> data_sub

data_sub$bin <- ifelse(data_sub$quantile >= 14, 1, 0)

mod1 <- lm(log(I((ass_last)/salesprice)) ~ pct_black + pct_renter_occupied + I(median_income/1000) + 
             pct_HSdiploma + I(median_house_value/1000) + pct_above_f + pct_below_f +
             revcode + deedtype + saleval + saletype + saledtyearyyyy + saledtmonthmon +
             calcacres + stories + age + I(age*age) + sqft4 + centheat + totbath + years_since_remodel + 
             rmtot + fixtot + style + extwall + fuel + attic + fronting +
             street1 + topo1 + util1 + parktype + parkprox + dcdu + 
             as.factor(location) + as.factor(luc) +
             zoning + xdist + xdist_sq + ydist + ydist_sq +
             as.factor(parcel_district) + as.factor(middlesch), data=data_sub)

summary(mod1)

vcov_firm <- cluster.vcov(mod1, as.factor(data_sub$BlockGroup))
coeftest(mod1, vcov_firm)



data_sub <- subset(data, prob==2)

data_sub %>%
  mutate(quantile = ntile(pct_black, 20)) -> data_sub

data_sub$bin <- ifelse(data_sub$quantile >= 14, 1, 0)

mod1 <- lm(log(I((ass_last)/salesprice)) ~ pct_black + pct_renter_occupied + I(median_income/1000) + 
             pct_HSdiploma + I(median_house_value/1000) + pct_above_f + pct_below_f +
             revcode + deedtype + saleval + saletype + saledtyearyyyy + saledtmonthmon +
             calcacres + stories + age + I(age*age) + sqft4 + centheat + totbath + years_since_remodel + 
             rmtot + fixtot + style + extwall + fuel + attic + fronting +
             street1 + topo1 + util1 + parktype + parkprox + dcdu + 
             zoning + xdist + xdist_sq + ydist + ydist_sq +
             as.factor(parcel_district) + as.factor(middlesch), data=data_sub)

summary(mod1)

vcov_firm <- cluster.vcov(mod1, as.factor(data_sub$BlockGroup))
coeftest(mod1, vcov_firm)



data_sub <- subset(data, prob==3)

data_sub %>%
  mutate(quantile = ntile(pct_black, 20)) -> data_sub

data_sub$bin <- ifelse(data_sub$quantile >= 14, 1, 0)

mod1 <- lm(log(I((ass_last)/salesprice)) ~ pct_black + pct_renter_occupied + I(median_income/1000) + 
             pct_HSdiploma + I(median_house_value/1000) + pct_above_f + pct_below_f +
             revcode + deedtype + saleval + saletype + saledtyearyyyy + saledtmonthmon +
             calcacres + stories + age + I(age*age) + sqft4 + centheat + totbath + years_since_remodel + 
             rmtot + fixtot + style + extwall + fuel + attic + fronting +
             street1 + topo1 + util1 + parktype + parkprox + dcdu + 
             zoning + xdist + xdist_sq + ydist + ydist_sq +
             as.factor(parcel_district) + as.factor(middlesch), data=data_sub)

summary(mod1)

vcov_firm <- cluster.vcov(mod1, as.factor(data_sub$BlockGroup))
coeftest(mod1, vcov_firm)



data_sub <- subset(data, prob==4)

data_sub %>%
  mutate(quantile = ntile(pct_black, 20)) -> data_sub

data_sub$bin <- ifelse(data_sub$quantile >= 14, 1, 0)

mod1 <- lm(log(I((ass_last)/salesprice)) ~ pct_black + pct_renter_occupied + I(median_income/1000) + 
             pct_HSdiploma + I(median_house_value/1000) + pct_above_f + pct_below_f +
             revcode + deedtype + saleval + saletype + saledtyearyyyy + saledtmonthmon +
             calcacres + stories + age + I(age*age) + sqft4 + centheat + totbath + years_since_remodel + 
             rmtot + fixtot + style + extwall + fuel + attic + fronting +
             street1 + topo1 + util1 + parktype + parkprox + dcdu + 
             zoning + xdist + xdist_sq + ydist + ydist_sq +
             as.factor(parcel_district) + as.factor(middlesch), data=data_sub)

summary(mod1)

vcov_firm <- cluster.vcov(mod1, as.factor(data_sub$BlockGroup))
coeftest(mod1, vcov_firm)



























fmv <- ass_last*2.5
sales price <- salesprice
tax <- tax_last


d <- read.csv("ATL data.csv")
d %>% select(concat, S2, S3, S4) -> d
#data <- merge(data, d, by=c("concat"))

data <- read.csv("atlanta single family 6116.csv")
data <- data[1:4978,] #3626 #4978

data <- merge(data, d, by=c("concat"))

data <- subset(data, taxpaid>0)
summary(data$taxpaid)
data$sqft4 <- as.numeric(data$sqft3)
data <- subset(data, sqft4>0)

summary(data$sqft4)

#data %>% select(salesprice, calcacres, stories, age, centheat, totbath, ENGMeanScaleScore15, median_income, sqft,
#median_house_value, HHsize, pct_black, pct_white, landuse, taxpaid_ass, age_sq, tax_last, sqft4, ass_last, ass) -> data

summary(data)

data %>%
  mutate(quantile = ntile(pct_black, 4)) -> data1

data1$quantile <- as.factor(data1$quantile)

levels(data1$quantile)

group_by(data1, quantile) %>%
  summarise(
    count = n(),
    
    mean_pct_black = mean(pct_black, na.rm=TRUE),
    
    mean_tax_fmv = mean(I((tax_last/1)/(ass_last*2.5)), na.rm = TRUE),
    sd_tax_fmv = sd(I((tax_last/1)/(ass_last*2.5)), na.rm = TRUE),
    
    mean_fmv_sp = mean(I((ass_last*2.5)/(salesprice)), na.rm = TRUE),
    sd_fmv_sp = sd(I((ass_last*2.5)/(salesprice)), na.rm = TRUE),
    
    mean_tax_sp = mean(I((tax_last)/(salesprice)), na.rm = TRUE),
    sd_tax_sp = sd(I((tax_last)/(salesprice)), na.rm = TRUE)
    
  )

data1$ass_sp <- (data1$ass_last*2.5)/(data1$salesprice)
data1 <- subset(data1, ass_sp<3)

data1$tax_fmv <- data1$tax_last/(data1$ass_last*2.5)
data1 <- subset(data1, tax_fmv < 0.2)

data1$tax_sp <- data1$tax_last/(data1$salesprice)
#data1 <- subset(data1, tax_fmv < 0.2)

plot <- ggplot(data1, aes(x=factor(quantile), y=tax_sp, fill = S4))+
  geom_boxplot()+xlab("Black % Quartile")+ylab("Tax to Sales Price")+
  theme( legend.position = "none" ) + geom_smooth(method = "lm", se=FALSE, color="black", group = 1)

plot



plot <- ggplot(data1, aes(x=factor(quantile), y=tax_fmv, fill = S4))+
  geom_violin(position="dodge", alpha=0.5, outlier.colour="transparent", trim=TRUE, draw_quantiles = c(0.5))+
  xlab("Black % quartile")+ylab("Tax to Fair Market Value")+
  theme( legend.position = "none" )

plot







data %>%
  mutate(quantile = ntile(pct_black, 10)) -> data2

data2$quantile <- as.factor(data2$quantile)

data2$ass_sp <- (data2$ass_last*2.5)/(data2$salesprice)
data2 <- subset(data2, ass_sp<3)

data2$tax_fmv <- data2$tax_last/(data2$ass_last*2.5)
data2 <- subset(data2, tax_fmv < 0.2)

data2$tax_sp <- data2$tax_last/(data2$salesprice)
#data2 <- subset(data2, tax_fmv < 0.2)

ggplot(data2, aes(x = pct_black, y = tax_sp, colour =factor(quantile))) + 
  geom_point() +
  stat_smooth(data=data2,
              method = "lm", se = T) + xlab("Black %")+ylab("Tax to Sales Price") + theme( legend.position = "none" )




d <- read.csv("ATL data.csv")
d %>% select(concat, S2, S3, S4) -> d
#data <- merge(data, d, by=c("concat"))

data <- read.csv("atlanta single family 6116.csv")
data <- data[1:4978,] #3626 #4978

data <- merge(data, d, by=c("concat"))

data <- subset(data, taxpaid>0)
summary(data$taxpaid)
data$sqft4 <- as.numeric(data$sqft3)
data <- subset(data, sqft4>0)

summary(data$sqft4)

data$ass_sp <- (data$ass_last*2.5)/(data$salesprice)

data$tax_fmv <- data$tax_last/(data$ass_last*2.5)

data$tax_sp <- data$tax_last/data$salesprice

data %>%
  mutate(quantile = ntile(pct_black, 4)) -> data1

res <- aov(tax_sp ~ quantile, data=data1)
summary(res)



TukeyHSD(res)

pairwise.t.test(data1$taxpaid_ass_last, data1$quantile, p.adjust.method="BH")





d <- read.csv("ATL data.csv")
d %>% select(concat, S2, S3, S4) -> d
#data <- merge(data, d, by=c("concat"))

data <- read.csv("atlanta single family 6116.csv")
data <- data[1:4978,] #3626 #4978

data <- merge(data, d, by=c("concat"))

data <- subset(data, taxpaid>0)
summary(data$taxpaid)
data$sqft4 <- as.numeric(data$sqft3)
data <- subset(data, sqft4>0)

summary(data$sqft4)

data$ass_sp <- (data$ass_last*2.5)/(data$salesprice)

data$tax_fmv <- data$tax_last/(data$ass_last*2.5)



data %>%
  mutate(quantile = ntile(pct_black, 10)) -> data1

plot <- ggplot(data1, aes(x=factor(quantile), y=tax_fmv))+
  geom_boxplot()+xlab("Black % Quartile")+ylab("Fair Market Value to Sales Price")+
  theme( legend.position = "none" ) + geom_smooth(method = "lm", se=FALSE, color="black", group = 1)

plot





#https://jamescheshire.github.io/learningR/mapping-point-data-in-r.html

library(tidycensus)
options(tigris_use_cache = TRUE)

hennepin_race <- get_decennial(
  geography = "tract",
  state = "GA",
  county = "Fulton",
  variables = c(
    Hispanic = "P2_002N",
    White = "P2_005N",
    Black = "P2_006N",
    Native = "P2_007N",
    Asian = "P2_008N"
  ),
  summary_var = "P2_001N",
  year = 2020,
  geometry = TRUE
) %>%
  mutate(percent = 100 * (value / summary_value))

library(tmap)
hennepin_black <- filter(hennepin_race, 
                         variable == "Black")

tm_shape(hennepin_black) + 
  tm_polygons()

tm_shape(hennepin_black) + 
  tm_polygons(col = "percent")




tm_shape(hennepin_black) + 
  tm_polygons(col = "percent",
              style = "quantile",
              n = 5,
              palette = "Purples",
              title = "2020 US Census") + 
  tm_layout(title = "Percent Black\nby Census tract",
            frame = FALSE,
            legend.outside = TRUE)



data$ass_sp <- (data$ass_last*2.5)/(data$salesprice)
#data <- subset(data, ass_sp<3)

data$tax_fmv <- data$tax_last/(data$ass_last*2.5)
#data <- subset(data, tax_fmv < 0.2)

data$tax_sp <- data$tax_last/data$salesprice

df <- sf::st_as_sf(data, coords = c("lon","lat"))

df$logtax_fmv <- log(data$tax_fmv)


hennepin_race <- get_decennial(
  geography = "tract",
  state = "GA",
  county = "Fulton",
  variables = c(
    Hispanic = "P2_002N",
    White = "P2_005N",
    Black = "P2_006N",
    Native = "P2_007N",
    Asian = "P2_008N"
  ),
  summary_var = "P2_001N",
  year = 2020,
  geometry = TRUE
) %>%
  mutate(percent = 100 * (value / summary_value))


hennepin_black <- filter(hennepin_race, 
                         variable == "Black")

tm_shape(hennepin_black) + 
  tm_polygons(col = "percent") + tm_shape(df) + tm_dots(col="logtax_fmv", size = 0.05, alpha = 0.5, 
                                                        scale = 1.5, palette = "Reds", style = "quantile")


tm_shape(hennepin_black) + 
  tm_polygons(col = "percent",
              style = "quantile",
              n = 5,
              palette = "Purples",
              title = "2020 US Census") + 
  tm_layout(title = "Percent Black\nby Census tract",
            frame = FALSE,
            legend.outside = TRUE) + tm_shape(df) + tm_dots(col="logtax_fmv", size = 0.05, alpha = 0.5, 
                                                            scale = 1.5, palette = "Reds", style = "quantile")


hennepin_black = sf::st_crop(hennepin_black, xmin=-84.25, xmax=-84.60, ymin=33.62, ymax=33.90)


current.mode <- tmap_mode("plot")
tmap_mode("view")


tm_shape(hennepin_black) + 
  tm_polygons(col = "percent",
              style = "quantile",
              n = 5,
              palette = "Blues",
              title = "Percent Black\nby Census tract") + 
  tm_shape(df) + tm_bubbles(size="tax_sp", col="tax_sp", palette = "Reds", style = "quantile", legend.size.show = FALSE,
                            border.lwd = 0.1, border.alpha = 0.1, border.col = "black",
                            title.col = "Tax/Sales Price", alpha = 0.5)


tm_shape(hennepin_black) + 
  tm_polygons(style = "quantile",
              n = 5,
              palette = "Blues",
              title = "2020 US Census") + tm_borders(alpha=0.4) + tm_shape(df) + tm_bubbles(size="tax_fmv", col="tax_fmv", 
                                                                                            palette = "Reds", style = "quantile", legend.size.show = FALSE,
                                                                                            border.lwd = 0.1, border.alpha = 0.1, border.col = "black",
                                                                                            title.col = "Tax/FMV", alpha = 0.5)



df$Submarket2 <- ifelse(df$S2=="S2_1", "Submarket 1", "Submarket 2")
df$Submarket3 <- ifelse(df$S3=="S3_1", "Submarket 1", ifelse(df$S3=="S3_2", "Submarket 2", "Submarket 3"))
df$Submarket4 <- ifelse(df$S4=="S4_1", "Submarket 1", ifelse(df$S4=="S4_2", "Submarket 2", ifelse(df$S4=="S4_3", "Submarket 3", "Submarket 4")))


hennepin_income <- get_acs(
  geography = "tract",
  state = "GA",
  county = "Fulton",
  variables = c(
    income = "B19013_001"
  ),
  year = 2018,
  geometry = TRUE
)


hennepin_income = sf::st_crop(hennepin_income, xmin=-84.25, xmax=-84.60, ymin=33.62, ymax=33.90)


tm_shape(hennepin_income) + 
  tm_polygons(col = "estimate",
              style = "quantile",
              n = 5,
              palette = "Blues",
              title = "Median Income\nby Census tract") + 
  tm_shape(df) + tm_dots(col="Submarket4", size = 0.05, alpha = 0.5, 
                         scale = 1, style = "quantile")


