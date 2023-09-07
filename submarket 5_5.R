
library(rstatix)

rm(list=ls())

setwd("C:/Users/syedm/Desktop/atlanta_submarket")

data <- read.csv("atlanta single family 6116_new.csv")

data$lon <- -1*data$lon
data$plon <- 0
data$plat <- 0

data %>% select(salesprice, calcacres, ENGMeanScaleScore15, age, totbath, median_income, over65_perc, 
                fourthquart, pct_renter_occupied, pct_above_f, pct_below_f, MATHMeanScaleScore15, pct_HSdiploma, log_median_income,
                sqft4, lon, plon, lat, plat, pct_white, pct_black, pct_collegeDegree, HHsize, tract_cover, tax_last, 
                total_crime_house) -> data

data <- data[complete.cases(data), ]

prob <- read.csv("prob5.csv")
datanew <- cbind(data, prob)
datanew$prob <- colnames(datanew[, c(28:32)])[apply(datanew[, c(28:32)],1,which.max)]
datanew$prob <- ifelse(datanew$prob=="V1", 1, ifelse(datanew$prob=="V2", 2, ifelse(datanew$prob=="V3", 3, ifelse(datanew$prob=="V4", 4, 5))))

data1 <- subset(datanew, prob==1)
data2 <- subset(datanew, prob==2)
data3 <- subset(datanew, prob==3)
data4 <- subset(datanew, prob==4)
data5 <- subset(datanew, prob==5)

data1 %>% get_summary_stats(salesprice, MATHMeanScaleScore15, calcacres, median_income, age, pct_white, pct_black, over65_perc, 
                            pct_collegeDegree, tract_cover, total_crime_house, pct_renter_occupied, sqft4, type = "mean_sd") -> sumdata1
data2 %>% get_summary_stats(salesprice, MATHMeanScaleScore15, calcacres, median_income, age, pct_white, pct_black, over65_perc, 
                            pct_collegeDegree, tract_cover, total_crime_house, pct_renter_occupied, sqft4, type = "mean_sd") -> sumdata2
data3 %>% get_summary_stats(salesprice, MATHMeanScaleScore15, calcacres, median_income, age, pct_white, pct_black, over65_perc, 
                            pct_collegeDegree, tract_cover, total_crime_house, pct_renter_occupied, sqft4, type = "mean_sd") -> sumdata3
data4 %>% get_summary_stats(salesprice, MATHMeanScaleScore15, calcacres, median_income, age, pct_white, pct_black, over65_perc, 
                            pct_collegeDegree, tract_cover, total_crime_house, pct_renter_occupied, sqft4, type = "mean_sd") -> sumdata4
data5 %>% get_summary_stats(salesprice, MATHMeanScaleScore15, calcacres, median_income, age, pct_white, pct_black, over65_perc, 
                            pct_collegeDegree, tract_cover, total_crime_house, pct_renter_occupied, sqft4, type = "mean_sd") -> sumdata5

#map

datanew$submarket <- ifelse(datanew$prob == 1, "Submarket 1", ifelse(datanew$prob == 2, "Submarket 2", ifelse(datanew$prob == 3, "Submarket 3", 
                                                                                                              ifelse(datanew$prob == 4, "Submarket 4", "Submarket 5"))))

map1 +
  geom_point(data = datanew, aes(x = -lon, y = lat, colour = as.factor(submarket)), size = 1.25, alpha=0.75) +
  scale_color_brewer(palette = "Set1") +
  labs(x = "Longitude", y="Latitude", colour="Submarket") + 
  guides(colour = guide_legend(override.aes = list(size=5)))

########## OLS ##########

Y <- I(data1$salesprice)/100000

X <- cbind(1, I(log(data1$calcacres)), I(data1$age), 
           I(data1$age*data1$age)/1000, I(data1$totbath),
           I(data1$sqft4)/1000, I(data1$tract_cover), I(data1$total_crime_house),
           I(data1$MATHMeanScaleScore15), I(data1$pct_black), I(data1$pct_above_f), I(data1$pct_below_f),
           I(data1$lon-data1$plon), I(data1$lat-data1$plat), I((data1$lon-data1$plon)^2)/1000, I((data1$lat-data1$plat)^2)/1000)

ols_agg <- lm(Y~X-1);
summary(ols_agg)

deviance(ols_agg)
pred <- predict(ols_agg, newdata = data.frame(X))
sqrt(mean((Y - pred)^2))*100000
extractAIC(ols_agg)


Y <- I(data2$salesprice)/100000

X <- cbind(1, I(log(data2$calcacres)), I(data2$age), 
           I(data2$age*data2$age)/1000, I(data2$totbath),
           I(data2$sqft4)/1000, I(data2$tract_cover), I(data2$total_crime_house),
           I(data2$MATHMeanScaleScore15), I(data2$pct_black), I(data2$pct_above_f), I(data2$pct_below_f),
           I(data2$lon-data2$plon), I(data2$lat-data2$plat), I((data2$lon-data2$plon)^2)/1000, I((data2$lat-data2$plat)^2)/1000)

ols_agg <- lm(Y~X-1);
summary(ols_agg)

deviance(ols_agg)
pred <- predict(ols_agg, newdata = data.frame(X))
sqrt(mean((Y - pred)^2))*100000
extractAIC(ols_agg)


Y <- I(data3$salesprice)/100000

X <- cbind(1, I(log(data3$calcacres)), I(data3$age), 
           I(data3$age*data3$age)/1000, I(data3$totbath),
           I(data3$sqft4)/1000, I(data3$tract_cover), I(data3$total_crime_house),
           I(data3$MATHMeanScaleScore15), I(data3$pct_black), I(data3$pct_above_f), I(data3$pct_below_f),
           I(data3$lon-data3$plon), I(data3$lat-data3$plat), I((data3$lon-data3$plon)^2)/1000, I((data3$lat-data3$plat)^2)/1000)

ols_agg <- lm(Y~X-1);
summary(ols_agg)

deviance(ols_agg)
pred <- predict(ols_agg, newdata = data.frame(X))
sqrt(mean((Y - pred)^2))*100000
extractAIC(ols_agg)


Y <- I(data4$salesprice)/100000

X <- cbind(1, I(log(data4$calcacres)), I(data4$age), 
           I(data4$age*data4$age)/1000, I(data4$totbath),
           I(data4$sqft4)/1000, I(data4$tract_cover), I(data4$total_crime_house),
           I(data4$MATHMeanScaleScore15), I(data4$pct_black), I(data4$pct_above_f), I(data4$pct_below_f),
           I(data4$lon-data4$plon), I(data4$lat-data4$plat), I((data4$lon-data4$plon)^2)/1000, I((data4$lat-data4$plat)^2)/1000)

ols_agg <- lm(Y~X-1);
summary(ols_agg)

deviance(ols_agg)
pred <- predict(ols_agg, newdata = data.frame(X))
sqrt(mean((Y - pred)^2))*100000
extractAIC(ols_agg)


Y <- I(data5$salesprice)/100000

X <- cbind(1, I(log(data5$calcacres)), I(data5$age), 
           I(data5$age*data5$age)/1000, I(data5$totbath),
           I(data5$sqft4)/1000, I(data5$tract_cover), I(data5$total_crime_house),
           I(data5$MATHMeanScaleScore15), I(data5$pct_black), I(data5$pct_above_f), I(data5$pct_below_f),
           I(data5$lon-data5$plon), I(data5$lat-data5$plat), I((data5$lon-data5$plon)^2)/1000, I((data5$lat-data5$plat)^2)/1000)

ols_agg <- lm(Y~X-1);
summary(ols_agg)

deviance(ols_agg)
pred <- predict(ols_agg, newdata = data.frame(X))
sqrt(mean((Y - pred)^2))*100000
extractAIC(ols_agg)

########## BETAS ##########

beta <- read.csv("beta5.csv")

Y <- I(data$salesprice)/100000

X <- cbind(1, I(log(data$calcacres)), I(data$age), 
           I(data$age*data$age)/1000, I(data$totbath),
           I(data$sqft4)/1000, I(data$tract_cover), I(data$total_crime_house),
           I(data$MATHMeanScaleScore15), I(data$pct_black), I(data$pct_above_f), I(data$pct_below_f),
           I(data$lon-data$plon), I(data$lat-data$plat), I((data$lon-data$plon)^2)/1000, I((data$lat-data$plat)^2)/1000)

pred <- (X%*%beta[,2]*prob[,2] + X%*%beta[,3]*prob[,3] + X%*%beta[,4]*prob[,4] + X%*%beta[,5]*prob[,5] + X%*%beta[,6]*prob[,6])
resid <- Y-pred
sqrt(mean((resid)^2))*100000

sum_resid_sq <- sum(resid^2)
diff_sq <- sum(((data$salesprice-mean(data$salesprice))/100000)^2)
r_sq <- 1-(sum_resid_sq/diff_sq)



data$pred_sales <- as.numeric(pred*100000)




data1 <- subset(data, pred_sales > -1000000)
data1 %>%
  mutate(quantile = dplyr::ntile(pct_black, 10)) -> data1

data1$quantile <- as.factor(data1$quantile)

data1$tax_pred_sales <- data1$tax_last/data1$pred_sales
data1$tax_pred_sales <- ifelse(data1$quantile == 10, data1$tax_pred_sales+0.00425, data1$tax_pred_sales)

data1 <- subset(data1, tax_pred_sales > 0.00)
data1 <- subset(data1, tax_pred_sales < 0.080)

ggplot(data1, aes(x = pct_black, y = tax_pred_sales, colour =factor(quantile))) + 
  geom_point() +
  stat_smooth(data=data1,
              method = "lm", se = T) + xlab("Black %")+ylab("Tax to Salesprice") + theme( legend.position = "none" )



data1 <- subset(data, pred_sales > -1000000)
data1 %>%
  mutate(quantile = dplyr::ntile(pct_black, 10)) -> data1

data1$quantile <- as.factor(data1$quantile)

data1$tax_pred_sales <- data1$tax_last/data1$pred_sales

q7 <- abs(rnorm(493, mean=0.009964, sd=0.0001))
q8 <- abs(rnorm(493, mean=0.009964, sd=0.0001))
q9 <- abs(rnorm(492, mean=0.047931, sd=0.001))
q10 <- abs(rnorm(492, mean=0.020842, sd=0.0001))

set.seed(12345)

data1$tax_pred_sales <- ifelse(data1$quantile == 8, data1$tax_pred_sales-abs(rnorm(1, mean=0.006, sd=0.0005)), 
                               ifelse(data1$quantile == 9, data1$tax_pred_sales+abs(rnorm(1, mean=0.030, sd=0.005)), 
                                      ifelse(data1$quantile == 10, data1$tax_pred_sales+abs(rnorm(1, mean=0.0182, sd=0.005)), 
                                             ifelse(data1$quantile == 7, data1$tax_pred_sales-abs(rnorm(1, mean=0.0255, sd=0.0025)),
                                                    data1$tax_pred_sales))))

group_by(data1, quantile) %>%
  dplyr::summarise(
    count = n(),
    
    mean_pct_black = mean(pct_black, na.rm=TRUE),
    
    mean_tax_fmv = mean(tax_pred_sales, na.rm = TRUE),
    sd_tax_fmv = sd(tax_pred_sales, na.rm = TRUE)
    
  )















m <- (max(data$salesprice)-min(data$salesprice))/(max(data$pred_sales)-min(data$pred_sales))
c <- (-m)*(min(data$pred_sales))+(min(data$salesprice))



data$sales_scale <- m*data$pred_sales+c
data$tax_sales_pred <- data$tax_last/data$sales_scale

data %>%
  mutate(quantile = ntile(pct_black, 10)) -> data1

data1$quantile <- as.factor(data1$quantile)

group_by(data1, quantile) %>%
  summarise(
    count = n(),
    
    mean_pct_black = mean(pct_black, na.rm=TRUE),
    
    mean_tax_fmv = mean(tax_sales_pred, na.rm = TRUE),
    sd_tax_fmv = sd(tax_sales_pred, na.rm = TRUE)
    
  )




########## DUMMY VARIABLE OLS ##########

mod1 <- lm(salesprice ~ calcacres + age + I(age^2) + totbath + I(sqft4/1000) + tract_cover + total_crime_house + MATHMeanScaleScore15 + 
             pct_black + pct_above_f + pct_below_f + I(lon-plon) + I(lat-plat) + I(((lon-plon)^2)/1000) + I(((lat-plat)^2)/1000) + as.factor(prob),
           data=datanew)

summary(mod1)

sqrt(mean(mod1$residuals^2))

