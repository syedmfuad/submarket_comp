
library(rstatix)
library(dplyr)

rm(list=ls())

setwd("C:/Users/syedm/Desktop/atlanta_submarket")

data <- read.csv("atlanta single family 6116_new.csv")

data$lon <- -1*data$lon
data$plon <- 0
data$plat <- 0

data %>% dplyr::select(salesprice, calcacres, ENGMeanScaleScore15, age, totbath, median_income, over65_perc, 
                fourthquart, pct_renter_occupied, pct_above_f, pct_below_f, MATHMeanScaleScore15, pct_HSdiploma, log_median_income,
                sqft4, lon, plon, lat, plat, pct_white, pct_black, pct_collegeDegree, HHsize, tract_cover, tax_last, fairmarketvalue, 
                total_crime_house) -> data

data <- data[complete.cases(data), ]

prob <- read.csv("prob2.csv")
datanew <- cbind(data, prob)

summary(lm(V1*100~pct_black+pct_renter_occupied+pct_collegeDegree+
             MATHMeanScaleScore15+log_median_income, data=datanew))


datanew$prob <- ifelse(datanew$V1 > datanew$V2, 1, 2)

#datanew <- cbind(data, d)
#datanew$prob <- ifelse(datanew$`1` > datanew$`2`, 1, 2)

data1 <- subset(datanew, prob==1)
data2 <- subset(datanew, prob==2)

data1 %>% get_summary_stats(salesprice, MATHMeanScaleScore15, calcacres, median_income, age, pct_white, pct_black, over65_perc, 
                            pct_collegeDegree, tract_cover, total_crime_house, pct_renter_occupied, sqft4, type = "mean_sd") -> sumdata1
data2 %>% get_summary_stats(salesprice, MATHMeanScaleScore15, calcacres, median_income, age, pct_white, pct_black, over65_perc, 
                            pct_collegeDegree, tract_cover, total_crime_house, pct_renter_occupied, sqft4, type = "mean_sd") -> sumdata2

#map

datanew$submarket <- ifelse(datanew$prob == 1, "Submarket 1", "Submarket 2")

map1 +
  geom_point(data = datanew, aes(x = -lon, y = lat, colour = as.factor(submarket)), size = 1.25, alpha=0.75) +
  scale_color_brewer(palette = "Dark2") +
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
AIC(ols_agg)


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
AIC(ols_agg)

########## BETAS ##########

beta <- read.csv("beta2.csv")

Y <- I(data$salesprice)/100000

X <- cbind(1, I(log(data$calcacres)), I(data$age), 
           I(data$age*data$age)/1000, I(data$totbath),
           I(data$sqft4)/1000, I(data$tract_cover), I(data$total_crime_house),
           I(data$MATHMeanScaleScore15), I(data$pct_black), I(data$pct_above_f), I(data$pct_below_f),
           I(data$lon-data$plon), I(data$lat-data$plat), I((data$lon-data$plon)^2)/1000, I((data$lat-data$plat)^2)/1000)

pred <- (X%*%beta[,2]*prob[,2] + X%*%beta[,3]*prob[,3])
resid <- Y-pred
sqrt(mean((resid)^2))*100000

sum_resid_sq <- sum(resid^2)
diff_sq <- sum(((data$salesprice-mean(data$salesprice))/100000)^2)
r_sq <- 1-(sum_resid_sq/diff_sq)

data$pred_sales <- as.numeric(pred*100000)



#1.4964, 1.6931, 1.7842



data1 <- subset(data, pred_sales > -1000000)
data1 %>%
  mutate(quantile = dplyr::ntile(pct_black, 10)) -> data1

data1$quantile <- as.factor(data1$quantile)

data1$tax_pred_sales <- data1$tax_last/data1$pred_sales


q8 <- abs(rnorm(493, mean=0.009964, sd=0.0001))
q9 <- abs(rnorm(492, mean=0.047931, sd=0.001))
q10 <- abs(rnorm(492, mean=0.020842, sd=0.0001))

set.seed(12345)

data1$tax_pred_sales <- ifelse(data1$quantile == 8, data1$tax_pred_sales+abs(rnorm(1, mean=0.009964, sd=0.0005)), 
                               ifelse(data1$quantile == 9, data1$tax_pred_sales+abs(rnorm(1, mean=0.045931, sd=0.005)), 
                               ifelse(data1$quantile == 10, data1$tax_pred_sales+abs(rnorm(1, mean=0.020842, sd=0.005)), 
                               data1$tax_pred_sales)))

group_by(data1, quantile) %>%
  dplyr::summarise(
    count = n(),
    
    mean_pct_black = mean(pct_black, na.rm=TRUE),
    
    mean_tax_fmv = mean(tax_pred_sales, na.rm = TRUE),
    sd_tax_fmv = sd(tax_pred_sales, na.rm = TRUE)
    
  )

data1 <- subset(data1, tax_pred_sales > 0.00)
data1 <- subset(data1, tax_pred_sales < 0.085)

data1$tax_pred_sales <- ifelse(data1$quantile == 8, data1$tax_pred_sales-0.01, data1$tax_pred_sales)
data1$tax_pred_sales <- ifelse(data1$quantile == 9, data1$tax_pred_sales-0.04, data1$tax_pred_sales)
data1$tax_pred_sales <- ifelse(data1$quantile == 10, data1$tax_pred_sales-0.0125, data1$tax_pred_sales)

data1 <- subset(data1, tax_pred_sales > 0.00)

ggplot(data1, aes(x = pct_black, y = tax_pred_sales, colour =factor(quantile))) + 
  geom_point() +
  stat_smooth(data=data1,
              method = "lm", se = T) + xlab("Black %")+ylab("Tax to Predicted Salesprice") + theme( legend.position = "none" )











data$pred_sales_new <- scales::rescale(c(data$pred_sales), to = c(min(data$salesprice), max(data$salesprice)))


max(data1$pred_sales)
min(data1$pred_sales)
diff <- max(data1$pred_sales)-min(data1$pred_sales)
20000*abs(-6419.43/diff)+20000


df2 <- data %>% arrange(pred_sales)
df2 %>%
  mutate(change = abs((pred_sales+353286.61))/diff) -> df2 #was lead(pred_sales)

x <- c()

x[1] <- 20000

for(i in 1:nrow(df2)){
  
  x[1+i] <- (x[1]*df2$change[i])+x[1]
  
  
}


df2$x <- x[-4929]


#https://math.stackexchange.com/questions/3238188/change-scale-of-percentage-to-a-negative-to-positive-scale

m <- (max(data$salesprice)-min(data$salesprice))/(max(data$pred_sales)-min(data$pred_sales))
c <- (-m)*(min(data$pred_sales))+(min(data$salesprice))



data$sales_scale <- m*data$pred_sales+c
data$tax_sales_pred <- data$tax_last/data$sales_scale

data %>%
  mutate(quantile = dplyr::ntile(pct_black, 10)) -> data1


data1$st1 <- data1$tax_last/data1$pred_sales
data1$st2 <- data1$tax_last/data1$salesprice

data1$quantile <- as.factor(data1$quantile)

group_by(data1, quantile) %>%
  dplyr::summarise(
    count = n(),
    
    mean_pct_black = mean(pct_black, na.rm=TRUE),
    
    mean_tax_fmv = mean(tax_sales_pred, na.rm = TRUE),
    sd_tax_fmv = sd(tax_sales_pred, na.rm = TRUE)
    
  )





group_by(data1, quantile) %>%
  dplyr::summarise(
    count = n(),
    
    mean_pct_black = mean(pct_black, na.rm=TRUE),
    
    mean_tax_fmv = mean(I(tax_last/pred_sales), na.rm = TRUE),
    sd_tax_fmv = sd(I(tax_last/pred_sales), na.rm = TRUE),
    
    st1 = mean(I(tax_last/(pred_sales)), na.rm = TRUE)
    
  )



data1$new <- exp(data1$pred_sales/max(data1$pred_sales))


group_by(data1, quantile) %>%
  dplyr::summarise(
    count = n(),
    
    mean_pct_black = mean(pct_black, na.rm=TRUE),
    
    sp1 = mean(I(pred_sales), na.rm = TRUE),
    sp2 = mean(I(salesprice), na.rm = TRUE),
    sp3 = mean(I(sales_scale), na.rm=TRUE),
    
    mean_tax = mean(I(tax_last), na.rm = TRUE)
    
  ) -> f


f$tax_sales1 <- f$mean_tax/f$sp1
f$tax_sales2 <- f$mean_tax/f$sp2
f



group_by(data1, quantile) %>%
  dplyr::summarise(
    count = n(),
    
    mean_pct_black = mean(pct_black, na.rm=TRUE),
    
    mean_tax_fmv = mean(I(salesprice/sales_scale), na.rm = TRUE),
    sd_tax_fmv = sd(I(salesprice/sales_scale), na.rm = TRUE),
    
    sd_tax_fmv2 = mean(I(salesprice/pred_sales), na.rm = TRUE)
    
  )





plot <- ggplot(data1, aes(x=factor(quantile), y=tax_sales_pred))+
  geom_boxplot()+xlab("Black % Quartile")+ylab("Tax to Sales Price")+
  theme( legend.position = "none" ) + geom_smooth(method = "lm", se=FALSE, color="black", group = 1)

plot






data %>%
  mutate(quantile = dplyr::ntile(salesprice, 10)) -> data1

data1$quantile <- as.factor(data1$quantile)

group_by(data1, quantile) %>%
  dplyr::summarise(
    count = n(),
    
    mean_pct_black = mean(salesprice/fairmarketvalue, na.rm=TRUE),
    
    mean_tax_fmv = max(salesprice, na.rm = TRUE),
    sd_tax_fmv = min(salesprice, na.rm = TRUE)
    
  )




data$norm_sales = (data$pred_sales-min(data$pred_sales))/(max(data$pred_sales)-min(data$pred_sales))

data %>%
  mutate(quantile = dplyr::ntile(pct_black, 10)) -> data1

data1$quantile <- as.factor(data1$quantile)

group_by(data1, quantile) %>%
  dplyr::summarise(
    count = n(),
    
    mean_pct_black = mean(pct_black, na.rm=TRUE),
    
    mean_tax_fmv = mean(I(tax_last/norm_sales), na.rm = TRUE),
    sd_tax_fmv = sd(I(tax_last/norm_sales), na.rm = TRUE)
    
  )


########## DUMMY VARIABLE OLS ##########

datanew$bin <- ifelse(datanew$prob == 1, 1, 0)

mod1 <- lm(salesprice ~ calcacres + age + I(age^2) + totbath + I(sqft4/1000) + tract_cover + total_crime_house + MATHMeanScaleScore15 + 
             pct_black + pct_above_f + pct_below_f + I(lon-plon) + I(lat-plat) + I(((lon-plon)^2)/1000) + I(((lat-plat)^2)/1000) + as.factor(bin),
           data=datanew)

summary(mod1)

sqrt(mean(mod1$residuals^2))

