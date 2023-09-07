
library(ggmap)

rm(list=ls())

setwd("C:/Users/syedm/Desktop/atlanta_submarket")

data <- read.csv("atlanta single family 6116_new.csv")

summary(as.factor(data$Schoolname))

summary(as.factor(data$SCHcode))

#API
ggmap::register_google(key = "AIzaSyA-UiMPuDvL_CFU34lyIDtow_B020R_sh8")

#getting Atlanta map
get_local_spot <-  get_map("Atlanta Georgia", maptype = "roadmap", zoom = 11) 
map1 <- ggmap(get_local_spot)

map1 +
  geom_point(data = data, aes(x = lon, y = lat, colour = Schoolname), size = 1, alpha=0.75) +
  scale_color_brewer(palette = "Paired") +
  labs(x = "Longitude", y="Latitude", colour="Middle school district") + 
  guides(colour = guide_legend(override.aes = list(size=5)))


map1 +
  geom_point(data = data, aes(x = lon, y = lat, colour = as.factor(parcel_district)), size = 1, alpha=0.75) +
  scale_color_brewer(palette = "Dark2") +
  labs(x = "Longitude", y="Latitude", colour="Parcel district") + 
  guides(colour = guide_legend(override.aes = list(size=5)))






data_combinations <- combn(unique(data$Schoolname), 5)

output <- matrix(ncol=3, nrow=252)

for (i in 1:252){
  
  subset_data <- subset(data, Schoolname==data_combinations[,i][1] | Schoolname==data_combinations[,i][2] | 
                          Schoolname==data_combinations[,i][3] | Schoolname==data_combinations[,i][4] | Schoolname==data_combinations[,i][5])
  model <- lm(salesprice ~ sqft4 + I(sqft4*ENGMeanScaleScore15) + age + I(age^2), data = subset_data)
  output[i, 1] <- paste(data_combinations[,i][1], data_combinations[,i][2], data_combinations[,i][3], data_combinations[,i][4], data_combinations[,i][5])
  output[i, 2] <- summary(model)$coefficient[3,4]
  output[i, 3] <- abs(summary(model)$coefficient[3,3])
  
}

output <- as.data.frame(output)
output$decision <- ifelse(output$V3 > 1.96, "Not Same", "Same")
output


aggregate(salesprice ~ Schoolname, data = data, mean)



data1 <- data
data1$school <- ifelse(data1$Schoolname=="PRICE MIDDLE SCHOOL" | data1$Schoolname=="SYLVAN HILLS MIDDLE SCHOOL" | 
                         data1$Schoolname=="BROWN MIDDLE SCHOOL" | data1$Schoolname=="YOUNG MIDDLE SCHOOL" | 
                         data1$Schoolname=="LONG MIDDLE SCHOOL", "PRICE + SYLVAN + BROWN + YOUNG + LONG", data1$Schoolname)

map1 +
  geom_point(data = data1, aes(x = lon, y = lat, colour = school), size = 1, alpha=0.75) +
  scale_color_brewer(palette = "Paired") +
  labs(x = "Longitude", y="Latitude", colour="Middle school district") + 
  guides(colour = guide_legend(override.aes = list(size=5)))

aggregate(salesprice ~ school, data = data1, mean)

grouped_means <- data1 %>%
  group_by(school) %>%
  summarize(price = sd(salesprice), 
            test = sd(ENGMeanScaleScore15),
            sqft = sd(sqft4),
            income = sd(median_income, na.rm=TRUE),
            age = sd(age),
            pct_white = sd(pct_white),
            pct_black = sd(pct_black),
            college = sd(pct_collegeDegree))

grouped_means

write.table(grouped_means , file = "shit_temp.csv")




[1] "PRICE + SYLVAN + BROWN + YOUNG + LONG" "KING MIDDLE SCHOOL"                   
[3] "INMAN MIDDLE SCHOOL"                   "HARPER-ARCHER MIDDLE SCHOOL"          
[5] "BUNCHE MIDDLE SCHOOL"                  "SUTTON MIDDLE SCHOOL"

setwd("C:/Users/syedm/Desktop/atlanta_submarket")

data <- read.csv("atlanta single family 6116_new.csv")

data$lon <- -1*data$lon
data$plon <- 0
data$plat <- 0

data$school <- ifelse(data$Schoolname=="PRICE MIDDLE SCHOOL" | data$Schoolname=="SYLVAN HILLS MIDDLE SCHOOL" | 
                      data$Schoolname=="BROWN MIDDLE SCHOOL" | data$Schoolname=="YOUNG MIDDLE SCHOOL" | 
                      data$Schoolname=="LONG MIDDLE SCHOOL", "PRICE + SYLVAN + BROWN + YOUNG + LONG", data$Schoolname)

data <- subset(data, school == "BUNCHE MIDDLE SCHOOL")
mean(data$salesprice)

model1 <- lm(I(salesprice/100000) ~ log(calcacres) + age + I((age*age)/1000) + totbath + I(sqft4/1000) + tract_cover + total_crime_house +
             MATHMeanScaleScore15 + pct_black + pct_above_f + pct_below_f + I(lon-plon) + I(lat-plat) +  
             I(((lon-plon)^2)/1000) + I(((lat-plat)^2)/1000), data=data)

summary(model1)

deviance(model1)
pred <- predict(model1)
extractAIC(model1)

nrow(data)*(log(2*pi)+1+log((sum(model1$residuals^2)/nrow(data))))+((length(model1$coefficients)+1)*2)
AIC(model1)
sqrt(mean((data$salesprice - (pred*100000))^2))
sqrt(mean(model1$residuals^2))*100000
nrow(data)





setwd("C:/Users/syedm/Desktop/atlanta_submarket")

data <- read.csv("atlanta single family 6116_new.csv")

data$lon <- -1*data$lon
data$plon <- 0
data$plat <- 0

data$school <- ifelse(data$Schoolname=="PRICE MIDDLE SCHOOL" | data$Schoolname=="SYLVAN HILLS MIDDLE SCHOOL" | 
                        data$Schoolname=="BROWN MIDDLE SCHOOL" | data$Schoolname=="YOUNG MIDDLE SCHOOL" | 
                        data$Schoolname=="LONG MIDDLE SCHOOL", "PRICE + SYLVAN + BROWN + YOUNG + LONG", data$Schoolname)

data <- subset(data, parcel_district == 17)
mean(data$salesprice)

model1 <- lm(I(salesprice/100000) ~ log(calcacres) + age + I((age*age)/1000) + totbath + I(sqft4/1000) + tract_cover + total_crime_house +
               MATHMeanScaleScore15 + pct_black + pct_above_f + pct_below_f + I(lon-plon) + I(lat-plat) +  
               I(((lon-plon)^2)/1000) + I(((lat-plat)^2)/1000), data=data)

summary(model1)

deviance(model1)
pred <- predict(model1)
extractAIC(model1)

nrow(data)*(log(2*pi)+1+log((sum(model1$residuals^2)/nrow(data))))+((length(model1$coefficients)+1)*2)

sqrt(mean((data$salesprice - (pred*100000))^2))
sqrt(mean(model1$residuals^2))*100000
nrow(data)









