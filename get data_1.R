
rm(list=ls())

library(dplyr)
library(ggplot2)
library(lmtest)
library(multiwayvcov)
library(tmap)

#load data

setwd("C:/Users/syedm/Desktop/Atlanta")

data <- read.csv("atlanta single family 6116_new.csv")

data <- subset(data, taxpaid>0)
summary(data$taxpaid)
data$sqft4 <- as.numeric(data$sqft3)
data <- subset(data, sqft4>0)

summary(data$sqft4)

#get additional data

#get over 65 population

library(tigris)
library(tidycensus)

#https://api.census.gov/data/key_signup.html
#census_api_key("590f195bf74ded3640d4b9bfe8e912ac7757ca44")

v17 <- load_variables(2010, "sf1", cache = TRUE) #use sf2 if doesn't work


options(tigris_use_cache = TRUE)

#this is total population

owner_occupied <- get_decennial(
  geography = "block group",
  state = "GA",
  county = "Fulton",
  variables = c(
    over_65 = "P022001"), #P022001 #H017002  
  year = 2010)

#this is total population between 65-74

owner_65_74 <- get_decennial(
  geography = "block group",
  state = "GA",
  county = "Fulton",
  variables = c(
    over_65 = "P022009"), #P022009 #H017009
  year = 2010)

#this is total population between 75-84

owner_75_84 <- get_decennial(
  geography = "block group",
  state = "GA",
  county = "Fulton",
  variables = c(
    over_65 = "P022010"), #P022010 #H017010
  year = 2010)

#this is total population over 85

owner_over85 <- get_decennial(
  geography = "block group",
  state = "GA",
  county = "Fulton",
  variables = c(
    over_65 = "P022011"), #P022011 #H017011
  year = 2010)

#getting population proportion above 65

new <- owner_occupied
new$owner_65_74 <- owner_65_74$value
new$owner_75_84 <- owner_75_84$value
new$owner_over85 <- owner_over85$value

new$over65 <- new$owner_65_74+new$owner_75_84+new$owner_over85
new$over65_perc <- new$over65/new$value


library(tidyr)
data %>%
  separate(BlockGroup, c("blkgroup1", "GEOID"), "US") -> data


merge <- merge(data, new, by="GEOID")


ggplot(merge, aes(x=over65_perc)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=0.02,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") + 
  geom_vline(aes(xintercept=mean(over65_perc, na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1) + 
  xlab("Proportion of householders over 65 years") +
  ylab("Density")

plot(merge$over65_perc)

plot(merge$median_income, merge$over65_perc)

quantile(merge$over65_perc, probs = seq(.1, .9, by = .1), na.rm=TRUE)

merge %>% mutate(quartile = ntile(pct_black, 10)) %>% 
  group_by(quartile)%>% 
  summarise(mean_over65=mean(over65_perc, na.rm=TRUE),
            .groups = 'drop')

data <- merge

setwd("C:/Users/syedm/Desktop/atlanta_submarket")

data2 <- read.csv("atlanta data.csv")
data2 <- dplyr::select(data2, c("parcelid", "tract_cover", "total_crime_house"))

merge <- merge(data, data2, by = 'parcelid') 

#write.csv(merge, file="atlanta single family 6116_new.csv")







