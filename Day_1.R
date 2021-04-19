#R BioStats 2021 
#Day 1 - Types of Data
#19/04/2021
#Jesse Phillips

library(tidyverse)
library(e1071) #package containing 'kurtosis()'

chicks <- as_tibble(ChickWeight)

#Lets scrutinize the data
head(chicks)
tail(chicks, 2)
colnames(chicks)
summary(chicks)
str(chicks)
class(chicks$weight)

#What is our total sample size? 
nrow(chicks)
unique(chicks$Chick) 
#Note distinction between 'nrow()' and 'true' sample size 
#Sample size = 50

#Calculate mean/sd/median/kurtosis of weight of chicks at day 20 grouped by Diets
chicks %>% 
  group_by(Diet) %>% 
  filter(Time == 20) %>% 
  summarise(mean_weight = mean(weight),
            sd_weight = sd(weight),
            med_weight = median(weight),
            kt_weight = kurtosis(weight),
            min_wt = min(weight),
            qrt1_wt = quantile(weight, p = 0.25),
            qrt2_wt = quantile(weight, p = 0.75),
            max_wt = max(weight))

#creating fictitious data to illustrate Missing Values
dat1 <- c(NA, 12, 76, 34, 23)
mean(dat1)
mean(dat1, na.rm = TRUE) #tells R to remove the NA from mean calculation 
