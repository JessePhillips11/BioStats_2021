#BioStats 2021
#Day 2 Exercise/Example
#20/04/2021
#Jesse Phillips 

#Packages
library(dplyr)
library(tidyr)
library(ggpubr)
library(Rmisc)

#Read in data
snakes <- read_csv("data/snakes.csv")
snakes$day = as.factor(snakes$day) #converts day to factor, as ANOVA work with factor independent variable

#create summaries of data
snakes.summary <- snakes %>% 
  group_by(day) %>%  #group the data by day only
  summarise(mean_openings = mean(openings),
            sd_openings = sd(openings)) %>%  #get mean and sd for openings each day
  ungroup()
snakes.summary

#Create visual summaries
snakes.summary2 <- summarySE(data = snakes, measurevar = "openings", groupvars = c("day"))  # 'summarySE()' summarises data - gives count, mean, sd, se and conf. int.
ggplot(data = snakes, aes(x = day, y = openings)) +
  geom_segment(data = snakes.summary2, aes(x = day, xend = day, y = openings - ci, yend = openings + ci, colour = day),
               size = 2.0, linetype = "solid", show.legend = F) +
  geom_boxplot(aes(fill = day), alpha = 0.6, show.legend = F) + 
  geom_jitter(width = 0.05)

#Our 2 null hypotheses:
  # H0: There is no difference between snakes with respect to the number of openings at which they habituate.
  # H0: There is no difference between days in terms of the number of openings at which the snakes habituate.

#Fit ANOVA model to test these hypotheses
snakes.aov <- aov(openings ~ day + snake, data = snakes) #Trying to explain variance in openings with day and snake
summary(snakes.aov)
  # there is a significant difference between DAYS (p = 0.049), but not SNAKES (p = 0338).
  # the DAY is causing variation in openings

#Test the assumptions (i.e. errors are normally distributed and heteroscedastic)
par(mfrow = c(2, 2))
# Checking assumptions...
# make a histogram of the residuals;
# they must be normal
snakes.res <- residuals(snakes.aov) 
hist(snakes.res, col = "red")

# make a plot of residuals and the fitted values;
# # they must be normal and homoscedastic
plot(fitted(snakes.aov), residuals(snakes.aov), col = "red")

snakes.tukey <- TukeyHSD(snakes.aov, which = "day", conf.level = 0.90)
plot(snakes.tukey, las = 1, col = "red")


# EXTRAS - Lets try and visualize snakes explaining variance in openings
ggplot(snakes, aes(x = snake, y = openings, fill = snake)) + 
  geom_boxplot() + 
  #geom_point(aes(x = snake, y = openings, group = day)) +   #this shows the individual days (probably won't add)
  theme_bw() + 
  labs(y = "Openings", title = "Number of openings done by each snake over the\n four days", x = NULL, fill = "Snake") +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(), panel.grid = element_blank()) +
  coord_flip()
  
ggplot(snakes, aes(x = day, y = openings, fill = snake)) + 
  geom_col(position = "dodge", col = "black") +
  scale_fill_brewer(palette = "YlGn") +
  theme_bw() +
  labs(title = "SNAKES!", x = "Days", y = "Openings", fill = "Snake") +
  theme(plot.title = element_text(size = 20, face = "bold"))
