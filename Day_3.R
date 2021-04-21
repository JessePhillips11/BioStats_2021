#BioStats 2021
#Day 3
#21/04/2021
#Jesse Phillips

library(tidyverse)
library(ggpubr)
library(corrplot) #for visualization of a correlation matrix
library(reshape2) #for melting correlation matrix into heat map

# Simple Linear Regressions -----------------------------------------------

glimpse(faithful)
head(faithful)

eruption.lm <- lm(eruptions ~ waiting, data = faithful) #lm of 'waiting' explaining 'eruptions'
summary(eruption.lm)
 # 'waiting' has a significant effect on 'eruptions' (t = 34.09, p < 0.05)

#A graph of the linear model
slope <- round(eruption.lm$coef[2], 3)
# p.val <- round(coefficients(summary(eruption.lm))[2, 4], 3) # it approx. 0, so...
p.val = 0.001
r2 <- round(summary(eruption.lm)$r.squared, 3)

ggplot(data = faithful, aes(x = waiting, y = eruptions)) +
  geom_point() +
  annotate("text", x = 45, y = 5, label = paste0("slope == ", slope, "~(min/min)"), parse = TRUE, hjust = 0) +
  annotate("text", x = 45, y = 4.75, label = paste0("italic(p) < ", p.val), parse = TRUE, hjust = 0) +
  annotate("text", x = 45, y = 4.5, label = paste0("italic(r)^2 == ", r2), parse = TRUE, hjust = 0) +
  stat_smooth(method = "lm", colour = "salmon") +
  labs(title = "Old Faithful eruption data",
       subtitle = "Linear regression",
       x = "Waiting time (minutes)",
       y = "Eruption duration (minutes)")

#Predicting from the linear model 

# use the accessor function to grab the coefficients:
erupt.coef <- coefficients(eruption.lm)
erupt.coef

# how long would an eruption last of we waited, say, 80 minutes?
waiting <- 80 

# the first and second coef. can be accessed using the 
# square bracket notation:
erupt.pred <- erupt.coef[1] + (erupt.coef[2] * waiting)
erupt.pred # the unit is minutes

# using the predict() function
pred.val <- data.frame(waiting = c(60, 80, 100))
predict(eruption.lm, pred.val) # returns waiting time in minutes


# Correlations ------------------------------------------------------------

# Load data
ecklonia <- read_csv("data/ecklonia.csv")

# Remove all categorical data, so each column represents pair-wise continuous/ordinal measurements
ecklonia_sub <- ecklonia %>% 
  select(-species, - site, - ID)

# PEARSON CORRELATION - 2 continuous variables
cor.test(x = ecklonia$stipe_length, ecklonia$frond_length,
         use = "everything", method = "pearson") #Note: last 2 arguments are the default settings

  # correlation matrix
  # because all our data is continuous, it is appropriate to use a Pearson test on every column
ecklonia_pearson <- cor(ecklonia_sub)
ecklonia_pearson

# SPEARMAN RANK CORRELATION - comparing continuous to ordinal variables
  # Create ordinal data
ecklonia$length <- as.numeric(cut((ecklonia$stipe_length+ecklonia$frond_length), breaks = 3))

  # Run test on any variable
cor.test(ecklonia$length, ecklonia$digits, method = "spearman")

# KENDALL RANK CORRELATION - will work for both continuous or ordinal data
  # Allow us to perform tests on non-normally distributed data
ecklonia_norm <- ecklonia_sub %>% 
  gather(key = "variable") %>% 
  group_by(variable) %>% 
  summarise(variable_norm = as.numeric(shapiro.test(value)[2]))
ecklonia_norm

cor.test(ecklonia$primary_blade_length, ecklonia$primary_blade_width, method = "kendall")


# One panel Visual
# Calculate Pearson r beforehand for plotting
r_print <- paste0("r = ", 
                  round(cor(x = ecklonia$stipe_length, ecklonia$frond_length),2))

# Then create a single panel showing one correlation
ggplot(data = ecklonia, aes(x = stipe_length, y = frond_length)) +
  geom_smooth(method = "lm", colour = "grey90", se = F) +
  geom_point(colour = "mediumorchid4") +
  geom_label(x = 300, y = 240, label = r_print) +
  labs(x = "Stipe length (cm)", y = "Frond length (cm)") +
  theme_pubclean()

# Multiple panel Visual
corrplot(ecklonia_pearson, method = "circle")

# Heat map
melt_ecklonia <- melt(ecklonia_pearson) #melt the correlation matrix

head(melt_ecklonia) #visualize the data

ggplot(data = melt_ecklonia, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() 

#This makes an ugly heat map, lets make it better  

#HELPER FUNCTIONS
# Get lower triangle of the correlation matrix
get_lower_tri<-function(ecklonia_pearson){
  ecklonia_pearson[upper.tri(ecklonia_pearson)] <- NA
  return(ecklonia_pearson)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(ecklonia_pearson){
  ecklonia_pearson[lower.tri(ecklonia_pearson)]<- NA
  return(ecklonia_pearson)
}

#Usage of the Helper Functions
upper_tri <- get_upper_tri(ecklonia_pearson)
upper_tri

# FINISHED CORRELATION MATRIX HEATMAP
# Melt the correlation matrix
melt_eck_2 <- melt(upper_tri, na.rm = TRUE)

#Heatmap
ggplot(data = melt_eck_2, aes(Var2, Var1, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "orangered4", mid = "white", 
                       midpoint = 0, limit = c(-0.1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1)) +
  coord_fixed()

# We can REORDER the heatmap
  
#HELPER FUNCTION
reorder_eck <- function(melt_eck_2){
  # Use correlation between variables as distance
  dd <- as.dist((1-melt_eck_2)/2)
  hc <- hclust(dd)
  melt_eck_2 <-melt_eck_2[hc$order, hc$order]
}

#Reordered correlation data visualization
eck_3 <- reorder_eck(ecklonia_pearson)
upper_tri <- get_upper_tri(eck_3)
# Melt the correlation matrix
melt_eck_3 <- melt(upper_tri, na.rm = TRUE)
# Create a ggheatmap
ggplot(melt_eck_3, aes(Var2, Var1, fill = value)) + 
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "orangered4", mid = "white", 
                       midpoint = 0, limit = c(-0.1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal() + # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1),
        axis.title = element_blank()) +
  labs(title = "Heat map of the correlations between \ncharacteristics of Ecklonia") +
  coord_fixed()



# DLPLY -------------------------------------------------------------------

dlply(ecklonia, .(digits))

dlply(ecklonia, .(site, ID))

View(dlply(ecklonia, .(species, ID, digits)))
