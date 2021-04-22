#BioStats 2021
#Day 4
#22/04/2021
#Jesse Phillips

#Packages
library(ggplot2)
library(tidyr)
library(rcompanion)

# Confidence Intervals ----------------------------------------------------

# Calculatin confidence: Bootstrapping
Input <- ("
Student  Sex     Teacher  Steps  Rating
a        female  Jacob    8000   7
b        female  Jacob    9000  10
c        female  Jacob   10000   9
d        female  Jacob    7000   5
e        female  Jacob    6000   4
f        female  Jacob    8000   8
g        male    Jacob    7000   6
h        male    Jacob    5000   5
i        male    Jacob    9000  10
j        male    Jacob    7000   8
k        female  Sadam    8000   7
l        female  Sadam    9000   8
m        female  Sadam    9000   8
n        female  Sadam    8000   9
o        male    Sadam    6000   5
p        male    Sadam    8000   9
q        male    Sadam    7000   6
r        female  Donald   10000  10
s        female  Donald    9000  10
t        female  Donald    8000   8
u        female  Donald    8000   7
v        female  Donald    6000   7
w        male    Donald    6000   8
x        male    Donald    8000  10
y        male    Donald    7000   7
z        male    Donald    7000   7
")

data <- read.table(textConnection(Input),header = TRUE) #Reads text data into df format
summary(data)

# ungrouped data is indicated with a 1 on the right side of the formula, or the group = NULL argument.
groupwiseMean(Steps ~ 1,data = data, conf = 0.95, digits = 3)

# one-way data
grouped_mean_data <- groupwiseMean(Steps ~ Sex, data = data, conf = 0.95,digits = 3)
grouped_mean_data
  
  # let's plot this data
ggplot(grouped_mean_data, aes(x = Sex, y = Mean)) +
  geom_col(col = "black", fill = "salmon", width = 0.6) +
  geom_errorbar(aes(x = Sex, ymax = Trad.upper, ymin = Trad.lower), width = 0.25) +
  theme_minimal() +
  labs(x = "", y = "Steps", title = "Mean number of steps taken by female and \nmale students")


# two-way data
g_m_dat_2 <- groupwiseMean(Steps ~ Teacher + Sex, data = data, conf = 0.95,digits = 3)
g_m_dat_2
  
  # let's plot this data as well
ggplot(g_m_dat_2, aes(x = Sex, y = Mean, group = Teacher, fill = Teacher)) +
  geom_col(position = "dodge", col = "black", width = 0.8) +
  geom_errorbar(position = position_dodge(0.8), aes(x = Sex, ymax = Trad.upper, ymin = Trad.lower), width = 0.25) +
  theme_minimal() +
  labs(x = "", y = "Steps", title = "Mean number of steps taken by female and \nmale students") +
  scale_fill_brewer(palette = "Set1")


# by bootstrapping (randomly generating data based on data generated in experiment)
groupwiseMean(Steps ~ Sex,
              data = data,
              conf = 0.95,
              digits = 3,
              R = 10000,  # number of bootstrap replicates to use for bootstrapped statistics
              boot = TRUE, # if TRUE, includes mean of the bootstrapped means
              traditional = FALSE,
              normal = FALSE,
              basic = FALSE,
              percentile = FALSE,
              bca = TRUE)

groupwiseMean(Steps ~ Teacher + Sex,
              data = data,
              conf = 0.95,
              digits = 3,
              R = 10000,
              boot = TRUE,
              traditional = FALSE,
              normal = FALSE,
              basic = FALSE,
              percentile = FALSE,
              bca = TRUE)

# ANOVA 
anova <- aov(Steps ~ Sex * Teacher, data = data) #shows interaction
summary(anova)

anova_Tukey <- TukeyHSD(anova)
plot(anova_Tukey)
