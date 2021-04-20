#BioStats 2021
#Day 2 
#20/04/2021
#Jesse Phillips

library(tidyverse)
library(plotly)

# Random normal data
set.seed(666)
r_dat <- data.frame(dat = c(rnorm(n = 1000, mean = 10, sd = 3),
                            rnorm(n = 1000, mean = 8, sd = 2)),
                    sample = c(rep("A", 1000), rep("B", 1000)))

# Create histogram
h <- ggplot(data = r_dat, aes(x = dat, fill = sample)) +
  geom_histogram(position = "dodge", binwidth = 1, alpha = 0.8) +
  geom_density(aes(y = 1*..count.., fill = sample), colour = NA, alpha = 0.4) +
  labs(x = "value")
h

#The Shapiro-Wilk test tests the Normality of the data
shapiro.test(r_dat$dat)
  #This is wrong, because you've used the ENTIRE dataset
    #not just sample A or B
r_dat %>% 
  group_by(sample) %>% #groups the dataset by it's samples
  summarise(norm_dat = as.numeric(shapiro.test(dat)[2])) #We use square bracket notation to select only the p-value
  #Both groups are normally distributed (p > 0.05)  

#To check for homoscedasticity, make sure the variances of the samples are not 2-4 times
  #greater than one another
r_dat %>% 
  group_by(sample) %>% 
  summarise(sample_var = var(dat))
#Variances aren't too far apart, the samples are homoscedasticity 

#Create a function which does both tests of assumptions in one
two_assum <- function(x) {
  x_var <- var(x)
  x_norm <- as.numeric(shapiro.test(x)[2])
  result <- c(x_var, x_norm)
  return(result)
}

#Now lets use this new function in a tidy way
r_dat %>% 
  group_by(sample) %>% 
  summarise(sample_var = two_assum(dat)[1],
            sample_norm = two_assum(dat)[2])

library(ggpubr) #contains the 'compare_means()' function 


# One-sample t-tests ------------------------------------------------------

# create a single sample of random normal data
set.seed(666)
r_one <- data.frame(dat = rnorm(n = 20, mean = 20, sd = 5),
                    sample = "A")

# check normality
shapiro.test(r_one$dat)

# No variance to compare

# compare random data against a population mean of 20
t.test(r_one$dat, mu = 20)
  #Sample mean is not different from mu

# compare random data against a population mean of 30
t.test(r_one$dat, mu = 30)
  #Sample mean IS significantly different from mu (t = -6.75, p < 0.05)

#Lets visualise the data
ggplot(data = r_one, aes(y = dat, x = sample)) +
  geom_boxplot(fill = "lightsalmon") +
  # population  mean (mu) = 20
  geom_hline(yintercept = 20, colour = "blue", 
             size = 1, linetype = "dashed") +
  # population  mean (mu) = 30
  geom_hline(yintercept = 30, colour = "red", 
             size = 1, linetype = "dashed") +
  labs(y = "Value", x = NULL) +
  coord_flip()


# Two-sample t-tests ------------------------------------------------------

# random normal data
set.seed(666)
r_two <- data.frame(dat = c(rnorm(n = 20, mean = 4, sd = 1),
                            rnorm(n = 20, mean = 5, sd = 1)),
                    sample = c(rep("A", 20), rep("B", 20)))

# perform t-test
# note how we set the `var.equal` argument to TRUE because we know 
# our data has the same SD (they are simulated as such!)
t.test(dat ~ sample, data = r_two, var.equal = TRUE) #How does 'sample' change given 'dat'?

# if the variances are not equal, simply set `var.equal` to false
# and a Welch's t-test will be performed

# perform t-test using `compare_means()`
# note that compare_means() takes the same arguments as t.test()
compare_means(dat ~ sample, data = r_two, method = "t.test", var.equal = TRUE)
  #different methods can be used in the 'compare_means()' function


# A t-test workflow -------------------------------------------------------

#Loading data
ecklonia <- read_csv("data/ecklonia.csv") %>% 
  gather(key = "variable", value = "value", -species, -site, -ID)

#Visualising data
ggplot(data = ecklonia, aes(x = variable, y = value, fill = site)) +
  geom_boxplot() +
  coord_flip()

#Formulating a hypothesis
# filter the data
ecklonia_sub <- ecklonia %>% 
  filter(variable == "stipe_mass")

# then create a new figure
ggplot(data = ecklonia_sub, aes(x = variable, y = value, fill = site)) +
  geom_boxplot() +
  coord_flip() +
  labs(y = "stipe mass (kg)", x = "") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

#Hypothesis: H0: Stipe mass at Batsata Rock IS NOT greater than Stipe mass at Boulders Beach
            #H1: Stipe mass at Batsata Rock IS greater than Stipe mass at Boulders Beach

#Choosing a test
  #t-test, comparing 2 samples, checking for '!>' (One-sided Two-sample t-test)

#Checking assumptions
ecklonia_sub %>% 
  group_by(site) %>% 
  summarise(stipe_mass_var = two_assum(value)[1],
            stipe_mass_norm = two_assum(value)[2])
#Assumptions met

#Running an analysis
t.test(value ~ site, data = ecklonia_sub, var.equal = TRUE, alternative = "greater")
  # t = 1.87, df = 24, p = 0.04

#Interpreting the results
# We can reject the null hypothesis (p < 0.05)
# H1: the true difference in means is greater than 0
# Stipe mass at Batsata Rock IS greater than stipe mass at Boulders Beach

#Conclusion
  #The stipe mass (kg) of the kelp Ecklonia maxima 
  #was found to be significantly greater at 
  #Batsata Rock than at Boulders Beach (p = 0.04, t = 1.87, df = 24).


# ANOVA -------------------------------------------------------------------

# First grab the data
chicks <- as_tibble(ChickWeight)

# H0: mu1 = mu2 = mu3 = mu4 
# where 'mu' is the means of the 4 diets

#Single Factor ANOVA 
chicks.aov1 <- aov(weight ~ Diet, data = filter(chicks, Time == 21)) #is there is difference in Diet (measured by weight) for the chicks at Time 21
summary(chicks.aov1)
  # F = 4.66, p = 0.01 
  # H0 is rejected. At least 1 Diet is significantly different  

#Tukey HSD test
TukeyHSD(chicks.aov1)
  # Diets 1 and 3 are significantly different (i.e mu1 != mu3)

#Multiple Factors
  # when we have multiple grouping variables (Time and Diet seem to have an effect)
summary(aov(weight ~ Diet + as.factor(Time), data = filter(chicks, Time %in% c(0, 21))))
  # we can also look at the interaction between Time and Diet
summary(aov(weight ~ Diet * as.factor(Time), data = filter(chicks, Time %in% c(4, 21))))

#we can also run a Tukey HSD test on mulitple factor ANOVAs
TukeyHSD(aov(weight ~ Diet * as.factor(Time), data = filter(chicks, Time %in% c(20, 21))))


#Alternatives to ANOVA 
# Then check for failing assumptions
chicks %>% 
  filter(Time == 0) %>% 
  group_by(Diet) %>% 
  summarise(norm_wt = as.numeric(shapiro.test(weight)[2]),
            var_wt = var(weight))

#Wilcox rank sum test (non-parametric t-test)
compare_means(weight ~ Diet, data = filter(chicks, Time == 0, Diet %in% c(1, 2)), method = "wilcox.test")

#Kruskall-Wallis rank sum test (non-parametric ANOVA)
compare_means(weight ~ Diet, data = filter(chicks, Time == 0), method = "kruskal.test") #single factor
