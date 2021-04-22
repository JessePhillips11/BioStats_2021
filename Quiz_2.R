#BioStats 2021
#Quiz 2
#22/04/2021
#Jesse Phillips

#Packages
library(tidyverse)
library(ggplot2)
library(ggpubr)

# Question 1 --------------------------------------------------------------

Orange <- datasets::Orange
ToothGrowth <- datasets::ToothGrowth
warpbreaks <- datasets::warpbreaks


# Orange data
ggplot(Orange, aes(x = Tree, y = circumference)) +
  geom_boxplot()

  # hypothesis -> H0: all trees have the same circumference (T1 = T2 = T3 = T4 = T5)
  #               H1: all tress do not have the same circumference 

  # We'll use an ANOVA (comparing 5 different groups) 

  # Assumptions -> is the data normally distributed?
ggplot(Orange, aes(x = circumference)) +
    geom_histogram(position = "dodge")

shapiro.test(Orange$circumference)
    # the circumferences are normally distributed (p = 0.08)

  # Assumptions -> Homoscedasticity 
Orange %>% 
  group_by(Tree) %>% 
  summarise(var = var(circumference))
    # the largest value is < 4x greater than smallest, so the variance is homogenous enough

  # Assumptions -> the dependent variable (circumference) must be continuous
str(Orange$circumference) # yes it is

  # Assumptions -> observations in groups being compared are independent
    # we shall assume so, they are separate tress

  # All assumptions met. Now we can run our ANOVA
orange_aov <- aov(circumference ~ Tree, data = Orange)
summary(orange_aov)
  # We cannot reject H0 (p = 0.49), therefore the trees in this experiment do NOT 
  # differ significantly in circumference. 


# ToothGrowth data
ggplot(ToothGrowth, aes(x = supp, y = len)) +
  geom_boxplot() +
  coord_flip()

  # hypothesis -> H0: the length of VC is not lesser than the length of OJ
  #               H1: the length of VC is lesser than the length of OJ (VC < OJ)

  # We'll use a one-sided two-sample t-test

  # Assumptions -> is the data normally distributed?
ggplot(ToothGrowth, aes(x = len)) +
  geom_histogram(position = "dodge")

shapiro.test(ToothGrowth$len)
    # the lengths are normally distributed (p = 0.11)

  # Assumptions -> Homoscedasticity 
ToothGrowth %>% 
  group_by(supp) %>% 
  summarise(var = var(len))
    # the variances are homoscedastic 

  # Assumptions -> the dependent variable (length) must be continuous
str(ToothGrowth$len) # yes

  # Assumptions -> observations in groups being compared are independent
    # once again assumed

  # now we run the t-test
compare_means(len ~ supp, data = ToothGrowth, method = "t.test", var.equal = T, alternative = "less")
  # We reject H0 (p = 0.03), and conclude that the true length under supp VC
  # is less than the length under supp OJ 


# warpbreaks data
ggplot(warpbreaks, aes(x = wool, y = breaks)) +
  geom_boxplot()

  # hypothesis -> H0: wool A has the same amount of breaks as wool B
  #               H1: wool A does not have the same amount of breaks as wool B

  # We'll use a two-sided two-sample t-test

  # Assumptions -> is the data normally distributed?
ggplot(warpbreaks, aes(x = breaks)) +
  geom_histogram(position = "dodge")

shapiro.test(warpbreaks$breaks)
  # the breaks are NOT normally distributed (p < 0.05)

  # We will have to use a Wilcox Rank Sum test instead
compare_means(breaks ~ wool, data = warpbreaks, method = "wilcox.test")
  # We cannot reject H0 (p = 0.25). Wool A produces the same number of breaks 
  # as wool B.


# Question 2 --------------------------------------------------------------

SACTN <- SACTN_daily_v4.2 #I automatically loaded data in from Files tab
SACTN_2 <- SACTN %>% 
  separate(col = index, into = c("site", "source"), sep = "/") %>% 
  separate(col = date , into = c("yr", "mo", "day"), sep = "-")

ggplot(SACTN_2, aes(x = mo, y = temp, group = site)) +
  geom_boxplot() + 
  facet_wrap(vars(source))
  
SACTN_aov <- aov(temp ~ mo, data = SACTN_2)
summary(SACTN_aov)

SACTN_Tk <- TukeyHSD(SACTN_aov)
plot(SACTN_Tk) 
  # monthly differences exist in SST (in general)

  # we can eyeball the ggplot created earlier and say that monthly difference do 
  # exist between each of the sites and source.