#BioStats 2021 
#Quiz 1
#20/04/2021
#Jesse Phillips


# Question 1 --------------------------------------------------------------

#List various data classes and give explanation and example for each
  # Numerical - Continuous : all numbers (2.5613, -0.989, 1.08e-06) 
  # Numerical - Discrete : whole numbers (-1, 0, 1, 2, 3)
  # Categorical : values that represent a set/class/category, 
      # but don't have inferred importance relative to one another (red, yellow, blue)
  # Ordinal : categorical values, but with inferred importance 
      # relative to one another (hot, lukewarm, cold)
  
#List some of the functions used to view your data in R
glimpse()
str()
head()
tail()
summary()
view()

#Discuss skewness and Kurtosis
  # The skewness of data refers to whether the data tend to be greater or 
  # lesser than the mean. Its Kurtosis refers to the shape of that skewness

# Question 2 --------------------------------------------------------------

orange <- datasets::Orange
?Orange
glimpse(orange)

#Explain what type of data class this orange data set belongs to
    # The 'Tree' variable contains Ordinal data. 
    # The 'age' variable contains Numerical data.
    # The 'circumference' variable contains Numerical data.

#Show the first 6 and last 6 rows, column names, and summary statistics
head(orange, 6)
tail(orange, 6)
names(orange)
summary(orange)

#Determine the Mean, Median and Standard Deviation of age and circumference 
orange %>% 
  group_by(Tree) %>% 
  summarise(mean_age = mean(age),
            med_age = median(age),
            sd_age = sd(age),
            mean_circ = mean(circumference),
            med_circ = median(circumference),
            sd_circ = sd(circumference))

#Determine skewness and Kurtosis
skewness(orange$age)
skewness(orange$circumference)
kurtosis(orange$age)
kurtosis(orange$circumference)

#Using summarise(), determine minimum, maximum, first and third quantiles for circumference
summarise(orange, min = min(orange$circumference),
          q1 = quantile(orange$circumference, 0.25),
          q3 = quantile(orange$circumference, 0.75),
          max = max(orange$circumference))

#Create 2 plots (with labels) and explain visible trends 
ggplot(orange, aes(x = Tree, y = circumference)) + 
  geom_boxplot() + 
  theme_bw() +
  labs(y = "Circumference (mm)", title = "Trunk circumference of orange trees after 1582 days")

ggplot(orange, aes(x = age, y = circumference)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  labs(x = "Age (days)", y = "Circumference (mm)", title = "Growth of orange tress shown by trunk circumference\nover time")

#TRENDS: The 5 Trees all have different circumferences at the same ages, with Tree 3 having
      #  the smallest circumference and Tree 4 having the largest. This shows that the trees 
      #  grew at varying rates, since they were all the same age. 
      #  We can also see that circumference increases with age, there is a positive correlation.
      #  The trees all start with the same circumference (roughly), then their growth rates
      #  diverge more and more as time goes on. 


# Question 3 --------------------------------------------------------------

mutate() #adds a column to the end of a data frame 
select() #grabs a specified column(s)
group_by() #groups data by the factors of a specified variable
filter() #grabs a specified row(s)
separate() #separates observations in a single column to multiple columns 