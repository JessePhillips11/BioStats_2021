#BioStats Assignment 1 
#19/04/2021
#Jesse Phillips


# Section 1 ---------------------------------------------------------------
View(BOD)
#C. BOD is tidy: each row is an observation with two values


# Section 2 ---------------------------------------------------------------
library(dplyr)
library(dslabs)
data(murders)

#exploring the dataset
glimpse(murders)
head(murders)
tail(murders)

#The murder dataset has 5 variables. 'state' refers to a specific US state
# while 'abb' is the abbreviation of that state. 'region' is the more 
# general geographical US region. 'population' is the total number of 
# people in that state, and 'total' is the number of gun murders in that 
# state. The dataset has observations for each of the 50 states plus 
# the District of Columbia.

#Select only states and population size
murders %>% 
  select(c(state, population))

#Remove Florida from the dataset
murders %>% 
  filter(state != "Florida")

#Create 'no_south' dataset 
no_south <- murders %>% 
  filter(region != "South")
nrow(no_south) #The no_south dataset has 34 states

#Calculate population size of the South and West regionally
murders %>% 
  filter(region %in% c("South", "West")) %>% 
  arrange(region) %>% 
  summarise(pop_size_South = sum(population[c(1:17)]),
            pop_size_West = sum(population[c(18:30)]))
#South = 115674434 and West = 71945553

#Create new dataframe with only population size of Northeast region
NE_pop_size <- murders %>% 
  filter(region == "Northeast") %>% 
  summarise(population = sum(population)) #55317240

#Create two plots and explain visible trends
#Population per Region
ggplot(murders, aes(region, population)) +
  geom_boxplot() +
  labs(x = "Region", y = "Population (millions)", title = "Total population of each geographical US region") +
  scale_y_continuous(breaks = c(0, 10000000, 20000000, 30000000), labels = c(0, 10, 20 , 30)) +
  theme_bw() + 
  theme(panel.grid = element_blank())
#Total murders per Region
ggplot(murders, aes(abb, total, colour = region)) +
  geom_point() +
  geom_text(aes(label = ifelse(total > 300, abb, '')), hjust = -0.2, vjust = 0, size = 3.5) +
  labs(y = "Total Murders", title = "Number of murders in each US state,\n grouped by their region", colour = "Region") +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
#TRENDS: The population per region plot shows that the Northeast has the highest total 
      #  population, while North Central has the highest average population. However;
      #  when it comes to individual states, there are two in the South, and one in the 
      #  West that has very high individual populations. When we compare this to the 
      #  number of murders per region, the South and North Central generally have high
      #  cases, while individually, NY and PA from the Northeast shows high murder 
      #  numbers and - glaringly - CA from the West has the highest number of murders. 

#Compare population size of South and West 
murders %>% 
  filter(region %in% c("South", "West")) %>% 
  arrange(region) %>% 
  summarise(pop_S = sum(population[c(1:17)]),
            pop_W = sum(population[c(18:30)]))
#The South region has 17 states with a population of 115,674,434 and the West region
 #has 13 states with a population of 71,945,553

#Create new dataframe where total > 20 but < 100 
murd_20_100 <- murders %>% 
  filter(total > 20 & total < 100)

#Create object containing rows 10-24 and row 26
murd_10_26 <- murders %>% 
  slice(10:24, 26)

#Convert murders table into a tibble
murders_tibble <- as_tibble(murders)

#Use 'group_by()' to convert murders into a tibble grouped by region
murders %>% 
  as_tibble() %>% 
  group_by(region)


# Section 3 ---------------------------------------------------------------
library(dplyr)
library(dslabs)
data(heights)

#The heights dataset is comprised of two variables; "sex", a categorical 
 #variable which is either male or female, and "height", which is height in 
 #inches. There are 1050 observations.

#Explore the dataset
str(heights)
head(heights)
summary(heights)

#Determine average and sd for males and females. 
#Then calculate median, minimum and maximum values. 
heights %>% 
  group_by(sex) %>% 
  summarise(average = mean(height),
            sd = sd(height),
            median = median(height),
            min = min(height),
            max = max(height))


# Section 4 ---------------------------------------------------------------
x <- c(1, 6, 21, 19, NA, 73, NA)
y <- c(NA, NA, 3, NA, 13, 24, NA)

#Count number of missing elements in x and y
sum(is.na(x)) #2
sum(is.na(y)) #4 
sum(is.na(x)) + sum(is.na(y)) #6

#Transform above code into a function
count_na <- function(vector) {
  for (i in seq_along(vector)){
    total = sum(is.na(vector))
  }
  return(total)
}

#Create 3 new vectors and test function
a <- c(5, 8 , NA, NA, 23, 56, 90)
b <- c(45, NA, 12, NA, NA, NA, 103)
c <- c(23, 1, 76, 34, NA, 4, 11)

count_na(c(a,b)) #6 correct
count_na(c(b,c)) #5 correct
count_na(c(a,b,c)) #7 CORRECT 


# Section 5 ---------------------------------------------------------------

Seasonal_data <- data.frame(year = c(2015, 2016, 2017, 2018),
                            Winter = c(41, 39, 47, 40),
                            Spring = c(41, 46, 57, 45),
                            Summer = c(75, 52, 85, 66),
                            Autumn = c(57, 66, 52, 56))

#Design a hypothesis, then create two plots and write a paragraph discussing findings

#Tidy the data first
seasonal_dat_2 <- Seasonal_data %>% 
  gather(Winter, Spring, Summer, Autumn, key = "Season", value = "value")

#Hypothesis
  # Summer will have higher values than the rest

#Plots
ggplot(seasonal_dat_2, aes(x = Season, y = value)) +
  geom_boxplot() + 
  theme_bw() +
  labs(y = "Measured value", title = "Seasonal measured values over 4 years") +
  theme(panel.grid = element_blank())

ggplot(seasonal_dat_2, aes(x = Season, y = value, group = year, colour = factor(year))) + 
  geom_line() +
  geom_point() + 
  theme_bw() +
  labs(y = "Measured value", title = "Seasonal measured values over 4 years", colour = "Year")
  
#Discuss findings
  # Summer has the highest average value and the highest value overall in the dataset
  # This highest value peaks in the year 2017, this year also contains the highest
  # values fro Spring and Winter. 

cats_data<- tibble(cats = c("A", "B", "C"),
                   position = c("1-2-3", "3-1-2", "2-3-1"),
                   minutes = c(3, 3, 3),
                   seconds = c(12, 44, 15))
cats_data

#Split 'position' into 3 new columns: 'first_place', 'second_place', 'third_place'
cats_data_2 <- cats_data %>% 
  separate(col = position, into = c("first_place", "second_place", "third_place"), sep = "-") 

#Unite minutes and seconds into 'total_time'
cats_data_3 <- cats_data_2 %>% 
  unite(minutes, seconds, col = "total_time", sep = ":")


# Section 6 ---------------------------------------------------------------

fish <- as_tibble(fish_encounters)
#The fish_encounters dataset contains instances of tagged fish being
  #encountered at various autonomous monitors down a river. If the 
  #fish is recorded to have swimmed past, a "1" is placed in the 
  #'seen' column 
  
#We can spread the dataset, so that each location gets its own column,
  #this makes a (sort of) matrix, so we can see where each fish has been
fish_spread <- fish %>% 
  spread(key = station, value = seen)

#This, however, makes the data difficult to plot, so we can gather all 
  #those columns back together, into the 2 original columns
fish_gather <- fish_spread %>% 
  gather(-fish, key = "station", value = "seen", na.rm = T) #we have to remove the NA's 

#We can unite two columns into one...
fish_unite <- fish %>% 
  unite(fish, station, col = "Fish_unique", sep = "/")
#...then separate information contained in 1 column into 2
fish_separate <- fish_unite %>% 
  separate(col = Fish_unique, into = c("fish", "station"), sep = "/")

#We can also sort by a specific variable, by arranging a certain column
fish_arrange <- fish %>% 
  arrange(station)

#We can extract certain columns as a new dataframe...
fish_select <- fish %>% 
  select(fish, seen)
#...and compute new columns to the dataframe
fish_mutate <- fish_select %>% 
  mutate(DoubleCheckSeen = seen+1)
#lets do another (to help with the next function: join())
fish_mutate_2 <- fish_unite %>% 
  mutate(DoubleCheckSeen = seen+1)

#We can join one table to columns from another
fish_join <- left_join(fish_unite, fish_mutate_2)

#We can also return a copy of the table which is grouped by a certain variable
fish_grouped <- fish %>% 
  group_by(station) 
 