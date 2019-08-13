# load the tidyverse library to use for data manipulation and graphics
library(tidyverse)

# set the them for ggplot graphics
theme_set(theme_bw() + theme(axis.title.y = element_text(angle = 0)))

# load 2009 customer level profit data
DATA9 <- read_csv('data_2009.csv')

# Question 1: How much variation in profitability is observed across customers?

sum_profit9 <- sum(select(DATA9, profit9))

count_customer <- nrow(DATA9)

DATA9 <- arrange(DATA9, desc(profit9))

DATA9 <- mutate(DATA9, cum_profit9 = cumsum(profit9))

DATA9 <- mutate(DATA9, per_profit9 = cum_profit9 / sum_profit9 * 100)

DATA9 <- mutate(DATA9, rnk_customer = row_number())

DATA9 <- mutate(DATA9, per_customer = rnk_customer / count_customer * 100)

head(filter(DATA9, per_customer > 20), n = 1)

ggplot(DATA9) +
  geom_line(aes(x=per_customer, y=per_profit9)) +
  labs(x = "Customer Percentile", y = "Cumulative\nProfit\nPercentile")

# Question 2: Based on the 1999 sample of customers, what can Pilgrim conclude about 
# he average profitability of Pilgrim Bank customers?

# Answer
# We can calculate sample mean.

mean(DATA9$profit9)
# Result: sample mean is $111.51

# The sample mean only summarizes the sample.
# If we want to be able to generalize,
# we need to be able to express our uncertainty.
# confidence intervals are one way of expressing the uncertainty about
# parameter estimates

result <- lm(profit9 ~ 1, DATA9)
coefficients(result)
# Result: sample mean = $111.51

confint(result, level = 0.95)
# Result: 95 percent confidence interval is ($108.50, $114.51)

# Alternative method:
t.test(DATA9$profit9, conf.level = 0.95)
# Result: 95 percent confidence interval is ($108.50, $114.51)



#### QUESTION 3 ####
# Are online and offline customer different
# in term of their profitability?

# Is the difference in average profitability 
# between online and offline customers in the sample indicative of
# a meaningful difference in profitability across these groups 
# in the entire population of Pilgrim Bank’s customers?

# Answer:

# First, visualize the distribution of 
p <- ggplot(DATA9, aes(factor(online9), profit9))
p

p + geom_boxplot()

# zoom on the important area and remove outliers
p + geom_boxplot(outlier.shape = NA) + ylim(-250, 650)

p + geom_violin() + ylim(-250, 650)

# Second, calculate summary statistics for the two group:
summarize(group_by(DATA9, online9), mean_profit9 = mean(profit9))

# Finally, test if there is any difference
# between the groups for the whole population
result <- lm(profit9 ~ factor(online9), DATA9)
coefficients(summary(result))

#### QUESTION 4 ####
# What is the role of customer demographics
# in comparing online and offline profitability?

# we need customer profiles to incorporate depographic info
PROFILE <- read_csv("profiles.csv")

# The profile data has missing values
# Count missing values for all of the columns
colSums(is.na(PROFILE))

# handling missing values
# Method 1: Removing records with missing value
PROFILE_CLEAN <- filter(PROFILE, !is.na(age) | !is.na(income))

# Method 2: Replace with mean (or median)
age_rpl <- mean(PROFILE$age, na.rm = TRUE)
income_rpl <- mean(PROFILE$income, na.rm = TRUE)
PROFILE_CLEAN <- replace_na(PROFILE, list(age = age_rpl, income = income_rpl))

# Method 3: Add an additional variable recording the missing values
#           and replace missing values with 0
PROFILE_CLEAN <- 
  mutate(PROFILE, missing_age = is.na(age), missing_income = is.na(income)) %>%
  replace_na(list(age = 0, income = 0))

# join 2009 profit data with cutomer profiles
DATA9 <- full_join(DATA9, PROFILE_CLEAN, by = "id")

# Now that the data is ready we go back to 
# our original question
# What is the role of customer demographics
# in comparing online and offline profitability?
result <- lm(
  profit9 ~ factor(online9) + age,
  data = DATA9)

coefficients(summary(result))
# Result the effect of online9 is statistically significant

# ???? Create a model with more variables



#### QUESTION 5 ####

# ???? Write a piece of code that loads 2000 profit data
# ???? and join it with 1999 data.


#### QUESTION 7 6 ####
# ???? How many customers have we lost during the year 2000?

# ???? What is the retention rate?

