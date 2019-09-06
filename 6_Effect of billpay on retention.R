# What is the effect of customer use of electronic bill pay on retention?

# The retention rate is a categorical variable. Yes we retained them or no we didn't (1, or 0)

library(tidyverse)

# Load and prepare data

data1999 <- read_csv("data_2009.csv")
data2000 <- read_csv("data_2010.csv")
Profiles <- read_csv("profiles.csv")

# Merge datasets

DATA <- data1999 %>%
  full_join(data2000, by = "id") %>%
  full_join(Profiles, by = "id")

# Handle missing values

DATA <- DATA %>%
  mutate(missing_age = is.na(age) * 1,
         missing_income = is.na(income) * 1) %>%
  replace_na(list(age = 0, income = 0))

# Create variable to indicate whether customer is retained or not
# If we have profits available for a customer in 1999 and in 2000 that means we retained
# that customer. Let's create a variable that gives a 1 to customers with profits in both years
# aka retained customers and ZEROS to customers who were only available in 1999 or 2000 (churned, new)

DATA <- DATA %>%
  mutate(retained = !is.na(profit9) & !is.na(profit0)) * 1

# Question 4, Does the use of electronic bill pay tell us anything about the retention of that
# customer?

# Since our objective is to explain the effect of bill pay on retention, which is a
# "binary categorical variable" aka binary qualitative variable, we will not use a linear
# regression since those models are used for continuous variables that model expected values
# such as expected means etc, but rather we will use logistic regression, implemented using glm function
# to model it in R.

retention_model <- glm(retained ~ profit9 + online9 + billpay9 +
                   age + income + tenure9 + factor(district) + missing_age + missing_income,
                   binomial(link = "logit"), DATA, na.action = "na.exclude")

coefficients(summary(retention_model))
                          Estimate   Std. Error    z value      Pr(>|z|)
(Intercept)           1.178515e-01 5.502792e-02  2.1416670  3.222029e-02
profit9               7.397262e-05 7.131405e-05  1.0372799  2.996054e-01
online9               2.426964e-01 5.649733e-02  4.2957148  1.741313e-05
billpay9             -6.129445e-01 1.337190e-01 -4.5838253  4.565457e-06
age                   2.776751e-01 1.110097e-02 25.0135985 4.348731e-138
income                1.766661e-01 7.707760e-03 22.9205547 2.898629e-116
tenure9               2.981534e-02 2.372820e-03 12.5653611  3.273768e-36
factor(district)1200  4.718767e-02 5.380192e-02  0.8770630  3.804524e-01
factor(district)1300  2.667161e-02 6.671230e-02  0.3998004  6.893035e-01

# Bill pay has a negative effect on retention. In other words we are less likely to retain
# someone who uses bill pay

# To quantify the impact take the exponents of the estimates

exp(coefficients(summary(retention_model))[, c("Estimate", "Std. Error")])

                        Estimate Std. Error
(Intercept)          1.1250770   1.056570
profit9              1.0000740   1.000071
online9              1.2746816   1.058124
billpay9             0.5417533   1.143072
age                  1.3200572   1.011163
income               1.1932327   1.007738
tenure9              1.0302643   1.002376
factor(district)1200 1.0483187   1.055276
factor(district)1300 1.0270305   1.068988

# According to the above results, in our "sample",
# the odds of retaining modern customers (vs. losing them) are 0.55 times the odds of retaining traditional customers.


# Generalizing using 99% confidence interval

exp(confint(retention_model, level=.99))
# According to the above results, among the whole population of the customers,
# the odds of retaining modern customers are between 0.39 and 0.78 times the odds of retaining traditional customers.
# and we are 99% confident that the above statement is correct. (1% chance it is incorrect)
