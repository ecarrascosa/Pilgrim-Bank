library(tidyverse)

######## Prepare Data

# Load the data
DATA9 <- read_csv("data_2009.csv")
DATA0 <- read_csv("data_2010.csv")
PROFILES <- read_csv("profiles.csv")

# merge the 3 datasets using the variable id
DATA <- DATA9 %>%
  full_join(DATA0, by = "id") %>%
  full_join(PROFILES, by = "id")

# handle missing values
DATA <- DATA %>%
  mutate(missing_age = is.na(age) * 1) %>%
  mutate(missing_income = is.na(income) * 1) %>%
  replace_na(list(age = 0, income = 0))

######## Question 3: Does use of electronic bill pay affect customer profitability?

# Since, our objective is to explain the effect of the use of electronic bill pay on
# profit which is a "continuous variable" (also called quantitative or numeric)
# the appropriate method is linear regression model, implemented using "lm" function in R.

#### Imagine It Is 1999

# First, we imagine that it is 1999 and we only have access to data available by then.
# So, we ignore the profit0, online0, and billpay0
# We want to create a linear model to quantify the effect of billpay9 on profit9
# billpay9: if the customer used electronic bill pay during 1999 (0 = No 1 = Yes)
# profit9: profit from the customer during 1999
# We want to control for the effect of other variables to reduce omitted variable bias
# So, we include those variables as control.
model_profit9 <- lm(profit9 ~ online9 + billpay9 + age + income + tenure9 + factor(district) + missing_age + missing_income, 
          data = DATA, na.action = "na.exclude")

## Interpreting the results for the "sample":
coefficients(summary(model_profit9))
# Results:
#                         Estimate Std. Error     t value      Pr(>|t|)
# (Intercept)          -104.001136  7.6244794 -13.6404245  3.032481e-42
# online9                 3.037770  4.9069904   0.6190698  5.358748e-01
# billpay9               78.901825 12.3755033   6.3756457  1.847044e-10
# age                    16.611886  1.1474990  14.4766014  2.414375e-47
# income                 16.754372  0.7550909  22.1885503 2.974772e-108
# tenure9                 4.744120  0.1917165  24.7455014 6.530551e-134
# factor(district)1200   20.935514  5.0835566   4.1182809  3.826784e-05
# factor(district)1300    7.782001  6.2543541   1.2442534  2.134155e-01
# missing_age            63.199428  9.1786330   6.8854947  5.866197e-12
# missing_income         56.434960  8.9906331   6.2770842  3.494531e-10

# According to the above results, 
# the customers in our "sample" who used electronic bill pay are approximately $79 more profitable.

## Generalizing the results to the "whole population":

# For generalizing the results to the whole population, we cannot be 100% correct.
# So, we have to select a level of confidence that we are comfortable with.
# In this example, we decide to have 99% confidence level.
confint(model_profit9, level=.99)
# Result:
#                           0.5 %     99.5 %
# (Intercept)          -123.641679 -84.360593
# online9                -9.602563  15.678102
# billpay9               47.022716 110.780933
# age                    13.655946  19.567826
# income                 14.809270  18.699475
# tenure9                 4.250261   5.237979
# factor(district)1200    7.840350  34.030679
# factor(district)1300   -8.329120  23.893122
# missing_age            39.555409  86.843447
# missing_income         33.275226  79.594694

# According to the above results, among the whole population of the customers,
# the ones who used electronic bill pay are approximately between $47 and $111 more profitable,
# and we are 99% confident that the above statement is correct. (1% chance it is incorrect)

# If the question is simply does the use of electronic bill pay has any effect on profitability

# IMPORTANT NOTE: the uncertainty captured by confidence interval
# is only about "sampling error".
# Other sources of error, for example omitted variables bias, are hard to quantify.
# Our only chance against omitted variable bias is to control for all important variables.

#### Imagine It Is 2000

# Now, we imagine that it is 2000 and we have access to data from both 1999 and 2000.

# We want to create a linear model to quantify the effect of billpay0 on profit0
# billpay0: if the customer used electronic bill pay during 2000 (0 = No 1 = Yes)
# profit0: profit from the customer during 2000
# We want to control for the effect of other variables to reduce omitted variable bias
# We might think that may be some information from the previous year is also
# usefull for helping us control for omited variables.
# for example, we can additionally control for the following variables:
#   - customer profit during the last year (profit9)
#   - whether the customer have used online services last year (online9)
#   - whether the customer have used electronic bill pay last year (billpay9)
model_profit0 <- lm(profit0 ~ profit9 + online9 + billpay9 + online0 + billpay0 +
                      age + income + tenure9 + factor(district) + missing_age + missing_income, 
                    data = DATA, na.action = "na.exclude")

## Interpreting the results for the "sample":
coefficients(summary(model_profit0))
# Results:
#                         Estimate   Std. Error     t value     Pr(>|t|)
# (Intercept)          -10.1346880  9.732918732  -1.0412794 2.977554e-01
# profit9                0.8246125  0.007108014 116.0116694 0.000000e+00
# online9                2.6389577  8.065384893   0.3271955 7.435226e-01
# billpay9             -14.3536761 17.320327903  -0.8287185 4.072712e-01
# online0               13.1545603  6.736574805   1.9527075 5.086482e-02
# billpay0              32.0960509 12.923457940   2.4835498 1.301418e-02
# age                   -0.4048841  1.441920625  -0.2807950 7.788698e-01
# income                 6.9853684  0.942890715   7.4084603 1.316044e-13
# tenure9                0.9514881  0.246858135   3.8543922 1.162923e-04
# factor(district)1200   6.2091022  6.644507764   0.9344714 3.500694e-01
# factor(district)1300   7.8442606  8.158737218   0.9614552 3.363321e-01
# missing_age            3.7038392 11.723651010   0.3159288 7.520590e-01
# missing_income        31.6473845 11.473064991   2.7584072 5.812380e-03


# According to the above results, 
# the customers in our "sample" who used electronic bill pay are approximately $32 more profitable

## Generalizing the results to the "whole population" with 99% confidence level

confint(model_profit0, level=.99)
# Result:
#                             0.5 %     99.5 %
# (Intercept)          -35.2068392 14.9374632
# profit9                0.8063022  0.8429229
# online9              -18.1376001 23.4155156
# billpay9             -58.9711122 30.2637599
# online0               -4.1989619 30.5080826
# billpay0              -1.1949793 65.3870810
# age                   -4.1192943  3.3095260
# income                 4.5564671  9.4142697
# tenure9                0.3155776  1.5873985
# factor(district)1200 -10.9072539 23.3254583
# factor(district)1300 -13.1727743 28.8612955
# missing_age          -26.4964695 33.9041479
# missing_income         2.0925894 61.2021797

# According to the above results, among the whole population of the customers,
# the ones who used electronic bill pay in 2000 are approximately
# between $1 less and $65 more profitable then the ones who have not used electronic bill pay in 2000,
# and we are 99% confident that the above statement is correct. (1% chance it is incorrect)

# ANALYSIS: After using new data and controling for new variables we see a different picture.
# This is a case of omitted variable bias in full display.
# Solution: use correlational analysis similar the above analysis as an exploratory analysis.
# If you find a rlationship that you have to accurately quantify, run randomized experiment with control group
# to have an unbiased estimate of the effect of one variable on the other.
