library(tidyverse)

# set the them for ggplot graphics
theme_set(theme_minimal() + theme(axis.title.y = element_text(angle = 0)))

# load data
PROFILES <- read_csv("profiles.csv")
DATA9 <- read_csv("data_2009.csv")
DATA0 <- read_csv("data_2010.csv")

# Merge dataset and create one
DATA <- PROFILES %>%
  full_join(DATA9, by = "id") %>%
  full_join(DATA0, by = "id")

# Handle missing values
DATA <- DATA %>%
  mutate(missing_age = is.na(age) * 1) %>%
  mutate(missing_income = is.na(income) * 1) %>% 
  replace_na(list(age = 0, income = 0))
  

# Run a regression model for prediction
res <- lm(profit0 ~ profit9 + online9 + tenure9 + factor(district) +
            age + income + missing_age + missing_income
          , DATA, na.action="na.exclude")

coefficients(summary(res))

# create and compare 3 predictive models
PRED <- DATA %>%
  select(profit9, profit0) %>%
  mutate(pred_dumber = mean(profit9),
         pred_dumb = profit9,
         pred_smart = predict(res, DATA),
         actual = profit0)

scatter_plot_template <- ggplot(data = PRED) +
  xlim(-200, 2000) +
  ylim(-200, 2000) +
  coord_equal() +
  theme_minimal()

scatter_plot_template + geom_point(aes(x = actual, y = pred_dumber), alpha = 0.01)
scatter_plot_template + geom_point(aes(x = actual, y = pred_dumb), alpha = 0.01)
scatter_plot_template + geom_point(aes(x = actual, y = pred_smart), alpha = 0.01)

# compare the error of predictions
ERROR_WIDE <- PRED %>%
  mutate(dumber = pred_dumber - actual, dumb = pred_dumb - actual, smart = pred_smart - actual) %>%
  select(dumber, dumb, smart) %>%
  mutate_all(funs(as.double))

ERROR <- ERROR_WIDE %>%
  gather(dumber, dumb, smart, key = "predictive_model", value = "error") %>%
  mutate(predictive_model = factor(predictive_model, levels = c("dumber", "dumb", "smart")))

distribution_plot_theme <-
  ggplot(data = ERROR) +
  ylim(-2000, 2000) +
  theme_minimal()


distribution_plot_theme +
  geom_jitter(aes(x = predictive_model, y = error), alpha = 0.01)
  
distribution_plot_theme +
  geom_boxplot(aes(x = predictive_model, y = error), alpha = 0.01)

distribution_plot_template +
  geom_violin(aes(x = predictive_model, y = error))


ERROR %>%
  group_by(predictive_model) %>%
  summarize(
    ME = mean(error),
    MAE = mean(abs(error)),
    MSE = mean(error ^ 2),
    SMSE = sqrt(mean(error ^ 2)),
    MedianE = median(error),
    MedianAE = mean(abs(error)),
    MedianSE = mean(error ^ 2),
    SMedianSE = sqrt(mean(error ^ 2))
)

## Find the retained and churned customers

DATA <- DATA %>% mutate(
  retained = (!is.na(profit9) & !is.na(profit0)) * 1,
  new = (is.na(profit9) & !is.na(profit0)) * 1,
  churned = (!is.na(profit9) & is.na(profit0)) * 1
)

colSums(select(DATA, retained, new, churned))

with(DATA, sum(retained) / (sum(retained) + sum(churned)))     

# Closer look at linear regression
# A. Closer look at the distribution of variable we want to predict (profit0)

ggplot(DATA) +
  geom_histogram(aes(x = profit0), binwidth = 10)

ggplot(DATA) +
  geom_histogram(aes(x = profit0), binwidth = 10) +
  lims(x = c(-200, 1000))

DATA %>% group_by(profit0) %>% tally() %>% arrange(desc(n))

ggplot(DATA) +
  geom_jitter(aes(x = "", y = profit0), alpha = 0.05) +
  geom_hline(yintercept=mean(DATA$profit0, na.rm = TRUE), color="red", size = 2) +
  lims(y = c(-200, 1000))

# Closer look at linear regression
# B. Closer look at regression results

ggplot(DATA) +
  geom_point(aes(x = profit9, y = profit0), alpha = 0.05) +
  lims(x = c(-200, 1000), y = c(-200, 1000)) +
  coord_equal()

res <- lm(profit0 ~ profit9, DATA, na.action = "na.exclude")
res

ggplot(DATA) +
  geom_point(aes(x = profit9, y = profit0), alpha = 0.05) +
  geom_point(aes(x = profit9, y = predict(res)), color='red') +
  lims(x = c(-200, 1000), y = c(-200, 1000)) +
  coord_equal()

# Now, let us see what logistic regression does

ggplot(DATA) +
  geom_point(aes(x = profit9, y = retained), alpha = 0.01) +
  lims(x = c(-200, 500))

ggplot(DATA) +
  geom_boxplot(aes(x = factor(retained), y = profit9), alpha = 0.01) +
  lims(y = c(-200, 500)) +
  coord_flip()
  

EXTENDED_RANGE <- tibble(profit9 = -10000:10000)

res <- lm(retained ~ profit9, DATA, na.action = "na.exclude")
ggplot(DATA) +
  geom_point(aes(x = profit9, y = retained), alpha = 0.05) +
  geom_point(aes(x = profit9, y = predict(res)), color='red')

ggplot(DATA) +
  geom_point(aes(x = profit9, y = retained), alpha = 0.05) +
  geom_line(aes(x = profit9, y = predict(res, newdata = EXTENDED_RANGE)), data = EXTENDED_RANGE, color = "blue") +
  geom_point(aes(x = profit9, y = predict(res)), color='red')

res <- glm(retained ~ profit9, binomial(link='logit'), DATA, na.action = "na.exclude")
ggplot(DATA) +
  geom_point(aes(x = profit9, y = retained), alpha = 0.05) +
  geom_point(aes(x = profit9, y = predict(res, type='response')), color='red')

ggplot(DATA) +
  geom_point(aes(x = profit9, y = retained), alpha = 0.05) +
  geom_line(aes(x = profit9, y = predict(res, newdata = EXTENDED_RANGE, type = 'response')), data = EXTENDED_RANGE, color = "blue") +
  geom_point(aes(x = profit9, y = predict(res, type='response')), color='red')

# Using logistic regression:

res <- glm(retained ~ profit9 + online9 + tenure9 + factor(district) +
             age + income + missing_age + missing_income,
           binomial(link='logit'), DATA, na.action = "na.exclude")

coefficients(summary(res))

ggplot(DATA) +
  geom_point(aes(x = predict(res), y = retained), alpha = 0.05) +
  geom_point(aes(x = predict(res), y = predict(res, type='response')), color='red')

# calculate predicted retention rate
mean(predict(res, type='response'))
