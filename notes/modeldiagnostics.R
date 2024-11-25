
# full model diagnostics from sta4164

library(MASS)
library(Metrics)
library(car)
library(olsrr)
df <- Boston  # dataset

attach(df)
print(summary(df))  # basic summary of the dataset
# we can use this to see if all variables have correct values.

print(head(Boston))
# actually printing the dataset (head :P (freaky))...

# removing a variable
# remove rows where crime is above 50:
df2 <- df[df$crim < 50, ]
print(summary(df2))

# dealing with categorical variables

# bar chart
chas_table <- table(chas)
barplot(chas_table, xlab =
          "Charles River Dummy Variable (1 = borders Charles River, 0 = no)",
        ylab = "Frequency")

# box plot
boxplot(medv ~ chas)

# numeric graphs (histogram)
hist(medv)  # histogram of the variable medv
hist(crim)  # histogram of the variable crim

# more boxplots
boxplot(medv, xlab = "medv")
boxplot(crim, xlab = "crim")

# scatterplot matrix
pairs(df)

# only some columns in scatterplot matrix
pairs(df[, c(14, 1, 2, 3, 4)])
pairs(df[, c(14, 5, 6, 7, 8)])
pairs(df[, c(14, 9, 10, 11, 12, 13)])

# correlation matrix
cor(df)

# remove categorical variables from plot. in this case, chas
cor(df[, -4])

# partial regression
# "." means to use all other variables in the dataset
full_model <- lm(medv ~ ., data = df)
crPlots(full_model)  # generate partial residual plots for regression

# apply transformations
transformed_full_model <- lm(medv ~ . + I(rm^2) + I(1 / dis) + i(log(lstat)),
                             data = df)
crPlots(transformed_full_model)

# assessing assumptions
full_model <- lm(medv ~ . + I(rm^2) + I(1 / dis) + I(log(lstat)), data = df)
plot(full_model)

par(mfrow = c(2, 2)) # 2x2 of graphs
par(mfrow = c(1, 1)) # 1x1 of graphs image

print("assessing normality")
print(shapiro.test(full_model$residuals))

boxCox(full_model) # see what transformation is needed

plot(full_model$fitted.values, studres(full_model))
# jackknife residuals for residual plot
# studres gets studentized residuals
abline(h = 0) # adds straight line to a plot for reference

# assessing outliers
print(tail(sort(hatvalues(full_model)), n = 10)) # print 10 largest values

# cook's distance, compare values to 1
print(tail(sort(cooks.distance(full_model)), n = 10))  # print 10 largest values

# get jackknife residuals
# assume:
# Leverage: Compare values to 2(k+1)/n
# k=16, n=506, 2(17)/506=.1343874

t <- qt(.025, 506 - 16 - 2, lower.tail = FALSE)
print(t)

print(head(sort(studeres(full_model)), n = 20))
print(tail(sort(studres(full_model)), n = 20))

# remove observation 365
df4 <- df[-c(365), ]
print(df4[c(364, 365), ])

# new transformation
full_model2 <- lm(
                  sqrt(medv) ~ . + I(rm^2) + I(1 / dis)
                  + I(log(lstat)), data = df)
plot(full_model2)
print(summary(full_model2))

shapiro.test(full_model2$residals) # check again for assumption violation

# function for residual plotting (wow...)
residual_plotting <- function(residuals) {
  m <- mean(residuals)
  s <- sd(residuals)
  hist_data <- hist(residuals, breaks = 24)
  x_values <- seq(min(residuals), max(residuals), length = 200)
  y_values <- dnorm(x_values, mean = mean(residuals), sd = sd(residuals))
  y_values <- y_values * diff(hist_data$mids[1:2]) * length(residuals)
  lines(x_values, y_values, lwd = 2)
  print(m, s)
}

# first plot
residual_plotting(full_model2$residuals)
# second plot
residual_plotting(full_model$residuals)

# check collinearity
options(scipen = 20)  # prevent scientific notation output
ols_coll_diag(full_model)

# standardize/center variables
df$lstat <- ((lstat - mean(lstart)) / sd(lstat)) + (min(lstat) + 1)
df$rm <- (rm - mean(rm)) / sd(rm)

# re-run model
full_model_3 <- lm(medv ~ . + I(rm^2) + I(1 / dis) + I(log(lstat)), data = df)
ols_coll_diag(full_model_3)

full_model_4 <- lm(medv ~ . + I(rm^2) + I(1 / dis), data = df)
ols_coll_diag(full_model_4)

# re-check assumptions
plot(full_model_4)
plot(full_model_4$fitted.values, studres(full_model))
abline(h = 0)

# chapter 16

set.seed(1) # to ensure repeatable results
df <- Boston
attach(df)
dim(df)

full <- df

# 50/50 train/test split
train <- sample(0:nrow(full), nrow(full) * 0.50) # training set
test <- (-train) # put all other obs into test set

# the actual split
train_set <- full[train, ]
test_set <- full[test, ]

print(train_set)
print(test_set)

attach(train_set)

full_model <- lm(medv ~ . + I(rm^2) + I(1 / dis), data = train_set)

# get best model. very slow apparently
# (won't be too much of a problem... we got fast pcs)
print(head(model_all[order(model_all$adjr, descreasing = T), ]))
print(head(model_all[order(model_all$aic, decreasing = F), ]))

# backward selection, stepwise, use p-value as criteria
model_backward <- ols_step_backward_p(full_model, prem = 0.1, details = TRUE)
print(model_backward)
plot(model_backward)

# using aic for backward selection
model_backward2 <- ols_step_backward_aic(full_model, details = TRUE)
print(model_backward2)

# forward selection
model_forward <- ols_step_forward_p(full_model, penter = 0.1, details = TRUE)
print(model_forward)

# using aic for forward selection
model_forward_aic <- ols_step_forward_aic(full_model, details = TRUE)
print(model_forward_AIC)
plot(model_forward_AIC)

# piecewise selection
model_step1 <- ols_step_both_p(full_model, pent = 0.1,
                               prem = 0.3, details = TRUE)
print(model_step1)

model_step2 <- ols_step_both_aic(full_model, details = TRUE)
print(model_step2)

# check reliability and model fit
df2 <- train_set[, -c(2)]
print(df2)

final_model <- lm(medv ~ . + I(rm^2) + I(1 / dis), data = df2)
print(summary(final_model))

# calculate y-hats for training model
pred_train <- predict(final_model, train_set)
print(mse(train_set$medv, pred_train))

pred_test <- predict(final_model, train_set)
print(mse(test_set$medv, pred_test))

# calculate y-hats for training model (again)
pred_train <- predict(final_model, train_set)
print(mae(train_set$medv, pred_train))

pred_test <- predict(final_model, test_set)
print(mae(test_set$medv, pred_test))

# overfit data (making the model larger, and using for regression analysis)
big_model <- lm(medv ~ (.)^2, data = train_set)

pred_train <- predict(big_model, train_set)
print(mae(train_set$medv, pred_train))

pred_test <- predict(big_model, test_set)
print(mae(test_set$medv, pred_test))

big_model <- lm(medv ~ (.)^3, data = train_set)

pred_train <- predict(big_model, train_set)
print(mae(train_set$medv, pred_train))

pred_train <- predict(big_model, test_set)
print(mae(test_set$medv, pred_test))
