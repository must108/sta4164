
library(MASS)
library(olsrr)
library(glmnet)  # for penalizing with lambda
df <- Boston

set.seed(123)
train <- sample(0:nrow(df), nrow(df) * 0.50)
test <- (-train)

# split data into train/test (50/50 split)
train_set <- df[train, ]
test_set <- df[test, ]

# backward selection for comparison
full_model <- lm(medv ~ ., data = train_set)
model_backward <- ols_step_backward_aic(full_model, details = TRUE)
print(model_backward)

df_aic <- train_set[, -c(3, 7)]
aic_model <- lm(medv ~ ., data = df_aic)

# lasso regression
grid <- 10^seq(10, -2, length = 100) # list of possible lambda values
x1 <- as.matrix(train_set[, -14])

set.seed(123)

cv_out <- cv.glmnet(x1, train_set$medv, alpha = 1)
plot(cv_out)
bestlam1 <- cv_out$lambda.min

# optimized lambda value for lasso regression
out <- glmnet(x1, train_set$medv, alpha = 1, lambda = grid)
lasso_coef <- predict(out, type = "coefficients", s = bestlam1)
print(lasso_coef)

# ridge regression
set.seed(123)

cv_out <- cv.glmnet(x1, train_set$medv, alpha = 0)
plot(cv_out)

bestlam2 <- cv_out$lambda.min

# optimized lambda value
out2 <- glmnet(x1, train_set$medv, alpha = 0, lambda = grid)
ridge_coef <- predict(out2, type = "coefficients", s = bestlam2)
print(ridge_coef)

# must manually create interaction/transformed terms
df2 <- df
df2$rm2 <- df$rm^2
df2$invdis <- 1 / df$dis
df2$loglstat <- log(lstat)

# repeating lasso regression after transformations
grid <- 10^seq(10, -2, length = 100)
x <- as.matrix(df2[, -14])

set.seed(1)
cv_out <- cv.glmnet(x, df2$medv, alpha = 1)
plot(cv_out)
bestlam <- cv_out$lambda.min

# use optimized lambda
out <- glmnet(x, df2$medv, alpha = 1, lambda = grid)
lasso_coef <- predict(out, type = "coefficients", s = bestlam)
print(lasso_coef)

# compare models

# for full model
pred_aic_train <- predict(aic_model, df_aic)
print(mse(df_aic$medv, pred_aic_train))

pred_full_test <- predict(aic_model, test_set)
print(mse(test_set$medv, pred_aic_test))

# for lasso
pred_lasso_train <- predict(out, newx = x1, s = bestlam1)
print(mse(train_set$medv, pred_lasso_train)) # calculate mse

x_test <- as.matrix(test_set[, -14])
pred_lasso_test <- predict(out, newx = X_test, s = bestlam1)
print(mse(test_set$medv, pred_lasso_test))

# for ridge
pred_ridge_train <- predict(out2, newx = x1, s = bestlam2)
print(mse(train_set$medv, pred_ridge_train))

pred_ridge_test <- predict(out2, newx = x_test, s = bestlam2)
print(mse(test_set$medv, pred_ridge_test))

# scale dataset
x_scaled <- scale(train_set[, -whihc(names(df) == "medv")])

# excluding response
x_scaled <- x_scaled[, -c(14)]
x_scaled <- as.matrix(x_scaled)
grid <- 10^seq(10, -2, length = 100)

set.seed(123)

# find optimal penalty value, alpha = 1 lasso, alpha = 0 for ridge
cv_out <- cv.glmnet(x_scaled, train_set$medv, alpha = 1)
plot(cv_out)
bestlam3 <- cv_out$lambda.min
print(bestlam3)

# use optimized lambda value in lasso
out <- glmnet(x_scaled, train_set$medv, alpha = 1, lambda = grid)
lasso_coef <- predict(out, type = "coefficients", s = bestlam3)
print(lasso_coef)

# training mse
pred_lasso <- predict(out, newx = x_scaled, s = bestlam3)
print(mse(train_set$medv, pred_lasso))

# testing mse
x_scaled_test <- scale(test_set[, -which(names(df) == "medv")])
x_scaled_test <- as.matrix(x_scaled_test[, -c(14)])
pred_lasso_test1 <- predict(out, newx = x_scaled_test, s = bestlam3)
print(mse(test_set$medv, pred_lasso_test1))