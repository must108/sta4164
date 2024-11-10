library(readxl)

data <- read_excel("./data/CH05Q01.xls")
df <- data.frame(data)
attach(df)

# create a labelled plot
plot(AGE, DRYWGT, main = "Age vs. Weights", xlab = "Ages",
     ylab = "Weights")

print("Initial model")
weight_model <- lm(DRYWGT ~ AGE, data = df) # linear regression model
print(summary(weight_model)) # print the typical summary
plot(weight_model) # Residuals vs. Leverage plot

print("Transformed model, squared")
transformed_weight <- lm(DRYWGT ~ I(AGE^2), data = df)
# I function returns a vector of values, good for the tilde operator
print(summary(transformed_weight))

print("Transformed model, log")
transformed_weight_2 <- lm(log(DRYWGT) ~ AGE, data = df)
print(summary(transformed_weight_2))

# predict weights
print("Weight at age 10, 0.95:")
predict(weight_model, newdata = data.frame(AGE = 10),
        interval = "confidence", level = 0.95)

print("Weight at age 10, 0.90:")
predict(weight_model, newdata = data.frame(AGE = 10),
        interval = "prediction", level = 0.90)
# the better one.. not gonna lie

# confidence interval:
print("confidence interval for the initial model:")
print(confint(weight_model, level = 0.90))
