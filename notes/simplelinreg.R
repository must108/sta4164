# Mustaeen Ahmed

library(readxl)
data <- read_excel("./data/CH05Q01.xls")
df <- data.frame(data)
attach(df)

plot(AGE, DRYWGT)

model <- lm(DRYWGT ~ AGE, data = df) # lm for linear regression

print(model)
print(summary(model)) # get our good ol summary

par(mfrow = c(2, 2)) # allows to show multiple plots at once
plot(model) # get many plots

print(anova(model))

plot(AGE, DRYWGT)
abline(model) # with line of best fit

conf_int_1 <- confint(model) # regular confidence interval
conf_int_2 <- confint(model, level = 0.99) # for a custom percentage

# check for mean of Y at age 11.

age_pred <- data.frame(AGE = 11) # setting a singular data value to 11.
print(predict(model, newdata = age_pred, interval = "confidence"))
# for a confidence interval

print(predict(model, newdata = age_pred, interval = "prediction"))
# for a prediction interval

age_pred_2 <- data.frame(AGE = c(10, 11, 12)) # for y at several ages
print(predict(model, newdata = age_pred_2, interval = "confidence"))
print(predict(model, newdata = age_pred_2, itnerval = "prediction"))

# apply transformations to a variable
print("APPLYING TRANSFORMATIONS!")

plot(AGE, log(DRYWGT))
model2 <- lm(I(log(DRYWGT)) ~ AGE, data = df)

plot(AGE, log(DRYWGT))
abline(model2)