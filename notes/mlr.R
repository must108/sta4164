# Mustaeen Ahmed

library(ISLR) # provides datasets

attach(Auto)

print(head(Auto)) # prints out the first 6 lines

# output many plots in a grid-like image
par(mfrow = c(2, 2))
plot(horsepower, mpg)
plot(weight, mpg)
plot(acceleration, mpg)

pairs(Auto) # even cooler grid-like image
pairs(Auto[, c(1, 4, 5, 6)])

full_model <- lm(mpg ~ horsepower + weight + acceleration
                 + I(horsepower^-1) + I(weight^-1),
                 data = Auto) # mlr model
options(digits = 4, scipen = 4) # set digits after decimal to 4
print(summary(full_model))

print("anova for initial full model")
print(anova(full_model)) # useful for a multiple partial f-test

print("anova comparing full model to a new, reduced model")
reduced_model <- lm(mpg ~ horsepower + weight + acceleration,
                    data = Auto)
print(anova(reduced_model, full_model))

print("slr comparing mpg with horsepower")
model <- lm(mpg ~ horsepower, data = Auto)
print(summary(model))

# centering the model
new_df <- Auto

new_df$horsepower <- (new_df$horsepower - mean(new_df$horsepower))
# (set the variable to itself - the mean)


# standardize the model
new_df$weight <- (new_df$weight - mean(new_df$weight)) / sd(new_df$weight)
# set the variable to (itself - mean) / std. dev