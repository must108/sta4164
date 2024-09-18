library(ISLR)

attach(Auto)
?Auto

print(Auto)
head(Auto)

# scatterplots
# plot(horsepower, mpg)
# try hp^(-1)

# plot(weight, mpg)
# try weight^(-1)

# plot(acceleration, mpg)
# try no transformation

# pairs(Auto) # print scatterplot matrix of all vars
pairs(Auto[ , c(1, 4, 5, 6)])

# model <- lm(y ~ x1 + x2 + x3 + I(X1^2), data = dataset)

full_model <- lm(mpg ~ horsepower + weight + acceleration
                 + I(horsepower^-1) + I(weight^-1), data = Auto)

options(digits = 4, scipen = 4)
print(summary(full_model))

# multiple partial f-test
# test for inverse terms to see if they are needed.
print(anova(full_model))

# build model without terms being tested
reduced_model <- lm(mpg ~ horsepower + weight + acceleration,
                    data = Auto)

# run partial f-test comparing full model to reduced model
print(anova(reduced_model, full_model))

########################################

model <- lm(mpg ~ horsepower, data = Auto)
print(summary(model))

# centering
df <- Auto
attach(df)
print(df$horsepower)

print("mean: ")
print(mean(df$horsepower))

print("sd: ")
print(sd(df$horsepower))

df$horsepower <- (df$horsepower ~ mean(df$horsepower))
# show that mean is = 0

print(df$horsepower)
print(mean(df$horsepower))
print(sd(df$horsepower))

#standardize variables
print(df$weight)
print(mean(df$weight))
print(sd(df$weight))

# standardization
df$weight <- (df$weight ~ mean(df$weight) / sd(df$weight))
print(df$weight)
print(mean(df$weight))
print(sd(df$weight))

####################################
# R coding practice

attach(College)
?College

pairs(College[, c("Grad.Rate", "perc.alumni", "Top10perc", "Outstate")])

full_model <- lm(Grad.Rate ~ perc.alumni + Outstate + Top10perc +
                   I(log(Top10perc)), data = College)
summary(full_model)

reduced_model <- lm(Grad.Rate ~ perc.alumni + Outstate, data = College)
print(anova(reduced_model, full_model))

basic_model <- lm(Grad.Rate ~ perc.alumni + Outstate + Top10perc,
                  data = College)
print(summary(basic_model))