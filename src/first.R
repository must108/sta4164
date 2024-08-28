# first R programming in sta4164

library(readxl)
data <- read_excel("./data/CH05Q01.xls") # this is a tibble

dataset <- data.frame(data) # convert to data frame (you know this!)

print("Prints the dataset:")
print(dataset)

# comments are very cool!

print("Prints the age col in the dataset")
print(dataset$AGE)

print("Prints the first row, first col")
print(dataset[1, 1]) # prints the first row, first col

print("Prints all columns for the 3rd")
print(dataset[3, ]) # prints all cols from the 3rd

print("Prints out the second column")
print(dataset[, 2]) # prints out the second column

######################

print("Age column in dataset")
attach(dataset) # looks for variable names in datasets
print(AGE) # will work once dataset is attached

######################

print("Output scatterplot for data")
?plot
plot(AGE, DRYWGT,
  main = "Age v.s. Weights", xlab = "Ages", ylab = "weights"
)

print("Build a model") # building a model... obviously
# model <= lm(y ~ x, data = dataset)
weight_model <- lm(DRYWGT ~ AGE, data = dataset)
print(summary(weight_model))
plot(weight_model)

# transformation of x, use I function
print("transform x")
weight_model2 <- lm(DRYWGT ~ I(AGE^2), data = dataset)
print(summary(weight_model2))

# transformation of Y, no I function
print("transform Y")
weight_model3 <- lm(log(DRYWGT) ~ AGE, data = dataset)
print(summary(weight_model3))

# predict weight at age = 10
print("predict weight at age = 10")
print("confidence:")
print(predict(weight_model, newdata = data.frame(AGE ~ 10),
              interval = "confidence"))

print("prediction:")
print(predict(weight_model, newdata = data.frame(AGE ~ 10),
              interval = "prediction", level = 0.90))

# confidence intervals
print("confidence intervals:")
print(confint(weight_model, level = 0.90))
