# Mustaeen Ahmed

# check for correlation...

attach(mtcars) # default dataset

# check for correlation between horsepower and 1/4 mile time
model <- lm(qsec ~ hp, data = mtcars)
print(summary(model))

# confidence interval can be used like a hypothesis test
print(cor(hp, qsec)) # check for correlation
print("Correlation Test:")
print(cor.test(hp, qsec, conf.level = 0.99)) # hypothesis test for correlation