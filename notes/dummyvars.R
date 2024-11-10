
library(ISLR)
attach(mtcars)
print(head(mtcars))

# use as.factor to treat certain vars as dummy vars
model <- lm(mpg ~ wt + as.factor(cyl) + wt * as.factor(cyl))
print(summary(model))

bad_model <- lm(mpg ~ wt + cyl + wt * cyl) # this won't work
print(summary(bad_model))

# if a variable like am is in 0/1 form, then it is already a dummy var
model_2 <- lm(mpg ~ wt + cyl + wt * cyl)
print(summary(model_2))

ok_model <- lm(mpg ~ wt + as.factor(am) + wt * as.factor(am)) # can also do this
print(summary(ok_model))

# test for coincidence
print("test for coincidence")
reduced_model <- lm(mpg ~ wt, data = mtcars)
print(anova(reduced_model, model))

# test for parallelism
print("test for parallelism")
reduced_model2 <- lm(mpg ~ wt + as.factor(cyl), data = mtcars)
print(anova(reduced_model2, model))