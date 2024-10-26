# Written by Mustaeen Ahmed - for Assignment 3, STA4164

attach(mtcars)
?mtcars
print(mtcars)

# for part A
print(dim(mtcars))

# for parts B and C
cars_model <- lm(mpg ~ wt * qsec * hp, data = mtcars)
reduced_model <- lm(mpg ~ wt + qsec + hp, data = mtcars)

print(summary(cars_model))
print(summary(reduced_model))

print(anova(reduced_model, cars_model))

# for part D
shaped_model <- lm(mpg ~ wt + qsec + vs + wt:vs + qsec:vs, data = mtcars)
print(summary(shaped_model))

# for part E and F
reduced_shaped_model <- lm(mpg ~ wt + qsec, data = mtcars)
print(anova(reduced_shaped_model, shaped_model))
