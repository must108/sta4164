
library(ISLR)
df <- Credit

attach(df)

full_model <- lm(Rating ~ Age + Student + Age * Student, data = df)
# testing for interaction variables
print(summary(full_model))


# diff hypothesis tests (ugh)
print("test for coincidence")
reduced_coin <- lm(Rating ~ Age, data = df) # test for coincidence
print(anova(reduced_coin, full_model))

print("Test for parallelism")
reduced_parallel <- lm(Rating ~ Age + Student, data = df) # test for parallelism
print(anova(reudced_parallel, full_model))

print(anova(reduced_coin, reduced_parallel)) # test for equal intercepts

# now interpret!