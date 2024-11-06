# Written by Mustaeen Ahmed - for Assignment 4, STA4164

library(ISLR)
library(olsrr)
library(car)
library(Metrics)

?College
df <- College
attach(df)

# part A
print(summary(df))
print(dim(df))

# part B
df <- df[df$Grad.Rate <= 100, ]
print(dim(df))

# part C
model <- lm(Grad.Rate ~ S.F.Ratio + Terminal + Personal, data = df)
avPlots(model)

# part D
print("Part D")
initial_model <- lm(Grad.Rate ~ S.F.Ratio * Personal + Terminal, data = df)

final_model <- ols_step_backward_aic(initial_model)
print(final_model)

# part E
final_formula <- final_model$model
refitted_model <- lm(final_formula, data = df)

preds <- predict(refitted_model, df)

mae_value <- mae(df$Grad.Rate, preds)
print(mae_value)