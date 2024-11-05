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