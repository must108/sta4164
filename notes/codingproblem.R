
library(ISLR)

attach(College)
print(head(College))

par(mfrow = c(2, 2))
plot(Grad.Rate, Top10perc)
plot(Grad.Rate, Outstate)
plot(Grad.Rate, perc.alumni)

first_model <- lm(Grad.Rate ~ perc.alumni + Outstate
                  + Top10perc + log(Top10perc))
print(summary(first_model))

reduced_model <- lm(Grad.Rate ~ perc.alumni + Outstate)
print(anova(reduced_model, model))

second_reduced_model <- lm(Grad.Rate ~ perc.alumni + Outstate + Top10perc)

print(anova(second_reduced_model, first_model))

# now compare these models to find the best one