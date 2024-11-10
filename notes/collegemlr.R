
library(ISLR) # for datasets

attach(College)

print(head(College))

pairs(College[, c("Grad.Rate", "perc.alumni", "Top10perc", "Outstate")])

print("full model")
full_model <- lm(Grad.Rate ~ perc.alumni + Outstate + Top10perc
                 + I(log(Top10perc)), data = College)
print(summary(full_model)) # get a full model

print("reduced model")
reduced_model <- lm(Grad.Rate ~ perc.alumni + Outstate, data = College)
print(anova(reduced_model, full_model)) # reduced model ... obv

print("Basic model")
basic_model <- lm(Grad.Rate ~ perc.alumni + Outstate + Top10perc,
                  data = College) # super basic model bruh
print(summary(basic_model))