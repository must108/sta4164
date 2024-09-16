library(readxl)
data <- read_excel("./data/HW1Q6.xlsx")
df <- data.frame(data)
attach(df)

# plot(AGE, SBP)
# plot(log(AGE), SBP)

model <- lm(SBP ~ AGE, data=df)
print(summary(model))

par(mfrow = c(2,2))
plot(model)

print(anova(model))

print(confint(model, level=0.99))

pred_val <- data.frame(AGE=55)

print(predict(model, newdata=pred_val, interval = "confidence", level = 0.95))
print(predict(model, newdata=pred_val, interval = "prediction", level = 0.95))

print("Last Step: ")
print(cor(AGE, SBP))
print(cor.test(AGE, SBP))
