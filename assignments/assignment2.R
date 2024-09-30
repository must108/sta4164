# written by mustaeen ahmed

library(readxl) # for using in vscode
data <- read_excel("./data/HW1Q6.xlsx")
df <- data.frame(data)
attach(df)

full_model <- lm(SBP ~ AGE + QUET + I(QUET^2), data = df)
print("summary of full model: ")
print(summary(full_model))

reduced_model <- lm(SBP ~ AGE, data = df)
print("summary of reduced model: ")
print(summary(reduced_model))

print("anova: ")
print(anova(reduced_model, full_model))