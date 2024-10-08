# written by Mustaeen Ahmed, for STA4164 Project A

# part A
dataset <- read.csv("./data/heart_failure_clinical_records_dataset.csv")
df <- data.frame(dataset)
attach(df)
print(df)

# part B
plot(age, serum_creatinine,
  main = "Age vs Serum Creatinine",
  xlab = "Age",
  ylab = "Serum Creatinine"
)

# part C/D/E
heart_model <- lm(serum_creatinine ~ age, data = df)
print(summary(heart_model))

# part F
correl_heart <- cor.test(age, serum_creatinine)
print(correl_heart$conf.int)

# part G
qqnorm(residuals(heart_model), main = "Q-Q Plot")
qqline(residuals(heart_model), col = "red")

# part H
model_log <- lm(log(serum_creatinine) ~ age, data = df)
model_sqrt <- lm(sqrt(serum_creatinine) ~ age, data = df)
print(summary(model_log))
print(summary(model_sqrt))

# par(mfrow = c(2, 2))
# plot(model_log)

par(mfrow = c(2, 2))
plot(model_sqrt)
