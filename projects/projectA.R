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

# part C
heart_model <- lm(serum_creatinine ~ age, data = df)
print(summary(heart_model))
