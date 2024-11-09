# Written by Mustaeen Ahmed
library(readxl)

print("Vectors")
vector_1 <- c(1, 3, 2, 5) # creating vectors
print(vector_1)

x <- c(1, 6, 2) # assign like this

?c
# opens documentation for a specific function in r-studio (not applicable here)

print("adding values")
array_y <- c(3, 1, 2)
print(length(array_y))
new_val <- x + array_y
print(new_val)

print("declare first matrix")
# one way to declare a matrix
matrix_x <- matrix(data = c(1, 2, 3, 4), nrow = 2, ncol = 2)
print(matrix_x)

print("declare second matrix")
# another way
matrix_x1 <- matrix(c(1, 2, 3, 4), 2, 2)
print(matrix_x1)

print("declare third matrix")
# a third way, fills with rows instead of columns
matrix_x2 <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2, byrow = TRUE)
print(matrix_x2)

print("printing random dataset")
random_x <- rnorm(n = 50, mean = 0, sd = 1)
# generates random set of data (cool!)
print(summary(random_x))
print(mean(random_x))
print(median(random_x))
print(sd(random_x))

# testing powers
print("testing powers")
power <- x ^ 2
print(power)
power_y <- log(power)
print(power_y) # not a number

# correlation between two variables
set.seed(1234)
x_correl <- rnorm(50)
y_correl <- x_correl + rnorm(50, mean = 50, sd = .1)

print(cor(x_correl, y_correl))

plot(x_correl, y_correl) # build a scatterplot!

plot(x_correl, y_correl, xlab = "This is the x-axis",
     ylab = "This is the y-axis",
     main = "Plot of X vs. Y") # better scatterplot!

print("Matrix A")
matrix_a <- matrix(1:16, nrow = 4, ncol = 4) # indexing the matrix
print(matrix_a)

print("Different points:")
print(matrix_a[2, 3])
print(matrix_a[1:3, 2:4])
print(matrix_a[c(1, 3), 4])
print(matrix_a[1, ])
print(matrix_a[, 2])
print(dim(matrix_a))


# INPUTTING DATA!

data <- read_excel("./data/CH05Q01.xls")
df <- data.frame(data)
attach(df) # can just print columns directly

print(df) # print out whole dataset

print(df$AGE) # print out specific column
print(df$DRYWGT)
hist(df$DRYWGT) # histogram

print(DRYWGT)

print("printing parts of the dataset")
print(df[1, 1]) # 1 index based
print(df[1, "AGE"])
print(df[, "DRYWGT"])