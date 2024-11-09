# Written by Mustaeen Ahmed

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
print(power_y)