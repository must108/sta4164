# first R programming in sta4164

library(readxl)
data <- read_excel("./data/CH05Q01.xls") # this is a tibble

dataset <- data.frame(data) # convert to data frame (you know this!)

print(dataset)