# week 9#
# Instructor : Young Joon Oh
# ctrl + enter : Run



# import csv file
auto <- read.csv("C:/Users/YoungJoon/Desktop/auto.csv")

str(auto)

? str

auto$price
summary(auto)
summary(auto$price)
var(auto$price)
sd(auto$price)

? describe

library(psych)
describe (auto$price)
? describe

