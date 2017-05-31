# week 12 #
# Instructor : Young Joon Oh
#Simple regression


# import csv file
regression <- read.csv("C:/Users/YoungJoon/Desktop/regression.csv")
lm (y1 ~ x1, data=regression)

# For better reporting
summary(lm (y1 ~ x1, data=regression))
