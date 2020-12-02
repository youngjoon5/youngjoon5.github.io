# week 10#
# Instructor : Young Joon Oh


# import all csv files
auto <- read.csv("C:/Users/YoungJoon/Desktop/auto.csv")
fuel <- read.csv("C:/Users/YoungJoon/Desktop/fuel.csv")
cure <- read.csv("C:/Users/YoungJoon/Desktop/cure.csv")
hsb2 <- read.csv("C:/Users/YoungJoon/Desktop/hsb2.csv")

# one sample t-test
t.test(auto$mpg, mu=20) # auto$mpg means 'mpg' variable in the 'auto' dataset
# for Another H1
t.test(auto$mpg, alternative="greater", mu=20)


# Before the proportion test
auto$foreign1<- ifelse(auto$foreign == "Domestic", 0,ifelse(auto$foreign == "Foreign", 1,99))
# One sample proportion test
prop.test(sum(auto$foreign1), length(auto$foreign1), p=0.4, correct=F)
sqrt(3.2523) # you can get Z-score



############
# Two sample
############


# small sample size with equal variance
t.test(fuel$mpg1, fuel$mpg2,var.equal=TRUE)
# for Another H1
t.test(fuel$mpg1, fuel$mpg2,alternative="greater", var.equal=TRUE)


# The data set has many N.A. values which are missing values.
# To deal with it. 
# We need New version of length which can handle NA's: if na.rm==T, don't count them
# Don't worry abour how to make this code.  Just use it.
length2 <- function (x, na.rm=FALSE) {
  if (na.rm) sum(!is.na(x))
  else       length(x)
}

# Two Proportions test
prop.test(c(sum(cure$cure1, na.rm = T), sum(cure$cure2, na.rm = T)),   # na.rm = T means ignoring N.A.
          c(length2(cure$cure1, na.rm = T),length2(cure$cure2, na.rm = T)), correct=F)
sqrt(4.2457) # you can get Z-score



# Paired samples test
t.test(hsb2$write, hsb2$read,paired=T)
