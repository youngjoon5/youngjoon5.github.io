# week 11 
# Instructor : Young Joon Oh


# import all csv files
anova <- read.csv("C:/Users/YoungJoon/Desktop/anova.csv")
anova2 <- read.csv("C:/Users/YoungJoon/Desktop/anova2.csv")
anova22 <- read.csv("C:/Users/YoungJoon/Desktop/anova22.csv")

# one-way
aov(a ~ g, data = anova)
# for good-looking outcome
summary(aov(a ~ g, data = anova))


# Two-way
# Use anova2
# Unlinke Stata, for Anova test, R needs nominal variables(Categorical variables) as independent variables.
# So, we need to create new variavles.
anova2$drug1 <- factor(anova2$drug)
anova2$disease1 <- factor(anova2$disease)

summary(aov(systolic ~ drug1 + disease1, data = anova2))
# R outcome is based on regression model. So, there is a very tiny difference.

#For interaction effect
summary(aov(systolic ~ drug1 + disease1 + drug1*disease1, data = anova2))

# Use anova22
anova22$a1 <- factor(anova22$a)
anova22$b1 <- factor(anova22$b)

summary(aov(y ~ a1 + b1, data = anova22))
summary(aov(y ~ a1 + b1 + a1*b1, data = anova22))


