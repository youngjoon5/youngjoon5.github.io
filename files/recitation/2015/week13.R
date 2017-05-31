# week 13 #
# Instructor : Young Joon Oh
#Multiple regression


# import csv file
class_data <- read.csv("C:/Users/YoungJoon/Desktop/class_data.csv")
multiple <- read.csv("C:/Users/YoungJoon/Desktop/multiple.csv")
earnings school exp height
reg earnings school exp height male
summary(lm (earnings ~ school + exp + height, data=class_data))
summary(lm (earnings ~ school + exp + height + male, data=class_data))


# 2nd part
summary(lm (api00 ~ mealcat, data=multiple))


# Making Dummy Variables : dummy_1, dummy_2 and dummy_3 for 'mealcat'
for(level in unique(multiple$mealcat)){
  multiple[paste("dummy", level, sep = "_")] <- ifelse(multiple$mealcat == level, 1, 0)
}

summary(lm (api00 ~ dummy_1 + dummy_2 +dummy_3 , data=multiple))
summary(lm (api00 ~ dummy_2 +dummy_3 , data=multiple))

