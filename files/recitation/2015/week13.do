// week 13
// Instructor : Young Joon Oh
// Multiple regression model


// import csv file
clear
import delimited C:\Users\YoungJoon\Desktop\class_data.csv

reg earnings school exp height
reg earnings school exp height male

// import csv file
clear
import delimited C:\Users\YoungJoon\Desktop\multiple.csv

codebook mealcat

regress api00 mealcat

// Making dummy variables
tabulate mealcat, gen(mealcat)

regress api00 mealcat1 mealcat2 mealcat3
regress api00 mealcat2 mealcat3
