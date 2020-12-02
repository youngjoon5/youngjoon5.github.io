*\week 10 For Hypothesis Test *\
*\EPPS 2302 by Instructor : Young Joon Oh 
*\ Mar. 2015 


*\import auto.csv file
clear
import delimited C:\Users\YoungJoon\Desktop\auto.csv

*\ One sample t-test 

summarize mpg
su mpg
ttest mpg == 20

*\ Before the proportion test, we need to create new variable
codebook foreign
su foreign  
*\ foreign is a nominal variable. 

generate foreign1 = 0 
replace foreign1 = 1 if foreign == "Foreign"

*\ proprotion test - one sample
prtest foreign1 == .4


*\ TWO SAMPLES

*\import fuel.csv file
clear
import delimited C:\Users\YoungJoon\Desktop\fuel.csv

*\ Test for Two samples - small sample size and equal variance
ttest mpg1==mpg2, unpaired


*\import cure.csv file
clear
import delimited C:\Users\YoungJoon\Desktop\cure.csv

*\ Test for Two proportion samples
prtest cure1==cure2



*\import hsb2.csv file
clear
import delimited C:\Users\YoungJoon\Desktop\hsb2.csv

*\ Test for Two paired samples
ttest write==read
