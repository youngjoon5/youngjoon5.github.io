*\week 11 
\\ Instructor : Young Joon Oh


*\ Do-file or R script is basically a text file. 
*\ So, you can open and edit it with any kind of 'notepad' program without STATA or R. 

*\import csv file
*\ There are two ways in STATA 13 : 1) Use do file
*\ 2) File -> import -> text data(...csv)

clear
import delimited C:\Users\YoungJoon\Desktop\anova.csv

su
codebook

oneway a g, tab 

*\ If you want to use 'anova' command, you need to create another variable for 'g'.
*\ This is because 'g' is nominal variable. We need to assign numerical values to 'g'.
*\ This is the same way for making a new variable 'foreign1' for prorportion test.

generate g1 = 1
*\ Now, we create g1 and all value for g1 are 1.
replace g1 = 2 if g == "b"
*\ Now, If g = "b", then assign 2 to g1.
replace g1 = 3 if g == "c"
*\ Now, If g = "c", then assign 3 to g1.

anova a g1


*\ Two way - ANOVA *\
clear
import delimited C:\Users\YoungJoon\Desktop\anova2.csv
su
codebook

*\ making table
tab drug disease

*\two-way anova test
anova systolic drug disease

anova systolic drug disease drug#disease
anova systolic drug##disease

clear
import delimited C:\Users\YoungJoon\Desktop\anova22.csv
su 
codebook
tab a b
anova y a b
anova y a##b
