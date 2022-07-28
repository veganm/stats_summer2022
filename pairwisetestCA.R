#Import dataset 
library(readxl)
worms <- read_excel("fakeworms.xlsx")
View(worms)

##Shapiro-Wilk normality test for the differences
##
shapiro.test(worms$CFU)

#Ftest for equal variances 
ftest <- var.test(CFU ~ Time, data = worms)
ftest

#t-test: unpaired
ttest <- t.test(CFU ~ Time, data = worms, var.equal = TRUE)
ttest