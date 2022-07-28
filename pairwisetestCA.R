#Import dataset 
library(readxl)
library(tidyverse)
worms <- read_excel("fakeworms.xlsx")
View(worms)

# Nic added:
# Visualize the data
worms %>%
  ggplot(aes(x=Time, y=CFU, color=Time))+
  geom_jitter(width=0.1, size=2)+
  scale_y_log10()+
  theme_classic()+
  theme(text=element_text(size=16))

##Shapiro-Wilk normality test for the differences
##
shapiro.test(worms$CFU)

#Ftest for equal variances 
ftest <- var.test(CFU ~ Time, data = worms)
ftest

#t-test: unpaired
ttest <- t.test(CFU ~ Time, data = worms, var.equal = TRUE)
ttest
