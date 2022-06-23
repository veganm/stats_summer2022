# Week 4, Vega lab meeting 2022
# Today we will be learning about looping and reviewing R tricks and tips
# using code and other material from
#   https://moderndive.com/3-wrangling.html
#   https://r4ds.had.co.nz/iteration.html
#   https://intro2r.com/loops.html
#   https://intro2r.com/prog_r.html
#   https://ggplot2-book.org/facet.html
#   https://www.dataanalytics.org.uk/axis-labels-in-r-plots-using-expression/
#
# We will:
# - learn more about iteration in R
# - learn about statistical distributions in R
# - review plotting and customization with ggplot()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# First, as usual, we will load the libraries we need
packages<-c("tidyverse","nycflights13", "cowplot", "e1071", "nortest", "ggpubr")
install.packages(setdiff(packages, rownames(installed.packages())))

library(tidyverse)
library(nycflights13)
library(cowplot)
library(e1071)
library(nortest)
library(ggpubr)

# Today we will also be using some worm data
# Salmonella enterica LT2-GFP single worms (fed on plates), same as in bleach protocol
# and corresponding data for S. aureus-GFP

PathogenCount<-read.csv("PathogenCount.csv")
glimpse(PathogenCount)

# From week 3, we had exercises:
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
#~~~~~~~~~          Homework for summary, grouping and mutations
#
# A) Using the weather data frame, write code to get the mean and standard deviation temperature for each day in 2013 for NYC.
# B) Using the flights data frame, identify how many flights took off for each of the three airports for each carrier.
# B.1) Plot the number of flights for each airport and carrier with month as the x-axis.
# C) Consider the diamonds data frame included in the ggplot2 package.
# C.1) Group these data by cut, then calculate mean, median, and IQR for price within each cut type.
# Store this information in a new tibble, then ungroup the data.
# C.2) Plot out price data grouped by (1) cut and (2) clarity. Create these plots again using a different plot type.
# Save all of these plots in a single image file using ggsave().
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#               HOMEWORK FOR LOOPS
# D) Pick one of the loops in section 21.2.1 of R for Data Science.
# For next week, write a for loop to perform that task.
#
# Results are in week3homeworksmaster.R