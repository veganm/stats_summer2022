# Week 2, Vega lab meeting 2022
# Today we will be exploring summary statistics of quantitative data
# using code and other material from
#   https://moderndive.com/3-wrangling.html
#
#  Background material for this section is from
#   https://www.khanacademy.org/math/statistics-probability/summarizing-quantitative-data
#
# We will:
# - learn more about pipes and object manipulation in R
# - learn how to generate summary statistics for data in R
# - review the concept of random sampling
# - review summary statistics of data
# - introduce the concept of normality as applied to biological data

# Before we begin, we will define the terms:
# Population; sample; random sample; summary statistic; accuracy; precision

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# First, as usual, we will load the libraries we need
packages<-c("tidyverse","nycflights13", "cowplot", "e1071", "nortest")
install.packages(setdiff(packages, rownames(installed.packages())))

library(tidyverse)
library(nycflights13)
library(cowplot)
library(e1071)
library(nortest)

# Today we will also be using some worm data
# Salmonella enterica LT2-GFP single worms (fed on plates), same as in bleach protocol
# and corresponding data for S. aureus-GFP

PathogenCount<-read.csv("PathogenCount.csv")
glimpse(PathogenCount)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~      SUMMARY STATISTICS IN R       ~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~ Generating summary statistics in R is easy
# Let's start with the weather tibble from the airline data set

glimpse(weather)

# The summarize() function from dplyr (part of tidyverse) takes in a data frame 
# and returns a data frame with only one row corresponding to the summary statistics
summary_temp <- weather %>% 
  summarize(mean = mean(temp), std_dev = sd(temp))
summary_temp

# oops! We forgot something. Let's fix that
summary_temp <- weather %>% 
  summarize(mean = mean(temp, na.rm = TRUE), 
            std_dev = sd(temp, na.rm = TRUE))
summary_temp

# more summaries!
?summarize
summary_temp <- weather %>% 
  summarize(mean = mean(temp, na.rm = TRUE), 
            std_dev = sd(temp, na.rm = TRUE),
            min=min(temp, na.rm = TRUE),
            max=max(temp, na.rm = TRUE),
            q90=quantile(temp, probs=0.9, na.rm=TRUE),
            q75=quantile(temp, probs=0.75, na.rm=TRUE),
            q50=quantile(temp, probs=0.5, na.rm=TRUE),
            q25=quantile(temp, probs=0.25, na.rm=TRUE),
            q10=quantile(temp, probs=0.1, na.rm=TRUE),
            IQR=IQR(temp, na.rm = TRUE),
            n=n())
summary_temp

# We can also group data by values of identification variables
# in which case summarize returns one row per group
# For example, id we want to summarize temperature by month:
summary_monthly_temp <- weather %>% 
  group_by(month) %>% 
  summarize(mean = mean(temp, na.rm = TRUE), 
            std_dev = sd(temp, na.rm = TRUE))
summary_monthly_temp

# We can confirm that this doesn't change the data set, just the meta-data
weather
weather %>% 
  group_by(month)

# and that this change can be un-done with ungroup()
weather %>% 
  group_by(month) %>%
  ungroup()
  
# We can use groupings before doing any kind of summary
# What should this do?
by_origin <- flights %>% 
  group_by(origin) %>% 
  summarize(count = n())
by_origin

# and we can group by more than one variable
# Observe that there are 36 rows to by_origin_monthly 
# because there are 12 months for 3 airports (EWR, JFK, and LGA).
by_origin_monthly <- flights %>% 
  group_by(origin, month) %>% 
  summarize(count = n())
by_origin_monthly

# Why not group by each variable separately?
by_origin_monthly_incorrect <- flights %>% 
  group_by(origin) %>% 
  group_by(month) %>% 
  summarize(count = n())
by_origin_monthly_incorrect

# the second group_by(month) overwrote the grouping structure meta-data of the earlier 
# group_by(origin), so that in the end we are only grouping by month. 
# SYNTAX
# it matters

# ok, so what will happen if we switch the order
#  to group by month and then origin?
by_origin_monthly <- flights %>% 
  group_by(month, origin) %>% 
  summarize(count = n())
by_origin_monthly
