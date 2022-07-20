# Vega lab meeting summer week 7
# Today we will start our discussion of comparisons and hypothesis testing
# using code and other materials from
#
# http://www.sthda.com/english/wiki/comparing-means-in-r
# https://rpubs.com/nmccurtin/comparingtwomeans
#
# Background information is from
#
# https://www.khanacademy.org/math/statistics-probability/designing-studies
# https://www.khanacademy.org/math/statistics-probability/significance-tests-one-sample
# https://www.khanacademy.org/math/statistics-probability/significance-tests-confidence-intervals-two-samples
#
# We will:
# - Review standard work-horse hypothesis tests
# - Discuss the assumptions made by these tests
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# First, as usual, we will load the libraries we need
packages<-c("tidyverse", "cowplot", "e1071", "ggpubr")
install.packages(setdiff(packages, rownames(installed.packages())))

library(tidyverse)
library(cowplot)
library(e1071)
library(ggpubr)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~