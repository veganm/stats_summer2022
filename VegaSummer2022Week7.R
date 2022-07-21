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
# https://www.youtube.com/watch?v=vemZtEM63GY
#
# We will:
# - Review standard work-horse hypothesis tests
# - Discuss the assumptions made by these tests
# - Understand the consequences of violating test assumptions
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# First, as usual, we will load the libraries we need
packages<-c("tidyverse", "cowplot", "ggpubr")
install.packages(setdiff(packages, rownames(installed.packages())))

library(tidyverse)
library(cowplot)
library(ggpubr)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#           One-sample tests: t-test and one-sample Wilcoxon
# 
# In a one-sample test, we are asking whether a sample is consistent with some hypothesis
# usually expressed as something like "is the mean X?"
#
# Here, we’ll use an example data set containing the weight of 10 mice.
# We want to know if the average weight of the mice differs from 25g?

set.seed(1234)
my_data <- data.frame(
  name = paste0(rep("M_", 10), 1:10),
  weight = round(rnorm(10, 20, 2), 1)
)

# Print the first 10 rows of the data
head(my_data, 10)

# Statistical summaries of weight
summary(my_data$weight)

# Let's throw up a quick boxplot
ggboxplot(my_data$weight, 
          ylab = "Weight (g)", xlab = FALSE,
          ggtheme = theme_minimal())

# and a histogram
gghistogram(my_data$weight, 
            bins=5,
            xlab = "Weight (g)", ylab = "Frequency",
            ggtheme = theme_classic())


# What test should we use?
# If the data are normally distributed, we can use a t-test
# They should be (because we made them that way)
# but let's make sure
# using the Shapiro-Wilk test (from week 5)
shapiro.test(my_data$weight)

# and visually with QQ plot
ggqqplot(my_data$weight, ylab = "Weight",
         ggtheme = theme_minimal())

# OK. so we can use a t-test here. let's do it!
# Recall we want to know whether the average weight is different than 25g

# One-sample t-test
res <- t.test(my_data$weight, mu = 25)

# Printing the results
res 

# In the result above :
#  t is the t-test statistic value (t = -9.078),
# df is the degrees of freedom (df= 9),
# p-value is the significance level of the t-test (p-value = 7.95310^{-6}).
# conf.int is the confidence interval of the mean at 95% (conf.int = [17.8172, 20.6828]);
# sample estimates is he mean value of the sample (mean = 19.25).

# Significance is determined for a t-test based on the p-value
# print the p-value only
res$p.value


# What if we wanted to know whether the mean was LESS THAN 25g (one-tailed test?)
t.test(my_data$weight, mu = 25,
       alternative = "less")

# or GREATER THAN 25g (other tail?)
t.test(my_data$weight, mu = 25,
       alternative = "greater")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# but what if the data weren't normal?
# In this case (as we have seen) the mean isn't very informative
# The Wilcoxon rank sum test for one sample can be interpreted as asking instead
# Is the median of the data equal to some theoretical value X?

# Let's generate some new data
set.seed(1234)
my_data <- data.frame(
  name = paste0(rep("M_", 10), 1:10),
  weight = round(rlnorm(10, log(20), log(2)), 1)
)

# Print the first 10 rows of the data
head(my_data, 10)

# Statistical summaries of weight
summary(my_data$weight)

# and a box plot
ggboxplot(my_data$weight, 
          ylab = "Weight (g)", xlab = FALSE,
          ggtheme = theme_minimal())

# and a histogram
gghistogram(my_data$weight, 
            bins=5,
            xlab = "Weight (g)", ylab = "Frequency",
            ggtheme = theme_classic())

# now the log data
gghistogram(log(my_data$weight), fill="lightgray", 
            bins=5,
          ylab = "Weight (g)", xlab = FALSE,
          ggtheme = theme_minimal())

# is it normal?
# using the Shapiro-Wilk test (from week 5)
shapiro.test(my_data$weight)

# and visually with QQ plot
ggqqplot(my_data$weight, ylab = "Weight",
         ggtheme = theme_minimal())

# The Wilcoxon test is nonparametric - 
# by converting the data into ranks, we lose the need to assume a distribution.

# Does the median of weights differ from 25g?
# One-sample wilcoxon test
res <- wilcox.test(my_data$weight, mu = 25)

# Printing the results
res 

# print only the p-value
res$p.value

# As before, we have the option to do a one-tailed test
wilcox.test(my_data$weight, mu = 25,
            alternative = "less")
wilcox.test(my_data$weight, mu = 25,
            alternative = "greater")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Why is this necessary?
# In this case, because the mean of a skewed distribution (the lognormal)...
# kinda doesn't matter.
# Sometimes the test is the wrong choice because it's asking the wrong question.
#
# Also, as we saw last week, the mean of a lognormal is badly behaved, 
# especially at small sample sizes. 
# If the mean doesn't behave as normal (which it won't), 
# the performance of the t-test will be erratic.
# Let's see that happen.

# Here we will generate a bunch of simulated data sets and run t-tests,
# then see how often we get p<0.05 (standard alpha for significance)
# when *we know that the null hypothesis is true*

reps<-1000 # how many simulations we will run
sim_pvals<-tibble(t_n=numeric(reps),
                  w_n=numeric(reps),
                  t_ln=numeric(reps),
                  w_ln=numeric(reps))# someplace to put the data

set.seed(1234)  # seed the random number generator
mu<-25 # and establish the parameters of the distributions
sdev<-2

# and run the simulation
for (i in seq_len(reps)){
  # normal
  weight = round(rnorm(10, mu, sdev), 1)
  ttest_n<-t.test(weight, mu=25)
  wtest_n<-wilcox.test(weight, mu=25, exact=FALSE)
  sim_pvals$t_n[i]<-ttest_n$p.value
  sim_pvals$w_n[i]<-wtest_n$p.value
  # lognormal
  weight = round(rlnorm(10, log(mu), log(sdev)), 1)
  ttest_ln<-t.test(weight, mu=25)
  wtest_ln<-wilcox.test(weight, mu=25, exact=FALSE)
  sim_pvals$t_ln[i]<-ttest_ln$p.value
  sim_pvals$w_ln[i]<-wtest_ln$p.value
}

# let's see what we have
glimpse(sim_pvals)

# plot it all out
# Look at the shape of the distributions. What has happened?
sim_pvals %>%
  pivot_longer(everything(), names_to="test", values_to="p") %>%
  ggplot(aes(x=p))+
  geom_histogram(color="black", fill="lightgray", binwidth=0.05)+
  geom_vline(xintercept=0.05, color="red")+
  geom_vline(xintercept=0.1, color="blue")+
  facet_wrap(~factor(test))

# How many false positives should we get?
length(which(sim_pvals$t_n<0.05))
length(which(sim_pvals$t_n<0.05))/reps
length(which(sim_pvals$w_n<0.05))
length(which(sim_pvals$w_n<0.05))/reps

length(which(sim_pvals$t_n<0.1))
length(which(sim_pvals$t_n<0.1))/reps
length(which(sim_pvals$w_n<0.1))
length(which(sim_pvals$w_n<0.1))/reps

# How many do we get with the lognormal?
# How many false positives should we get at each level?
length(which(sim_pvals$t_ln<0.05))
length(which(sim_pvals$t_ln<0.05))/reps
length(which(sim_pvals$w_ln<0.05))
length(which(sim_pvals$w_ln<0.05))/reps

length(which(sim_pvals$t_ln<0.1))
length(which(sim_pvals$t_ln<0.1))/reps
length(which(sim_pvals$w_ln<0.1))
length(which(sim_pvals$w_ln<0.1))/reps

# These distributions are fairly compact (low SD) - what happens if that changes?
# Change the parameter and run again. What do you observe?

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Comparing the means of two groups: Unpaired samples
# 
# The unpaired two-samples t-test is used to compare the mean of two independent groups. 
# For example, suppose that we have measured the weight of 100 individuals: 50 women (group A) and 50 men (group B). 
# We want to know if the mean weight of women (mA) is significantly different from that of men (mB).
#In this case, we have two unrelated (i.e., independent or unpaired) groups of samples. 
# Therefore, it’s possible to use an independent t-test to evaluate whether the means are different.
# IF AND ONLY IF
#   the two groups of samples (A and B), being compared, are normally distributed. NOT NEGOTIABLE
#   and the variances of the two groups are equal. This can be checked using F-test. 
#
# (If the second assumption isn't met, we can use Welch's t-test for unequal variances.)

# Here, we’ll use an example data set, which contains the weight of 18 individuals (9 women and 9 men):
# Data in two numeric vectors
women_weight <- c(38.9, 61.2, 73.3, 21.8, 63.4, 64.6, 48.4, 48.8, 48.5)
men_weight <- c(67.8, 60, 63.4, 76, 89.4, 73.3, 67.3, 61.3, 62.4) 
# Create a data frame
my_data <- data.frame( 
  group = rep(c("Woman", "Man"), each = 9),
  weight = c(women_weight,  men_weight)
)

# We want to know, if the average women’s weight differs from the average men’s weight?
group_by(my_data, group) %>%
  summarise(
    count = n(),
    mean = mean(weight, na.rm = TRUE),
    sd = sd(weight, na.rm = TRUE)
  )

# Plot weight by group and color by group
ggboxplot(my_data, x = "group", y = "weight", 
          color = "group", palette = c("#00AFBB", "#E7B800"),
          ylab = "Weight", xlab = "Groups")

############    ASSUMPTION 1; Normality
# Use Shapiro-Wilk normality test  
# Null hypothesis: the data are normally distributed
# Alternative hypothesis: the data are not normally distributed

# Shapiro-Wilk normality test for Men's weights
with(my_data, shapiro.test(weight[group == "Man"]))

# Shapiro-Wilk normality test for Women's weights
with(my_data, shapiro.test(weight[group == "Woman"]))
 
######### ASSUMPTION 2: EQUAL VARIANCES
# We’ll use F-test to test for homogeneity in variances. 
# This can be performed with the function var.test()
res.ftest <- var.test(weight ~ group, data = my_data)
res.ftest

# The p-value of F-test is p = 0.1713596. 
# It’s greater than the significance level alpha = 0.05. 
# In conclusion, there is no significant difference between the variances of the two sets of data. 
# Therefore, we can use the classic t-test witch assume equality of the two variances.

# Compute t-test
res <- t.test(weight ~ group, data = my_data, var.equal = TRUE)
res

# If we want to know whether the average men's weight is less or greather than the average woman's:
t.test(weight ~ group, data = my_data,
       var.equal = TRUE, alternative = "less")
t.test(weight ~ group, data = my_data,
       var.equal = TRUE, alternative = "greater")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# But what if the assumptions are violated?
# The unpaired two-samples Wilcoxon test (also known as Wilcoxon rank sum test or Mann-Whitney test) 
# is a non-parametric alternative to the unpaired two-samples t-test, which can be used to compare two independent groups of samples. 
# It’s used when your data are not normally distributed. 
# They still have to be independent, though.

women_weight <- round(rlnorm(9, log(52), log(15)), 1)
men_weight <- round(rlnorm(9, log(69), log(10)), 1) 
# Create a data frame
my_data <- data.frame( 
  group = rep(c("Woman", "Man"), each = 9),
  weight = c(women_weight,  men_weight)
)

# We want to know, if the average women’s weight differs from the average men’s weight?
group_by(my_data, group) %>%
  summarise(
    count = n(),
    mean = mean(weight, na.rm = TRUE),
    median=median(weight, na.rm=TRUE),
    sd = sd(weight, na.rm = TRUE)
  )

# Plot weight by group and color by group
ggboxplot(my_data, x = "group", y = "weight", 
          color = "group", palette = c("#00AFBB", "#E7B800"),
          ylab = "Weight", xlab = "Groups")

# Test for normality and equal variances
with(my_data, shapiro.test(weight[group == "Man"]))
with(my_data, shapiro.test(weight[group == "Woman"]))
res.ftest <- var.test(weight ~ group, data = my_data)
res.ftest

# Can we use the t-test?
res <- wilcox.test(weight ~ group, data = my_data,
                   exact = FALSE)
res

# There are one-sided tests here as well, with the same syntax
wilcox.test(weight ~ group, data = my_data, 
            exact = FALSE, alternative = "less")
wilcox.test(weight ~ group, data = my_data,
            exact = FALSE, alternative = "greater")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Again, what happens if we violate the assumptions of the paired test?
# We saw a hint about this last time, but let's see it here
# using a similar simulation as for the one-sample tests

reps<-1000 # how many simulations we will run
sim_pvals<-tibble(t_n=numeric(reps),
                  w_n=numeric(reps),
                  t_ln=numeric(reps),
                  w_ln=numeric(reps))# someplace to put the data

set.seed(1234)  # seed the random number generator
mu<-52 # and establish the parameters of the distributions
sdev<-15

# and run the simulation
for (i in seq_len(reps)){
  # normal
  weight1 = round(rnorm(10, mu, sdev), 1)
  weight2 = round(rnorm(10, mu, sdev), 1)
  ttest_n<-t.test(weight1, weight2)
  wtest_n<-wilcox.test(weight1, weight2, exact=FALSE)
  sim_pvals$t_n[i]<-ttest_n$p.value
  sim_pvals$w_n[i]<-wtest_n$p.value
  # lognormal
  weight1 = round(rlnorm(10, log(mu), log(sdev)), 1)
  weight2 = round(rlnorm(10, log(mu), log(sdev)), 1)
  ttest_ln<-t.test(weight1, weight2)
  wtest_ln<-wilcox.test(weight1, weight2, exact=FALSE)
  sim_pvals$t_ln[i]<-ttest_ln$p.value
  sim_pvals$w_ln[i]<-wtest_ln$p.value
}

# let's see what we have
glimpse(sim_pvals)

# plot it all out
# Look at the shape of the distributions. What has happened?
sim_pvals %>%
  pivot_longer(everything(), names_to="test", values_to="p") %>%
  ggplot(aes(x=p))+
  geom_histogram(color="black", fill="lightgray", binwidth=0.05)+
  geom_vline(xintercept=0.05, color="red")+
  geom_vline(xintercept=0.1, color="blue")+
  facet_wrap(~factor(test))

# How many false positives should we get?
length(which(sim_pvals$t_n<0.05))
length(which(sim_pvals$t_n<0.05))/reps
length(which(sim_pvals$w_n<0.05))
length(which(sim_pvals$w_n<0.05))/reps

length(which(sim_pvals$t_n<0.1))
length(which(sim_pvals$t_n<0.1))/reps
length(which(sim_pvals$w_n<0.1))
length(which(sim_pvals$w_n<0.1))/reps

# How many do we get with the lognormal?
# How many false positives should we get at each level?
length(which(sim_pvals$t_ln<0.05))
length(which(sim_pvals$t_ln<0.05))/reps
length(which(sim_pvals$w_ln<0.05))
length(which(sim_pvals$w_ln<0.05))/reps

length(which(sim_pvals$t_ln<0.1))
length(which(sim_pvals$t_ln<0.1))/reps
length(which(sim_pvals$w_ln<0.1))
length(which(sim_pvals$w_ln<0.1))/reps

