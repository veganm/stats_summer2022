# Vega lab meeting, summer 2022, week 10
# This week we will discuss the mysteries of where a hypothesis test comes from,
# what a test statistic is, and how it is turned into a p-value
# as we unpack the normal distribution, the chi-squared, and the F test
# so that we can discuss ANOVA and other partitions of variance.
# Background information is from:
#   https://www.khanacademy.org/math/statistics-probability/modeling-distributions-of-data#z-scores



# As usual we will load the required packages:
pacman::p_load(tidyverse, effectsize, pwr, cowplot, ggpubr)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#              N(0,1) AND THE Z-TEST
# Recall back in week 5, when we learned to generate and plot out a normal distribution.
# We'll start with that code to learn how a Z test is constructed.

shared_mean<-5 # our data will have the indicated mean
shared_sd<-2 # and variance

# For the normal, start by creating a sequence of values based on population mean and standard deviation
# First we will create the x-axis
c(-4, 4)
c(-4, 4)*shared_sd+shared_mean # what's happening here? hmm.

# then generate & draw simulated data from the normal distribution using 
?stat_function()

pNormal<-
  ggplot(data.frame(x = c(-4, 4)*shared_sd+shared_mean), aes(x = x)) +
  stat_function(fun = dnorm, args=list(mean=shared_mean, sd=shared_sd))+
  theme_classic()+
  theme(text=element_text(size=16), 
        plot.title=element_text(hjust=0.5, size=16)) + 
  labs(title="Normal")
pNormal

# The function pnorm() gives the distribution function for a normal.
# Let's see what this means. 
# Note that lower.tail is a logical; 
# if TRUE (default), probabilities are P[X ≤ x] otherwise, P[X > x].
pnorm(5, mean=shared_mean, sd=shared_sd)
pnorm(5, mean=shared_mean, sd=shared_sd, lower.tail = FALSE)
qnorm(0.5, mean=shared_mean, sd=shared_sd)
qnorm(0.5, mean=shared_mean, sd=shared_sd, lower.tail = FALSE)

x1<-qnorm(0.025, mean=shared_mean, sd=shared_sd)
x1
x2<-qnorm(0.025, mean=shared_mean, sd=shared_sd, lower.tail = FALSE)
x2
pnorm(x1, mean=shared_mean, sd=shared_sd)
pnorm(x2, mean=shared_mean, sd=shared_sd, lower.tail = FALSE)

qnorm(0.25, mean=shared_mean, sd=shared_sd)
qnorm(0.75, mean=shared_mean, sd=shared_sd)

pnorm(shared_mean-shared_sd, mean=shared_mean, sd=shared_sd)
pnorm(shared_mean+shared_sd, mean=shared_mean, sd=shared_sd)
pnorm(shared_mean+0.5*shared_sd, mean=shared_mean, sd=shared_sd) - pnorm(shared_mean-0.5*shared_sd, mean=shared_mean, sd=shared_sd)
pnorm(shared_mean+1*shared_sd, mean=shared_mean, sd=shared_sd) - pnorm(shared_mean-1*shared_sd, mean=shared_mean, sd=shared_sd)
pnorm(shared_mean+1.5*shared_sd, mean=shared_mean, sd=shared_sd) - pnorm(shared_mean-1.5*shared_sd, mean=shared_mean, sd=shared_sd)

# Let's visualize:
pNormal + 
  geom_vline(xintercept=x1, color="red") +
  geom_vline(xintercept=x2, color="red")

# We've glossed over something interesting.
# We rescaled the interval in a very particular way using the shared mean and SD.
# This is because the normal distribution 
