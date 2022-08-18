# Vega lab meeting, summer 2022, week 11
# This week we will discuss the mysteries of where a hypothesis test comes from,
# what a test statistic is, and how it is turned into a p-value
# as we unpack the normal distribution, the chi-squared, and the F test
# so that we can discuss ANOVA and other partitions of variance.
#
# Background information is from:
#   https://www.khanacademy.org/math/statistics-probability/inference-categorical-data-chi-square-tests#chi-square-goodness-of-fit-tests
#   https://crumplab.com/statistics/07-ANOVA.html
#
# with code etc from:
#   https://www.statmethods.net/graphs/density.html
#   http://www.cookbook-r.com/Graphs/Plotting_distributions_(ggplot2)/
#   http://www.sthda.com/english/wiki/ggplot2-density-plot-quick-start-guide-r-software-and-data-visualization

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# As usual we will load the required packages:
pacman::p_load(tidyverse, effectsize, pwr, cowplot, ggpubr)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# When we left off, we were talking about where test statistics come from
# in the kinds of standard, frequentist hypothesis tests we have been discussing.
# You were given some assignments; let's discuss.
# 
# During lab meeting, we said that we could find the critical values associated with 
# alpha=0.05 for Z and t-tests, but we left this as an exercise. 
# Assuming that your test statistic under the null hypothesis is distributed as a 
# standard normal N(0,1), find the critical values associated with one-tailed (upper and lower) 
# and two-tailed testing using alpha = 0.05 and alpha = 0.1. 
# Do the same for a Student's T distribution with some reasonable number of 
# degrees of freedom (your choice).

# Recall that, for the normal distribution,
# the function dnorm() gives the density of the probability distribution function (pdf) at a given value of x,
# the function pnorm() gives the cumulative probability density function (cdf),
# and qnorm() gives the quantile function.
# If we don't specify, these functions assume a standard normal - N(0,1).

#This is what the N(0,1) looks like:
pNormal<-
  ggplot(data.frame(x = c(-4, 4)), aes(x = x)) +
  stat_function(fun = dnorm, args=list(mean=0, sd=1))+
  theme_classic()+
  theme(text=element_text(size=16), 
        plot.title=element_text(hjust=0.5, size=16)) + 
  labs(title="N(0,1) pdf")
pNormal

# Recall that lower.tail is a logical in pnorm() and qnorm(); 
# if TRUE (default), probabilities are P[X ≤ x] otherwise, P[X > x].
# Note the values we are using here. What is this equivalent to?
x1<-qnorm(0.025)
x1
x2<-qnorm(0.025, lower.tail = FALSE)
x2
# Sanity check:
pnorm(x1)
pnorm(x2, lower.tail = FALSE)

# If instead we wanted the critical values for a Z-test at p=0.1
qnorm(0.05)
qnorm(0.05, lower.tail = FALSE)

# How many SD around the mean is this? Well, since SD=1,
abs(qnorm(0.05))+abs(qnorm(0.05, lower.tail = FALSE))
abs(qnorm(0.025))+abs(qnorm(0.025, lower.tail = FALSE))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# We can do the same for the Student's T with, say, 10 df.
?dt

pT<-
  ggplot(data.frame(x = c(-4, 4)), aes(x = x)) +
  stat_function(fun = dt, args=list(df=10))+
  theme_classic()+
  theme(text=element_text(size=16), 
        plot.title=element_text(hjust=0.5, size=16)) + 
  labs(title="T(df=10) pdf")
pT

x1t<-qt(0.025, df=10)
x1t
x2t<-qt(0.025, df=10, lower.tail = FALSE)
x2t
# Sanity check:
pt(x1t, df=10)
pt(x2t, df=10, lower.tail = FALSE)

# If instead we wanted the critical values for a t-test at p=0.1
qt(0.05, df=10)
qt(0.05, df=10, lower.tail = FALSE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#             THE CHI-SQUARED DISTRIBUTION
#         AND CHI-SQUARED HYPOTHESIS TESTING
#
# For your homework, you were asked to demonstrate empirically that the sum
# of n N(0,1)^2 random variables is a chi-squared with n degrees of freedom.

# let's start by generating values from a normal N(0,1).
?rnorm
x1 <- rnorm(1000)

# Plot these out as density:
d1<-density(x1)
plot(d1, main="N(0,1), n=1000")
polygon(d1, col="red", border="grey", main="N(0,1), n=1000")

# or in ggplot
data.frame(x1) %>%
  ggplot(aes(x=x1))+
  geom_density(color="grey", fill="red")+
  geom_vline(aes(xintercept=mean(x1)),
             color="black", linetype="dashed", size=1)+
  theme_classic()+
  theme(text=element_text(size=16), 
        plot.title=element_text(hjust=0.5, size=16)) + 
  labs(title="N(0,1), n=1000")

# Now let's square them.
c1 <- x1*x1
x1[1:5]
c1[1:5]
# Sanity check:
x1[1]
x1[1]*x1[1]

# This should be the same as a chi-squared distribution with df=1
ggplot(data.frame(x = c(0, 10)), aes(x = x)) +
  stat_function(fun = dchisq, args=list(df=1), aes(colour="1"))+
  stat_function(fun = dchisq, args=list(df=2), aes(colour="2"))+
  stat_function(fun = dchisq, args=list(df=5), aes(colour="5"))+
  stat_function(fun = dchisq, args=list(df=9), aes(colour="9"))+
  theme_classic()+
  theme(text=element_text(size=16), 
        plot.title=element_text(hjust=0.5, size=16)) + 
  scale_color_manual("df", values = c("blue", "darkgreen", "red", "purple")) +
  labs(title="Chisq pdf")

# Let's plot.
chi1<-rchisq(1000, df=1)
mychi1<-tibble(data=c(c1, chi1),
               distr=c(rep("N", 1000), rep("Chi", 1000)),
               df=rep(1, 2000))
glimpse(mychi1)

# And plot
mychi1 %>%
  ggplot(aes(x=data, colour=distr))+
  geom_density()+
  theme_classic()+
  theme(text=element_text(size=16), 
        plot.title=element_text(hjust=0.5, size=16),
        legend.title = element_blank()) + 
  labs(title="N(0,1)^2 vs Chisq(1), n=1000")

# Cool. How about adding some df?
x2 <- rnorm(1000)
c2 <- (x1*x1) + (x2*x2)
chi2<-rchisq(1000, df=2)
mychi2<-tibble(data=c(c2, chi2),
               distr=c(rep("N2", 1000), rep("C2", 1000)),
               df=rep(2, 2000))
glimpse(mychi2)

# And plot
mychi2 %>%
  ggplot(aes(x=data, colour=distr))+
  geom_density()+
  theme_classic()+
  theme(text=element_text(size=16), 
        plot.title=element_text(hjust=0.5, size=16),
        legend.title = element_blank()) + 
  labs(title="sum(N(0,1)^2) vs Chisq(2), n=1000")

# Some more?
x3 <- rnorm(1000)
x4 <- rnorm(1000)
x5 <- rnorm(1000)
c5 <- (x1*x1) + (x2*x2) + (x3*x3) + (x4*x4) + (x5*x5)
chi5<-rchisq(1000, df=5)
mychi5<-tibble(data=c(c5, chi5),
               distr=c(rep("N5", 1000), rep("C5", 1000)),
               df=rep(5, 2000))
x6 <- rnorm(1000)
x7 <- rnorm(1000)
x8 <- rnorm(1000)
x9 <- rnorm(1000)
c9<- c5 + (x6*x6) + (x7*x7) + (x8*x8) + (x9+x9)
chi9<-rchisq(1000, df=9)
mychi9<-tibble(data=c(c9, chi9),
               distr=c(rep("N9", 1000), rep("C9", 1000)),
               df=rep(9, 2000))

# Combine and plot
mychi_all<-rbind(mychi1, mychi2,  mychi5, mychi9)
mychi_all %>%
  ggplot(aes(x=data, colour=distr))+
  geom_density()+
  theme_classic()+
  theme(text=element_text(size=16), 
        plot.title=element_text(hjust=0.5, size=16),
        legend.title = element_blank()) + 
  labs(title="sum(N(0,1)^2) vs Chisq(), n=1000")+
  facet_wrap(vars(df), ncol=2)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#         CHI-SQUARED GOODNESS OF FIT TEST
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The relationship between observed frequencies and expectations
# when numerical observations are divided over a number of categories
# is assessed using a CHI-SQUARED GOODNESS OF FIT test.
# Let's say we have data on births in a given year, 
# and we want to know if it is equally likely
# that a birth will occur on any given day of the week.

births<-data.frame(Day=c("Sun", "Mon", "Tues", "Wed", "Thurs", "Fri", "Sat"),
                   Number=c(33,41,63,63,47,56,47))
births
sum(births$Number)

# For this test, we have
# H0: The probability of birth is the same on every day of the week
# Ha: The probability of birth is NOT the same on every day of the week

# The chi-squared statistic is the sum of (observed-expected)^2/expected
# Under H0, what is the expectation?
# In the year in question, each day occurred 52 times, except Friday which occurred 53 times.
# The expected number of births is therefore (on every day but Friday)
(52/365)*350

births$p<-c((52/365), (52/365), (52/365), (52/365), (52/365), (53/365), (52/365))
births<-births %>%
  mutate(Expected=350*p)
births

# The chi-squared statistic measures the discrepancy between observation and expectation.
# We can calculate the statistic for ourselves:
# (Note that the calculation uses the absolute frequencies (counts) and not the relative frequencies-
# using proportions will give the wrong answer.)
births<-births %>% 
  mutate(chisq_addend = (Number-Expected)^2/Expected)
sum(births$chisq_addend)
CT<-chisq.test(births$Number, p=births$p)
CT

# Sanity check
qchisq(0.01982, df=6, lower.tail = FALSE)
?pchisq

ggplot(data.frame(x = c(0, 30)), aes(x = x)) +
  stat_function(fun = dchisq, args=list(df=6))+
  geom_vline(xintercept=15.0567, color="red")+
  theme_classic()+
  theme(text=element_text(size=16), 
        plot.title=element_text(hjust=0.5, size=16)) + 
  scale_color_manual("df", values = c("blue", "darkgreen", "red", "purple")) +
  labs(title="Chisq(df=6) pdf")

# But why is sum((o-e)^2/e) distributed as chi-squared?
# Let's go to the board and find out.

# As usual, this test requires assuming that we have a random sample from a population
# with all individuals independent and identically distributed.
# The data themselves don't have to be normal!
# But the residuals do.
#
# The chi-squared test can be used when:
# - None of the categories have an expected frequency <1
# - Not more than 20% of the categories have expected frequency <5.
# If these requirements are not met, it may be possible to combine groups 
#        IF they can be reasonably combined
#       (this will take away some degrees of freedom accordingly)
# or find an alternate test.

# Since this is a parametric test, there is a power analysis for it:
?pwr.chisq.test
effectsize(CT)
pwr.chisq.test(w=0.08, df=6, N=350)

