# Vega Lab Meeting Week 9
# This week we will continue our discussion of type I and II errors,
# mostly in the context of two-sample comparisons.
# Relevant background is here:
#   https://www.khanacademy.org/math/statistics-probability/significance-tests-confidence-intervals-two-samples#comparing-two-proportions
#   https://www.khanacademy.org/math/statistics-probability/significance-tests-one-sample/tests-about-population-proportion/v/constructing-hypotheses-for-a-significance-test
#   https://www.statmethods.net/stats/power.html
#   https://www.r-bloggers.com/2021/05/power-analysis-in-statistics-with-r/
#   https://ladal.edu.au/pwr.html
#   https://en.wikipedia.org/wiki/Effect_size#Cohen's_d
#   https://www.intro2r.info/unit3/swirl/Testing_Ratios
#   https://data-flair.training/blogs/chi-square-test-in-r/

# As usual we will load the required packages:
pacman::p_load(tidyverse, effectsize, pwr, BiodiversityR, ggsci, ggrepel, ggforce, ggpubr)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Your assignment for this week was:
# For at least one of the pairwise comparisons we performed in week 7, 
# calculate the estimated effect size (package effectsize will help you) 
# and power to find an effect of that size or greater (package pwr has these functions). 
# At this effect size, how many data points would be needed for a power of 0.2? 0.8? 
# Be prepared to explain the results. I have attached a script with example code

# You were given a pre-meeting script, with starter code:
# Generate two sets of made-up data (A and B) in a data frame
set.seed(1234)
my_data <- data.frame(
  group = rep(c("A", "B"), each=10), 
  values = c(round(rnorm(10, 20, 2), 1), round(rnorm(10, 22, 2), 1))
)

# Take a quick look
glimpse(my_data)

# And generate some summary statistics
my_data %>%
  group_by(group) %>%
  summarise(mean=mean(values),
            median=median(values),
            sd=sd(values))

# let's say I want to do a t-test.
# First I want to know the effect size:
TT<-t.test(values~group, data=my_data, var.equal=TRUE)
TT
effectsize(TT)

# Note that "d", the effect size here, is Cohen's D,
# calculated as (mu1-mu2)/s, where s is the pooled standard deviation.
# Cohen suggests that d values of 0.2, 0.5, and 0.8 represent small, medium, and large effect sizes respectively. 

# and then the power:
pwr.t.test(d=1.28, n=10, sig.level = 0.05, alternative="two.sided")

# If we drop the number of data points or decrease the effect size, power drops: 
pwr.t.test(d=1.28, n=5, sig.level = 0.05, alternative="two.sided")
pwr.t.test(d=0.54, n=10, sig.level = 0.05, alternative="two.sided")

# And if we increase n or effect size, the reverse occurs:
pwr.t.test(d=1.55, n=10, sig.level = 0.05, alternative="two.sided")
pwr.t.test(d=0.8, n=25, sig.level = 0.05, alternative="two.sided")

# The pwr package contains some other built-in power analysis functions,
# but you will note there is nothing for the Mann-Whitney U test (nonparametric).
# This is because parametric power analysis assumes the distribution of the data (normal, for a t-test).
# For non-parametric tests, power has to be estimated from the preliminary data.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pwr.MW.test.boot<-function(my_data_1, my_data_2, sig=0.05, reps=1000){
  # A function to generate a bootstrap estimate of power for the Mann-Whitney test for unpaired data.
  # Requires two vectors of numeric data (do not need to be the same size)
  # By default, will assume significance level sig=0.05
  # and carry out 1000 bootstrap replicates
  # Requires:
  pacman::p_load(effectsize)
  
  # Make objects to hold the results
  my_pvals<-numeric(length=reps)
  alt_pvals<-numeric(length=reps)

  
  # Extract parameters for simulation
  capp1<-length(my_data_1)
  capp2<-length(my_data_2)
  capp<-round((capp1+capp2/2))
  
  # Create a combined data object
  new_data<-c(my_data_1, my_data_2)
  

  # Loop
  for (i in seq_along(reps)){

    # Generate new data sets by resampling
    new_data_1<-sample(my_data_1,capp1,replace=TRUE)
    new_data_2<-sample(my_data_2,capp2,replace=TRUE)
    wtest<-wilcox.test(new_data_1, new_data_2, exact=FALSE)

    my_pvals[i]<-wtest$p.value
    # and for the combined data
    new_data_1<-sample(new_data,capp,replace=TRUE)
    new_data_2<-sample(new_data,capp,replace=TRUE)
    wtest<-wilcox.test(new_data_1, new_data_2, exact=FALSE)
    alt_pvals[[i]]<-wtest$p.value
  }
  ncp<-(length(which(my_pvals<=sig))-length(which(alt_pvals<=sig)))/reps
 # ncp<-(length(which(my_pvals<=sig)))/reps

  d<-cohens_d(my_data_1, my_data_2)$Cohens_d
  structure(list(n1=capp1, n2=capp2, d=d, pwr=ncp))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Let's pull apart the existing data frame
#my_data_1<-my_data %>%
 # as_tibble() %>%
  #filter(group == "A") %>%
 # select(values) %>%
 # t() %>%
 # as.numeric()
#my_data_1
#my_data_2<-my_data %>%
#  filter(group == "B") %>%
#  select(values) %>%
#  t()%>%
#  as.numeric()

my_data_1<-round(rnorm(10, 20, 2), 1)
my_data_2<-round(rnorm(10, 22, 2), 1)

# Now we can call the new function
pwr.MW.test.boot(my_data_1 = my_data_1, my_data_2 = my_data_2)

# What if we reduce the effect size?
# What can we say about power of the parametric vs. non-parametric test here?
my_data_1<-round(rnorm(10, 20, 2), 1)
my_data_2<-round(rnorm(10, 21, 2), 1)

mean(my_data_1)
median(my_data_1)
mean(my_data_2)
median(my_data_2)

MWpwr2<-pwr.MW.test.boot(my_data_1 = my_data_1, my_data_2 = my_data_2)
MWpwr2
pwr.t.test(d=MWpwr2$d, n=MWpwr2$n1, sig.level = 0.05, alternative="two.sided")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#             PROPORTIONAL DATA
# Last week, we discussed the idea that proportions are a special kind of data.
# The problems with proportions are two-fold:
# they are bounded (must be between zero and one)
# and, if we don't have the counts they are based on, they are compositional (relative rather than absolute).
#
# Let's look at the squash bug data again.
#I have some percent composition data from an experiment in which 9 bugs were successfully infected
#with a 3:1 ratio of GFP to RFP labelled symbionts.
low=c(0,0,0.59,0.12,0.98,0.95,0.96,1,0.99)

#The question I want to test is if the average %composition of GFP symbionts is different from 25% (0.25).
#I might want to do this because I'm looking to see if two symbionts exhibit competitive dynamics during
#infection, with the assumption that the GFP symbiont is the superior competitor.
comps <- data.frame(
  name = paste0(rep("M_", 9), 1:9),
  percentGFP = low
)

#Let's examine our data.
# Statistical summaries of weight
summary(comps$percentGFP)

# and a histogram
gghistogram(comps$percentGFP, 
            bins=10,
            xlab = "Percent GFP", ylab = "Frequency",
            ggtheme = theme_classic())

# These data are severely censored;
# that is, there are hard boundaries at 0 and 1.
# This is always true for proportional data.
# Sometimes these hard boundaries aren't very important. For example, take a fair coin toss:

?rbinom
data_binom<-rbinom(1000, 10, 0.5)/10 #Fraction of successes, out of 10 trials; 1000 times; p=0.5

# How do these data look?
gghistogram(data_binom, fill="blue", bins=25, xlim=c(0,1))

# Are they normally distributed? Why?
shapiro.test(data_binom)

# How can we determine if the coin is fair (probability of a success is 0.5)?
# We have only two outcomes; for one trial, this is a BINOMIAL TEST.
?binom.test
data_binom[1]
binom.test(data_binom[1]*10, 10)

# We can use instead a PROPORTIONAL Z-TEST (implemented as prop.test()) when n>30,
# since at this sample size the binomial starts approximating the normal:
data_binom<-rbinom(1000, 10, 0.5)/10 #Fraction of successes, out of 10 trials; 1000 times; p=0.5
p1<-gghistogram(data_binom, fill="blue", bins=25, xlim=c(0,1), main="10 trials")
shapiro.test(data_binom)
data_binom<-rbinom(1000, 30, 0.5)/30 #Fraction of successes, out of 30 trials; 1000 times; p=0.5
p2<-gghistogram(data_binom, fill="blue", bins=25, xlim=c(0,1), main="30 trials")
shapiro.test(data_binom)
data_binom<-rbinom(1000, 50, 0.5)/50 #Fraction of successes, out of 50 trials; 1000 times; p=0.5
p3<-gghistogram(data_binom, fill="blue", bins=25, xlim=c(0,1), main="50 trials")
shapiro.test(data_binom)
data_binom<-rbinom(1000, 100, 0.5)/100 #Fraction of successes, out of 100 trials; 1000 times; p=0.5
p4<-gghistogram(data_binom, fill="blue", bins=50, xlim=c(0,1), main="100 trials")
shapiro.test(data_binom)
plot_grid(p1, p2, p3, p4)

# Essentially, if the data aren't too badly censored, 
# you probably won't break a test designed for count-based numeric data.
# If there are zero/one values at any appreciable frequency,
# standard parametric tests won't apply, and Mann-Whitney will lose its mind over too many ties.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# What about distributions of proportional data over some variable or gradient?
# In a PROPORTIONAL MODEL, the frequency of occurrence of events is proportional to the number of opportunities.
# The relationship between observed frequencies and expectations from such a model
# is assessed using a CHI-SQUARED GOODNESS OF FIT test.
# Let's say we have data on births in a given year, and we want to know if it is equally likely
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

# As usual, this test requires assuming that we have a random sample from a population
# with all individuals independent and identically distributed.
# The chi-squared test can be used when:
# - None of the categories have an expected frequency <1
# - Not more than 20% of the categories have expected frequency 5.
# If these requirements are not met, it may be possible to combine groups IF they can be reasonably combine
# (this will take away some degrees of freedom accordingly)
# or find an alternate test.

# Since this is a parametric test, there is a power analysis for it:
?pwr.chisq.test
effectsize(CT)
pwr.chisq.test(w=0.08, df=6, N=350)
