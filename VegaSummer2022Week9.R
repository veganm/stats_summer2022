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

# As usual we will load the required packages:
pacman::p_load(tidyverse, effectsize, pwr, BiodiversityR, ggsci, ggrepel, ggforce, readxl)

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

# Note that "d", the effect size, is Cohen's D,
# calculated as |mu1-mu2|/s, where s is the pooled standard deviation.
# Cohen suggests that d values of 0.2, 0.5, and 0.8 represent small, medium, and large effect sizes respectively. 

# and then the power:
pwr.t.test(d=0.8, n=10, sig.level = 0.05, alternative="two.sided")

# If we drop the number of data points or decrease the effect size, power drops: 
pwr.t.test(d=0.8, n=5, sig.level = 0.05, alternative="two.sided")
pwr.t.test(d=0.54, n=10, sig.level = 0.05, alternative="two.sided")

# And if we increase n or effect size, the reverse occurs:
pwr.t.test(d=1.35, n=10, sig.level = 0.05, alternative="two.sided")
pwr.t.test(d=0.8, n=25, sig.level = 0.05, alternative="two.sided")

# The pwr package contains some other built-in power analysis functions,
# but you will note there is nothing for the Mann-Whitney U test (nonparametric).
# This is because parametric power analysis assumes the distribution of the data (normal, for a t-test).
# For non-parametric tests, power has to be estimated.

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
my_data_1<-my_data %>%
  filter(group == "A") %>%
  select(values) %>%
  t() %>%
  as.numeric()
my_data_1
my_data_2<-my_data %>%
  filter(group == "B") %>%
  select(values) %>%
  t()%>%
  as.numeric()

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
# Last week, we saw some proportional data from two different sources
# and discussed the idea that proportions are a special kind of data.
#
# In a PROPORTIONAL MODEL, the frequency of occurrence of events is proportional to the number of opportunities.
# The relationship between observed frequencies and expectations from such a model
# is assessed using a CHI-SQUARED GOODNESS OF FIT test.

# Let's look at the glutamate receptor data again.
# Read in the data file
"ICC_data_df" <-read_excel("ICC_data_Week8.xlsx")

# Take a peek
glimpse(ICC_data_df)

# Isolate the 60-minute data for treatment ("mbcd") and control ("vehicle")
sixtymin_df <- ICC_data_df %>% 
  filter(trial==1) %>% 
  filter(treatment == "veh60" | treatment == "mbcd60")
glimpse(sixtymin_df)

# Note that each trial contains data for 40 cells. 

pwr.MW.test.boot(my_data_1 = my_data_1, my_data_2 = my_data_2)
pwr.t.test(d=0.52, n=10, sig.level = 0.05, alternative="two.sided")

