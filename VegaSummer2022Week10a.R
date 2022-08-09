# Vega lab meeting, summer 2022, week 10
# This week we will discuss the mysteries of where a hypothesis test comes from,
# what a test statistic is, and how it is turned into a p-value
# as we unpack the normal distribution, the chi-squared, and the F test
# so that we can discuss ANOVA and other partitions of variance.
# Background information is from:
#   https://www.khanacademy.org/math/statistics-probability/modeling-distributions-of-data#z-scores
#   https://www.khanacademy.org/math/statistics-probability/significance-tests-one-sample#more-significance-testing-videos


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
  labs(title="Normal pdf")
pNormal

# We can also plot the cumulative density function (cdf):
pNormalC<-
  ggplot(data.frame(x = c(-4, 4)*shared_sd+shared_mean), aes(x = x)) +
  stat_function(fun = pnorm, args=list(mean=shared_mean, sd=shared_sd))+
  theme_classic()+
  theme(text=element_text(size=16), 
        plot.title=element_text(hjust=0.5, size=16)) + 
  labs(title="Normal cdf")
pNormalC
plot_grid(pNormal, pNormalC, nrow = 2)

# While the function dnorm() gives the density of the probability distribution function (pdf) at a given value of x,
# the function pnorm() gives the cumulative probability density function (cdf),
# and qnorm() gives the quantile function.
# Let's see what this means. 

dnorm(5, mean=shared_mean, sd=shared_sd) # Look at the pdf plot. Does this make sense?
dnorm(0, mean=shared_mean, sd=shared_sd)
dnorm(2.5, mean=shared_mean, sd=shared_sd)
dnorm(7.5, mean=shared_mean, sd=shared_sd)
dnorm(10, mean=shared_mean, sd=shared_sd)

# Note that lower.tail is a logical in pnorm() and qnorm(); 
# if TRUE (default), probabilities are P[X ≤ x] otherwise, P[X > x].
pnorm(5, mean=shared_mean, sd=shared_sd)
pnorm(5, mean=shared_mean, sd=shared_sd, lower.tail = FALSE)
qnorm(0.5, mean=shared_mean, sd=shared_sd)
qnorm(0.5, mean=shared_mean, sd=shared_sd, lower.tail = FALSE)
qnorm(0.25, mean=shared_mean, sd=shared_sd)
qnorm(0.25, mean=shared_mean, sd=shared_sd, lower.tail = FALSE)
qnorm(0.75, mean=shared_mean, sd=shared_sd)
qnorm(0.75, mean=shared_mean, sd=shared_sd, lower.tail = FALSE)

# Note the values we are using here. What is this equivalent to?
x1<-qnorm(0.025, mean=shared_mean, sd=shared_sd)
x1
x2<-qnorm(0.025, mean=shared_mean, sd=shared_sd, lower.tail = FALSE)
x2
pnorm(x1, mean=shared_mean, sd=shared_sd)
pnorm(x2, mean=shared_mean, sd=shared_sd, lower.tail = FALSE)

# What will this code give us?  
pnorm(shared_mean-shared_sd, mean=shared_mean, sd=shared_sd)
pnorm(shared_mean+shared_sd, mean=shared_mean, sd=shared_sd)
pnorm(shared_mean+0.5*shared_sd, mean=shared_mean, sd=shared_sd) - pnorm(shared_mean-0.5*shared_sd, mean=shared_mean, sd=shared_sd)
pnorm(shared_mean+1*shared_sd, mean=shared_mean, sd=shared_sd) - pnorm(shared_mean-1*shared_sd, mean=shared_mean, sd=shared_sd)
pnorm(shared_mean+1.5*shared_sd, mean=shared_mean, sd=shared_sd) - pnorm(shared_mean-1.5*shared_sd, mean=shared_mean, sd=shared_sd)


# We've glossed over something interesting about the normal.
# We rescaled the interval in a very particular way using the shared mean and SD.
# This is because the normal distribution has the particular property
# that if X ~ N(mu, s^2), then
# X - mu ~ N(0, s^2); and (X-mu)/s ~ N(0,1).
# I'll show this on the whiteboard so we understand how it works.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# So what's a Z-test?
# Recall from the Khan Academy video that, if we know that a value x is from a normal distribution,
# and we have at least estimates of the mean and variance of that distribution,
# we can calculate a Z-SCORE as (x-mu)/(s/sqrt(n))
# and that Z-score can be converted into a p-value, 
# indicating the probability of drawing a value at least as extreme as x from that distribution.

# Let's visualize. What are we showing here?
pNormal + 
  geom_vline(xintercept=x1, color="red") +
  geom_vline(xintercept=x2, color="red")

# This is from the Normal(mean, SD) distribution that we declared.
# What happens to the mean as the sample size changes?
# Let's recall some code from week 5:

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     A FUNCTION TO LOOK AT MEANS OF SAMPLES OF DIFFERENT SIZES
sim_means_4dist_fast<-function(n_reps, sample_sizes, shared_mean){
  # A function to generate simulated distributions of means
  # using the normal, lognormal, exponential, and Poisson distributions 
  # Data generated are average values for simulated "samples"
  # n_reps is the number of "samples" to generate
  # sample_sizes is a vector of sample sizes
  # shared_mean is the population mean for all three distributions
  # Returns a tibble where observed mean values are stored for each "sample"
  
  # Create an empty temporary vector of defined size to hold the data
  temp<-vector("list", length=length(sample_sizes)*n_reps)
  
  # and populate it
  # Establish an index which will be used to store data
  idx<-1
  # Iterate over each of the values in the vector of sample sizes
  for (i in 1:length(sample_sizes)){
    #print(i) # for debugging
    # Within each sample size, execute the indicated number of replicates
    for (j in 1:n_reps){
      #print(j) # for debugging
      # Within each replicate, create a tibble with the mean from each sample
      # and staple it into the temporary vector
      temp[[idx]]<-tibble(dist_name=c("Norm", "LNorm", "Pois", "Exp"),
                          sample_size=sample_sizes[i],
                          means=c(mean(rnorm(sample_sizes[i], mean=shared_mean, sd=shared_mean)),
                                  mean(rlnorm(sample_sizes[i], 
                                              mean=(log(shared_mean)-(shared_mean^2)/2), 
                                              sd=log(shared_mean)+shared_mean)),
                                  mean(rpois(sample_sizes[i], lambda=shared_mean)),
                                  mean(rexp(sample_sizes[i], rate=1/shared_mean)))
      )
      idx<-idx+1
    }
  }
  # Rearrange the finished data from temp into a proper tibble
  # with columns dist_name, sample_size, and means
  distribution_means<-dplyr::bind_rows(temp)
  return(distribution_means)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Let's call the new function to create simulated data
n_reps<-20 # number of "samples" to take
sample_sizes<-c(2,5,10,20,100) # number of "measurements" in a "sample"
shared_mean<-1 # our data will have the same true mean

dist_means_20_f<-sim_means_4dist_fast(n_reps=n_reps, sample_sizes=sample_sizes, shared_mean=shared_mean)

# Plot out histograms of simulated data
# The red line is the true mean for all populations
pdist_means_20<-dist_means_20_f %>%
  ggplot(aes(x=means)) +
  #geom_histogram(aes(y = ..density..), color = "white", binwidth=1) +
  geom_histogram(color = "white", binwidth=0.25) +
  geom_vline(aes(xintercept = 1), lty=1, lwd=1, color="red")+
  theme_classic()+
  facet_grid(vars(dist_name), vars(sample_size)) +
  theme(plot.title=element_text(hjust=0.5, size=14))+
  labs(title="Simulated means, 20 samples")
pdist_means_20

# Just the normal? OK
dist_means_20_f %>%
  filter(dist_name=="Norm") %>%
  ggplot(aes(x=means)) +
  #geom_histogram(aes(y = ..density..), color = "white", binwidth=1) +
  geom_histogram(color = "white", binwidth=0.25) +
  geom_vline(aes(xintercept = 1), lty=1, lwd=1, color="red")+
  theme_classic()+
  facet_wrap(vars(sample_size), ncol=5)+
  theme(plot.title=element_text(hjust=0.5, size=14))+
  labs(title="Simulated means, 20 samples, normal distribution")

# What exactly is changing?
dist_means_20_f %>%
  filter(dist_name=="Norm") %>%
  group_by(sample_size) %>%
  summarize(mean=mean(means, na.rm=TRUE),
            median=median(means, na.rm=TRUE),
            sd=sd(means, na.rm=TRUE)
            )

# If the formula for the Z-score is right, then...
1/sqrt(2)
1/sqrt(5)
1/sqrt(10)
1/sqrt(100)

# So.
# Recall from the Khan Academy video that, if we know that a value x is from a normal distribution,
# and we have at least estimates of the hypothetical mean and variance of that distribution,
# we can calculate a Z-SCORE as (x-mu)/(s/sqrt(n))
# and that Z-score can be converted into a p-value, 
# indicating the probability of drawing a value at least as extreme as x from that distribution.
#
# IN SIMPLE LANGUAGE, WHAT IS THE DISTRIBUTION TO WHICH THE Z-SCORE BELONGS?
# How do we find the p-value corresponding to a z-score?


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#               THE BINOMIAL DISTRIBUTION AND TEST
#
# Last week, we started talking about binomials
# as part of our discussion on proportional data.

