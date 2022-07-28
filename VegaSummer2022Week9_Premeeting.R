# Vega Lab Meeting Week 9
# Pre-meeting script, with starter code
#
# First we load the required packages
pacman::p_load(tidyverse, effectsize, pwr)

# Then we take (or make) some data
# I am going to make some up
set.seed(1234)
my_data <- data.frame(
  group = rep(c("A", "B"), each=10), 
<<<<<<< HEAD
  data = c(round(rnorm(10, 20, 2), 1), round(rnorm(10, 22, 2), 1))
=======
  data = c(round(rnorm(10, 20, 2), 1), round(rnorm(10, 25, 2), 1))
>>>>>>> a1208e1d554cc5e4735cc698c495c386e1ca5e10
)
glimpse(my_data)

# let's say I want to do a t-test.
# First I want to know the effect size:
TT<-t.test(data~group, data=my_data, var.equal=TRUE)
TT
effectsize(TT)

# and then the power:
<<<<<<< HEAD
pwr.t.test(d=-0.8, n=10, sig.level = 0.05, alternative="two.sided")
=======
pwr.t.test(d=-2.8, n=10, sig.level = 0.05, alternative="two.sided")
>>>>>>> a1208e1d554cc5e4735cc698c495c386e1ca5e10
