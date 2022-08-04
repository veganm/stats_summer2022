#Load Packages 
pacman::p_load(tidyverse, effectsize, pwr, tidyverse, cowplot, ggpubr, PairedData)

##Make up some data- the one Nic made up in week7

#Weight of the mice before treatment
before <-c(200.1, 190.9, 192.7, 213, 241.4, 196.9, 172.2, 185.5, 205.2, 193.7)
# Weight of the mice after treatment
after <-c(392.9, 393.2, 345.1, 393, 434, 427.9, 422, 383.9, 392.3, 352.2)
# Create a data frame
my_data <- data.frame( 
  group = rep(c("before", "after"), each = 10),
  weight = c(before,  after)
)

#plot data
pd <- paired(before, after)
plot(pd, type = "profile") + theme_bw()

#determine if difference is normally distrubuted
# compute the difference
d <- with(my_data, 
          weight[group == "before"] - weight[group == "after"])

# Shapiro-Wilk normality test for the differences
shapiro.test(d) 

#Paired t-test on data frame
ttest <- t.test(weight ~ group, data = my_data, paired = TRUE)
ttest

#calculate effect size
effectsize(ttest)

#power test 
pwr.t.test(d=3.75, n=3, sig.level = 0.05, alternative="two.sided")

#At this effect size, how many data points would be needed for a power of 0.2?  n=2

#0.8? n= 3
