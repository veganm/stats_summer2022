# Week 4, Vega Lab Meeting Summer 2022
# Today we will be exploring summary statistics of quantitative data
# using code and other material from
#   https://moderndive.com/3-wrangling.html
#   https://r4ds.had.co.nz/iteration.html
#   https://intro2r.com/loops.html
#   https://intro2r.com/prog_r.html
#   https://ggplot2-book.org/facet.html
#   https://www.dataanalytics.org.uk/axis-labels-in-r-plots-using-expression/
#
#  Background material for this section is from
#   https://www.khanacademy.org/math/statistics-probability/summarizing-quantitative-data
#
# We will:
# - learn about iteration and generating functions in R
# - learn more about how to generate summary statistics for data in R
# - review the concept of random sampling
# - review summary statistics of data
# - continue learning about the concept of (log)normality as applied to biological data
# - observe the central limit theorem in action

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# First, as usual, we will load the libraries we need
packages<-c("tidyverse","nycflights13", "cowplot", "e1071", "nortest", "ggpubr")
install.packages(setdiff(packages, rownames(installed.packages())))

library(tidyverse)
library(nycflights13)
library(cowplot)
library(e1071)
library(nortest)
library(ggpubr)

# W will also be using some worm data
# Salmonella enterica LT2-GFP single worms (fed on plates), same as in bleach protocol
# and corresponding data for S. aureus-GFP

PathogenCount<-read.csv("PathogenCount.csv")
glimpse(PathogenCount)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#               Back to the data analysis.
# Now we can start making comparisons with the normal distribution
# S. aureus day 1 has 31 observations, let's match that number of data points
?rnorm
hist(rnorm(31))
# Now let's allow the simulated data to have the same mean and SD as the count data
hist(rnorm(31, mean=7325, sd=11592))
# What's wrong here? What is true of these data that can't be true of pathogen counts?

# Let's instead use the mean and SD of the log10-transformed data - is this better?
# Run the histogram a few times -  what do you see?
hist(rnorm(31, mean=3.49, sd=0.591), breaks=10, xlim=c(0,6))

# Is the normal a good match for the log-transformed real CFU data?
# This would imply that the CFU data are lognormal, btw.
# First let's just compare the histogram of the real data to the normal.
# We'll need to pre-generate our Gaussian data; note that we are plotting a density histogram (scaled to 1)
set.seed(0)
data<-tibble(x=rnorm(1000, mean=3.49, sd=0.591))
PathogenCount %>%
  filter(Species=="SA" & Date==1) %>%
  ggplot(aes(x=logCount))+
  geom_histogram(aes(y = ..density..), fill="lightgrey", color = "black") +
  stat_function(fun = dnorm, args = list(mean=mean(data$x), sd=sd(data$x)))+
  theme_bw()

# We can also assess normality visually using a Q-Q plot
# First for the count data
PathogenCount  %>%
  filter(Species=="SA" & Date==1) %>%
  {list(qqnorm(.$Count, pch=1, frame=FALSE), qqline(.$Count, col="red", lwd=2))}
# And then for the log-transformed data
PathogenCount  %>%
  filter(Species=="SA" & Date==1) %>%
  {list(qqnorm(.$logCount, pch=1, frame=FALSE), qqline(.$logCount, col="red", lwd=2))}
PathogenCount  %>%
  filter(Species=="SA" & Date==2) %>%
  {list(qqnorm(.$logCount, pch=1, frame=FALSE), qqline(.$logCount, col="red", lwd=2))}
PathogenCount  %>%
  filter(Species=="SA" & Date==3) %>%
  {list(qqnorm(.$logCount, pch=1, frame=FALSE), qqline(.$logCount, col="red", lwd=2))}
# For the pooled S. aureus data
PathogenCount  %>%
  filter(Species=="SA") %>%
  {list(qqnorm(.$logCount, pch=1, frame=FALSE), qqline(.$logCount, col="red", lwd=2))}
# and for the pooled S. enterica data
PathogenCount  %>%
  filter(Species=="SE") %>%
  {list(qqnorm(.$logCount, pch=1, frame=FALSE), qqline(.$logCount, col="red", lwd=2))}

# Let's look back at the PathogenStats we generated.
PathogenStats
# We can ask the same statistics of the normally distributed data. What's different?
mean(data$x) 
sd(data$x)
quantile(data$x, probs=0.5)
IQR(data$x)
skewness(data$x)

# There are also statistical tests for normality, such as the Shapiro-Wilk test
shapiro.test(PathogenCount$logCount[PathogenCount$Species=="SA" & PathogenCount$Date==1])
shapiro.test(PathogenCount$logCount[PathogenCount$Species=="SA" & PathogenCount$Date==2])
shapiro.test(PathogenCount$logCount[PathogenCount$Species=="SA" & PathogenCount$Date==3])
shapiro.test(PathogenCount$logCount[PathogenCount$Species=="SA"])
shapiro.test(PathogenCount$logCount[PathogenCount$Species=="SE"])

# Note, however, that like many tests, Shapiro starts to get weird as n changes - why?
shapiro.test(data$x)
data2<-tibble(x=rnorm(31, mean=3.49, sd=0.591))
shapiro.test(data2$x)

# Other tests for normality (Anderson-Darling, Komolgorov-Smirnov) differ in sensitivity
#K-S is in base R and compares to a default N(0,1) as shown here
ks.test(data$x, "pnorm")
data<-data %>%
  mutate(y=(x-3.49)/0.591)
ks.test(data$y, "pnorm")

#Anderson-Darling is in the nortest library 
# and assumes mean and SD of the normal distribution are as in the data
ad.test(data$x)
ad.test(data2$x)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~              Populations of sample means:
#~    Biological averaging, skewed data, and the Central Limit Theorem
# Last time, we said that the average of data from a normal will be normal
# Let's check!
# But first, we need to talk about...
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#                  FUNCTIONS IN R
#
# Functions contain code that we want to use repeatedly,
# but where we want to change what happens in different runs.
# Let's see how this happens.

# If you enter the name of a function without the (),
# you can usually see the source code for the function.
# Let's do that now so we can see what a function contains.
theme_classic

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~              WRITING A FUNCTION
# A function in R has the form:
# nameOfFunction <- function(argument1, argument2, ...) {
#                     #expression that does stuff
#                     result <- #put stuff here
#                     return(result)
#                     }
# The function starts by collecting arguments (in parentheses, some of which have default values)
# which will be passed to the code in curly brackets {expression}
f_add_1<-function(x) {x+1}

# Look at your environment - a new category emerges!
# Let's try it
f_add_1(5)

# (the brackets can be omitted if your code is only one line)
f_add_1<-function(x) x+1
f_add_1(5)

# What if we provide a default value?
f_add_1<-function(x=1) x+1
f_add_1(5)
f_add_1()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~              A MORE COMPLICATED FUNCTION
# Here, the function sim_means_4dist() contains code we want to reuse
# This code is provided in script sim_means_4dist.R
# Go run the script now.
# What will the code do?
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~                OK, back to the story.
#~~~~              Populations of sample means:
#~    Biological averaging, skewed data, and the Central Limit Theorem
# Last time, we said that the average of data from a normal will be normal
# Let's check!

# First let's look at these distributions
# We define a shared mean for all data
shared_mean<-1 # our data will have the same true mean

# For the normal, start by creating a sequence of values based on population mean and standard deviation
# First we will create the x-axis
c(-4, 4)
c(-4, 4)*shared_mean+shared_mean

# then generate & draw simulated data from a statistical distribution using 
?stat_function()

pNorm<-
  ggplot(data.frame(x = c(-4, 4)*shared_mean+shared_mean), aes(x = x)) +
  stat_function(fun = dnorm, args=list(mean=shared_mean, sd=shared_mean))+
  theme_classic()+
  theme(text=element_text(size=16), 
        plot.title=element_text(hjust=0.5, size=16)) + 
  labs(title="Normal")
pNorm

# We can do the same for other distributions, but note that the x-axes are different
pLNorm<-
  ggplot(data.frame(x = c(0, 4)*shared_mean), aes(x = x)) +
  stat_function(fun = dlnorm, args=list(mean=(log(shared_mean)-(shared_mean^2)/2), sd=log(shared_mean)+shared_mean))+
  theme_classic()+
  theme(text=element_text(size=16), 
        plot.title=element_text(hjust=0.5, size=16)) + 
  labs(title="Lognormal")
pPoiss<-
  ggplot(data.frame(x = c(0, 4)*shared_mean), aes(x = x)) +
  stat_function(fun = dpois, args=list(lambda=shared_mean))+
  theme_classic()+
  theme(text=element_text(size=16), 
        plot.title=element_text(hjust=0.5, size=16)) + 
  labs(title="Poisson")
pExponential<-
  ggplot(data.frame(x = c(0, 4)*shared_mean), aes(x = x)) +
  stat_function(fun = dexp, args=list(rate=1/shared_mean))+
  theme_classic()+
  theme(text=element_text(size=16), 
        plot.title=element_text(hjust=0.5, size=16)) + 
  labs(title="Exponential")
plot_grid(pNorm, pLNorm, pPoiss, pExponential)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# What is true of the underlying distributions?
y_norm<-rnorm(1000, mean=shared_mean, sd=shared_mean)
mean(y_norm)
median(y_norm)
skewness(y_norm)

y_pois<-rpois(1000, lambda=1)
mean(y_pois)
median(y_pois)
skewness(y_pois)

y_exp<-rexp(1000, rate=1/shared_mean)
mean(y_exp)
median(y_exp)
skewness(y_exp)

y_lnorm<-rlnorm(1000, meanlog=(log(shared_mean)-(shared_mean^2)/2), sdlog=(log(shared_mean)+shared_mean))
mean(y_lnorm)
median(y_lnorm)
skewness(y_lnorm)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Let's call the new function to create simulated data
n_reps<-20 # number of "samples" to take
sample_sizes<-c(2,5,10,20,100) # number of "measurements" in a "sample"
shared_mean<-1 # our data will have the same true mean

start_time<-Sys.time()
dist_means_20<-sim_means_4dist(n_reps=n_reps, sample_sizes=sample_sizes, shared_mean=shared_mean)
end_time<-Sys.time()
end_time-start_time

start_time<-Sys.time()
dist_means_20_f<-sim_means_4dist_fast(n_reps=n_reps, sample_sizes=sample_sizes, shared_mean=shared_mean)
end_time<-Sys.time()
end_time-start_time

# Plot out histograms of simulated data
# The red line is the true mean for all populations
pdist_means_20<-dist_means_20_f %>%
  ggplot(aes(x=means)) +
  #geom_histogram(aes(y = ..density..), color = "white", binwidth=1) +
  geom_histogram(color = "white", binwidth=0.25) +
  geom_vline(aes(xintercept = 1), lty=1, lwd=1, color="red")+
  theme_classic()+
  facet_wrap(vars(dist_name, sample_size), ncol=5)+
  theme(plot.title=element_text(hjust=0.5, size=14))+
  labs(title="Simulated means, 20 samples")
pdist_means_20

# Again, with a larger number of "samples"
# Note we are switching to facet_grid() for this display - 
# this works well when you have two discrete identifying variables
# and all combinations of these variables are present in the data

start_time<-Sys.time()
dist_means_100<-sim_means_4dist(n_reps=100, sample_sizes=sample_sizes, shared_mean=shared_mean)
end_time<-Sys.time()
end_time-start_time

start_time<-Sys.time()
dist_means_100_f<-sim_means_4dist_fast(n_reps=100, sample_sizes=sample_sizes, shared_mean=shared_mean)
end_time<-Sys.time()
end_time-start_time

pdist_means_100<-dist_means_100 %>%
  ggplot(aes(x=means)) +
  #geom_histogram(aes(y = ..density..), color = "white", binwidth=1) +
  geom_histogram(color = "white", binwidth=0.2) +
  geom_vline(aes(xintercept = 1), lty=1, lwd=1, color="red")+
  theme_classic()+
  facet_grid(vars(dist_name), vars(sample_size)) +
  theme(plot.title=element_text(hjust=0.5, size=14))+
  labs(title=" Simulated means, 100 samples")
pdist_means_100

# We can also add a dashed blue line indicating the mean of the data within each set
mu_20<-dist_means_20 %>%
  group_by(dist_name, sample_size) %>%
  summarize(grp.mean=mean(means))
pdist_means_20 + geom_vline(data=mu_20, aes(xintercept=grp.mean), color="blue", lty=2, lwd=1.2)

mu_100<-dist_means_100 %>%
  group_by(dist_name, sample_size) %>%
  summarize(grp.mean=mean(means))
pdist_means_100 + geom_vline(data=mu_100, aes(xintercept=grp.mean), color="blue", lty=2, lwd=1.2)

# For each combination, is the sample mean an accurate estimator? Is it precise?
# What's going on here?
# First, let's focus in on the normal vs. lognormal simulated means
dist_means_100 %>%
  filter(dist_name=="Norm" | dist_name =="LNorm")

dist_means_100 %>%
  filter(dist_name=="Norm" | dist_name =="LNorm") %>%
  ggplot(aes(x=means)) +
  geom_histogram(aes(y = ..density..), color = "white", binwidth=0.25) +
  #geom_histogram(color = "white", binwidth=0.2) +
  geom_vline(aes(xintercept = 1), lty=1, lwd=1, color="red")+
  theme_classic()+
  facet_grid(vars(dist_name), vars(factor(sample_size))) +
  theme(plot.title=element_text(hjust=0.5, size=14))+
  labs(title=" Simulated means, 50 samples")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~         Summary statistics of real data:
#~~            CFU/worm in C. elegans
#
# Let's return to the PathogenCounts data set
pPathogenCount

# We already know these data are at best log-normal, so what do the means look like?
# Fortunately, we have experimental data to show this
# These are real data from batch digests 2022-2-18, S. aureus and S. enterica
# SA looks fine, but I didn't get enough worms for SE, and needed to plate higher dilutions
BatchDigests<-read.table("batchdigests.txt", header=TRUE)
BatchDigests$Batch<-as.factor(BatchDigests$Batch)
pBatchSA<-subset(BatchDigests, Species=="SA") %>%
  ggplot(aes(x=Batch, y=logCFU, color=Batch)) + 
  geom_jitter(shape=16, position=position_jitter(0.05)) +
  geom_violin(fill=NA) + theme_classic() + 
  theme(text=element_text(size=16), 
        axis.title.x = element_blank(), 
        axis.text.x = element_blank(),
        plot.title=element_text(hjust=0.5, size=14),
        legend.position=c(0.9,0.3)) + 
  labs(title=expression(paste(italic("S. aureus"), " Newman")), y="log10(CFU/worm)")
pBatchSA

# We can get the same effect by creating synthetic "batches" off of the CFU/worm data
# using function wormboot() from script wormboot.R
# Go run that script now
?subset

# Once the function is loaded, we can use it.
names(PathogenCount)
SaBoot1<-PathogenCount %>%
  subset(Species=="SA" & Date==1, select=Count) %>%
  t() %>%
  wormboot(reps=25)
SaBoot1$Day<-as.factor("1")

SaBoot2<-PathogenCount %>%
  subset(Species=="SA" & Date==2, select=Count) %>%
  t() %>%
  wormboot(reps=25)
SaBoot2$Day<-as.factor("2")

jointSaBoot<-rbind(SaBoot1, SaBoot2)

# and plot
# ggpubr provides the function stat_compare_means()
# which uses Wilcoxon rank sum testing as a default
jointSaBoot %>%
  ggplot(aes(x=Day, y=logCount, color=Day)) + 
  geom_jitter(shape=16, position=position_jitter(0.05)) +
  geom_violin(fill=NA) + 
  ylim(-0.1,6)+ theme_classic() + 
  theme(
    text=element_text(size=14), 
    #axis.title.x = element_blank(),
    #axis.text.x = element_blank(),
    plot.title=element_text(hjust=0.5,size=14),
    legend.position="none")+
  stat_compare_means(label.y = 0.05, label.x=1.1) +
  labs(title="Simulated batch digests", x="Sampling Day", y=expression(log[10](CFU/Worm)))+
  facet_wrap(~batch)

