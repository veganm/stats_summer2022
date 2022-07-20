# Week 6, Vega lab meeting 2022
# Today we will continue learning about normality and log-normality
# in the context of random variables and random sampling
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
#   https://www.khanacademy.org/math/statistics-probability/random-variables-stats-library
#   https://www.khanacademy.org/math/statistics-probability/designing-studies
#
# We will:
# - learn more about iteration and generating functions in R
# - learn about statistical distributions in R
# - review the concept of random sampling
# - review summary statistics of data
# - continue learning about the concept of (log)normality as applied to biological data
# - observe the central limit theorem in action

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# First, as usual, we will load the libraries we need
packages<-c("tidyverse", "cowplot", "e1071", "ggpubr")
install.packages(setdiff(packages, rownames(installed.packages())))

library(tidyverse)
library(cowplot)
library(e1071)
library(ggpubr)

# Today we will also be using some worm data
# Salmonella enterica LT2-GFP single worms (fed on plates), same as in bleach protocol
# and corresponding data for S. aureus-GFP

PathogenCount<-read.csv("PathogenCount.csv")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#              PROPERTIES OF LOG-NORMALITY
#
# The paper for this week described log-normality as a "multiplicative process"
# Let's confirm for ourselves that this is the case.
#
# First, we will simulate data for an additive process.
# As in the paper, we will use a simple Markov random walk
# What happens in a Markov process? What do we expect to see at the end?

# First we define the parameters of our process
# nsteps is the number of random steps we will take
nsteps<-20

# We will take a number of "walks" equal to reps
# and each step can be one unit forward or backwards, no other choices.
reps<-1000
step_choice<-c(-1,1)

# Declare an object to keep our data
data_markov<-numeric(reps)

# Now carry out the walk. What will this code do?
for (i in seq_len(reps)){
  myplace<-1 # This will be the center of our distribution
  for (j in seq_len(nsteps)){
    myplace<-myplace+sample(step_choice, size=1)
  }
  data_markov[[i]]<-myplace
}

# Plot out. Is this what we expected?
hist(data_markov, xlim=c(-30, 30))

# Are the summary statistics what we expected?
mean(data_markov)
median(data_markov)
var(data_markov)
skewness(data_markov)
IQR(data_markov)

# What will happen when we decrease or increase the number of steps?
# Try each nsteps below; go back to line 65 and re-run the loop.
nsteps<-10
nsteps<-50

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Now we will do the same for a multiplicative process.
# First we define the parameters of our process
reps<-1000
nsteps<-20
cLN<-1.5 # the "c" from the paper's lognormal simulation
step_choice<-c(cLN, 1/cLN)

# and declare an object to keep our data
data_galton<-numeric(reps)

# now carry out the walk
for (i in seq_len(reps)){
  myplace<-1 # This will again be the center of our distribution
  for (j in seq_len(nsteps)){
    myplace<-myplace*sample(step_choice, size=1)
  }
  data_galton[[i]]<-myplace
}

# Plot out. Is this what we expected?
hist(data_galton)
hist(log10(data_galton))

# We can as usual compare summary statistics
mean(data_markov)
mean(data_galton)

median(data_markov)
median(data_galton)

var(data_markov)
var(data_galton)

skewness(data_markov)
skewness(data_galton)

IQR(data_markov)
IQR(data_galton)

# What do we see in the summary statistics?

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# And just for fun, let's put some simulated data sets together and plot out to compare
reps<-1000
nsteps<-20
step_choice_markov<-c(-1, 1)
cLN<-1.3 # the "c" from the paper's lognormal simulation
step_choice_galton<-c(cLN, 1/cLN)

# Declare an object to keep our data
data_walks<-tibble(markov=numeric(reps),
                   galton=numeric(reps))


# Now carry out the walk. What will this code do?
for (i in seq_len(reps)){
  myplace_m<-1 # This will be the center of our distribution
  myplace_g<-1
  for (j in seq_len(nsteps)){
    myplace_m<-myplace_m+sample(step_choice_markov, size=1)
    myplace_g<-myplace_g*sample(step_choice_galton, size=1)
  }
  data_walks$markov[i]<-myplace_m
  data_walks$galton[i]<-myplace_g
}

data_walks%>%
  mutate(log10galton = log10(galton)) %>%
  pivot_longer(everything(), names_to="type", values_to="value")%>%
  ggplot(aes(x=factor(type), y=value, color=factor(type))) +
  geom_boxplot()+
  geom_jitter(width=0.1, alpha=0.2)+
  theme_classic()+
  theme(plot.title=element_text(hjust=0.5, size=14),
        legend.title=element_blank(),
        axis.text = element_text(size=12))+
  labs(title="Simulated random walks", y="Value", x="")

# And once again we can directly compare the summary statistics.
# Is this consistent with expectations?
summarize_walks<- data_walks %>%
  mutate(log10galton = log10(galton)) %>%
  pivot_longer(everything(), names_to="type", values_to="value")%>%
  group_by(type) %>%
  summarize(mean=mean(value, na.rm=TRUE),
            median=median(value, na.rm=TRUE),
            sd=sd(value, na.rm=TRUE),
            skew=skewness(value, na.rm=TRUE),
            q25=quantile(value, probs=0.25, na.rm=TRUE),
            q75=quantile(value, probs=0.75, na.rm=TRUE),
            q05=quantile(value, probs=0.05, na.rm=TRUE),
            q95=quantile(value, probs=0.95, na.rm=TRUE)
  )
summarize_walks


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~         Summary statistics of real data:
#~~            CFU/worm in C. elegans
#
# Let's return to the PathogenCounts data set
glimpse(PathogenCount)

# Review from week 5: (Non) normality of CFU/worm data
# Is the normal a good match for the log-transformed real CFU data?
# This would imply that the CFU data are lognormal, btw.
# First let's just compare the histogram of the real data to the normal.
# We'll need to pre-generate our Gaussian data 
set.seed(0)
# Let's generate a more complete set of Pathogen Count Stats for S. aureus day 1.
PathogenCountStats_SA1<-PathogenCount %>%
  filter(Species=="SA" & Date==1) %>%
  summarize(mean_count=mean(Count),
            sd_count=sd(Count),
            mean_logcount=mean(logCount),
            sd_logcount=sd(logCount),
            median_logcount=quantile(logCount, probs=0.5),
            IQR_logcount=IQR(logCount),
            skew=skewness(logCount))
PathogenCountStats_SA1
data<-tibble(x=rnorm(1000, mean=PathogenCountStats_SA1$mean_logcount, sd=PathogenCountStats_SA1$sd_logcount))

# Plot out the real data with the new random normal data over top
# Note that we are plotting a density histogram (scaled to 1)
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

# And finally for the pooled S. aureus data
PathogenCount  %>%
  filter(Species=="SA") %>%
  {list(qqnorm(.$logCount, pch=1, frame=FALSE), qqline(.$logCount, col="red", lwd=2))}

# Now let's look at the pooled S. enterica data
PathogenCount  %>%
  filter(Species=="SE") %>%
  {list(qqnorm(.$logCount, pch=1, frame=FALSE), qqline(.$logCount, col="red", lwd=2))}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# We can see that these data are at best log-normal, so what do the means look like?
# Fortunately, we have experimental data to show this
# They are in a file called batchdigests.txt

BatchDigests<-read.table("batchdigests.txt", header=TRUE)
BatchDigests$Batch<-as.factor(BatchDigests$Batch)

# These are real data from batch digests 2022-2-18, S. aureus and S. enterica
# with batch sizes of 1, 5 or 25 worms/batch and n=24 samples for each batch size
# SA looks fine, but I didn't get enough worms for SE, and needed to plate higher dilutions
# so let's plot out the S. aureus data

pBatchSA<-subset(BatchDigests, Species=="SA") %>%
  ggplot(aes(x=Batch, y=logCFU, color=Batch)) + 
  geom_jitter(shape=16, position=position_jitter(0.05)) +
  geom_violin(fill=NA) + theme_classic() + 
  theme(text=element_text(size=16), 
        axis.title.x = element_blank(), 
        axis.text.x = element_blank(),
        plot.title=element_text(hjust=0.5, size=16),
        legend.position=c(0.9,0.3)) + 
  labs(title=expression(paste(italic("S. aureus"), " Newman")), y=expression(log[10](CFU/worm)))
pBatchSA

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# We can get the same effect by creating synthetic "batches" off of the CFU/worm data
# using function wormboot() from script wormboot.R

wormboot<-function(reps, mydata){
  # Expects a number of reps for the bootstrap (reps)
  # and a vector of worm CFU data for individuals (mydata)
  # Returns a data frame of simulated batch digests
  # with batch sizes 1, 5, 10, 20, 50 worms/batch
  # values reported as inferred CFU/worm and log10(CFU/worm)
  capp<-length(mydata)
  batch5<-rep(0,reps)
  batch10<-rep(0,reps)
  batch20<-rep(0,reps)
  batch50<-rep(0, reps)
  for(i in 1:reps){
    idx5<-sample(1:capp,5,replace=TRUE)
    idx10<-sample(1:capp,10,replace=TRUE)
    idx20<-sample(1:capp,20,replace=TRUE)
    idx50<-sample(1:capp,50,replace=TRUE)
    batch5[i]<-mean(mydata[idx5])
    batch10[i]<-mean(mydata[idx10])
    batch20[i]<-mean(mydata[idx20])
    batch50[i]<-mean(mydata[idx50])
  }
  batch5log<-log10(batch5+1)
  batch10log<-log10(batch10+1)
  batch20log<-log10(batch20+1) 
  batch50log<-log10(batch50+1) 
  batch<-c(rep(1,times=capp), rep(5, times=reps), rep(10, times=reps), rep(20, times=reps), rep(50, times=reps))
  logCFU<-log10(mydata)
  logCount<-c(logCFU, batch5log, batch10log, batch20log, batch50log)
  Count<-c(mydata, batch5, batch10, batch20, batch50)
  dataSet<-data.frame(batch, Count, logCount)
  return(dataSet)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Once the function is loaded, we can use it.
# Walk through the code below. What will this do?

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
  stat_compare_means(label.y = 5.5, label.x=1) +
  stat_compare_means(method="t.test", label.y = 0.1, label.x=1) +
  labs(title="Simulated batch digests", x="Sampling Day", y=expression(log[10](CFU/Worm)))+
  facet_wrap(~batch, nrow=1)

# What do we observe in the statistical tests? Why?

