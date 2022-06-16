# Week 3, Vega lab meeting 2022
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

# Today we will also be using some worm data
# Salmonella enterica LT2-GFP single worms (fed on plates), same as in bleach protocol
# and corresponding data for S. aureus-GFP

PathogenCount<-read.csv("PathogenCount.csv")
glimpse(PathogenCount)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~   Picking up from week 2:
#~~~~~~~~     Mutations of data
# Data can be pushed through mutate() to create transformed columns
# Let's say we want temp data in degrees C as well as F
# Note that we are over-writing the original data frame; since we aren't losing data, this is ok
glimpse(weather)
weather <- weather %>% 
  mutate(temp_in_C = (temp - 32) / 1.8)

# The new data can be treated like any other data
weather %>% 
  group_by(month) %>%
  summarize(mean_temp_in_F = mean(temp, na.rm = TRUE), 
            mean_temp_in_C = mean(temp_in_C, na.rm = TRUE))

# We can do the same thing to the flights data
# Passengers are often frustrated when their flight departs late, 
# but aren't as annoyed if, in the end, pilots can make up some time during the flight. 
# This is known in the airline industry as gain, and we will create this variable using the mutate() function:
flights <- flights %>% 
  mutate(gain = dep_delay - arr_delay)

# Let's look at the first ten flights, considering only delays and gain:
flights %>%
  subset(select=c(dep_delay, arr_delay, gain)) %>%
  print(n=10, width=Inf)

# and generate some summary statistics
# what should "missing" contain?
gain_summary <- flights %>% 
  summarize(
    min = min(gain, na.rm = TRUE),
    q1 = quantile(gain, 0.25, na.rm = TRUE),
    median = quantile(gain, 0.5, na.rm = TRUE),
    q3 = quantile(gain, 0.75, na.rm = TRUE),
    max = max(gain, na.rm = TRUE),
    mean = mean(gain, na.rm = TRUE),
    sd = sd(gain, na.rm = TRUE),
    missing = sum(is.na(gain))
  )
gain_summary

# Since gain is numeric, we can visualize it several different ways:
# As a histogram
ggplot(data = flights, mapping = aes(x = gain)) +
  geom_histogram(color = "white", bins = 40)+
  theme_classic()+
  geom_vline(xintercept=quantile(flights$gain, 0.75, na.rm=TRUE), linetype="dashed", lwd=1.5, color="blue")+
  geom_vline(xintercept=median(flights$gain, na.rm=TRUE), linetype="dashed", lwd=1.5, color="red")+
  geom_vline(xintercept=quantile(flights$gain, 0.25, na.rm=TRUE), linetype="dashed", lwd=1.5, color="blue")

# As a box or violin plot, separated by some factor of interest (here, by carrier)
# What does the function complete_cases() do? What will be in flights2?
?complete.cases
flights2<-flights[complete.cases(flights),]
flights2 %>%
  ggplot(aes(x=factor(carrier), y=gain))+
  geom_boxplot()+
  #geom_violin() +
  geom_hline(yintercept=quantile(flights2$gain, 0.75), linetype=3, lwd=1.2, color="blue")+
  geom_hline(yintercept=median(flights2$gain), linetype=1, lwd=1, color="red")+
  geom_hline(yintercept=quantile(flights2$gain, 0.25), linetype=3, lwd=1.2, color="blue")+
  theme(plot.title=element_text(hjust=0.5, size=14))+
  labs(title="Gain by carrier", y="Gain (minutes)", x="")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
#~~~~~~~   Processing Worm Data
# Let's call back up PathogenCount and plot out
names(PathogenCount)
PathogenCount %>%
  ggplot(aes(x=Date, y=logCount, color=Date)) +
  geom_jitter(shape=16, position=position_jitter(0.05)) +
  geom_violin() + 
  theme_classic() + 
  theme(text=element_text(size=14), 
        axis.title.x = element_text(size=14), 
        axis.text.x = element_text(size=12),
        plot.title=element_text(hjust=0.5, size=14)) + 
  labs(title="", y=expression(log[10](CFU/worm)), x="Sampling Day")+
  facet_wrap(~Species, ncol=1)

# uh-oh. What did we forget to do?
# We have to treat date as a factor here, not a number!
# Could do this in the tibble:
#PathogenCount$Date<-as.factor(PathogenCount$Date)

# We can also tell ggplot() to use Date as a factor.
# Let's save the results as a plot object named pPathogenCount.
pPathogenCount<-PathogenCount %>%
  ggplot(aes(x=factor(Date), y=logCount, color=factor(Date))) +
  geom_jitter(shape=16, position=position_jitter(0.05)) +
  geom_violin(fill=NA) + 
  #geom_violin()+ # If we don't remove the fill, the violin will hide the data points
  theme_classic() + 
  theme(text=element_text(size=14), 
        axis.title.x = element_text(size=14), 
        axis.text.x = element_text(size=12),
        legend.position = "none", 
        plot.title=element_text(hjust=0.5, size=14)) + 
  labs(title="", y=expression(log[10](CFU/worm)), x="Sampling Day")+
  facet_wrap(~Species, ncol=1)
pPathogenCount

# QUESTION: Do these data look normal? How can we tell?
# Note that we are plotting the log10-transformed data. What can we infer about the raw counts?

# We can use the functions just introduced to generate summary statistics.
# What will the PathogenStats tibble contain?
# Remember - what does n() give?
PathogenStats<- PathogenCount %>%
  group_by(Species, Date) %>%
  summarize(mean = mean(Count), 
            std_dev = sd(Count),
            q50=quantile(Count, probs=0.5),
            IQR=IQR(Count),
            log_mean=mean(logCount),
            log_std_dev=sd(logCount),
            log_q50=quantile(logCount, probs=0.5),
            log_IQR=IQR(logCount),
            n=n())

# we'll add a few more 
# first, skewness (a measure of asymmetry) using functionality from e1071
# Let's add columns for skew of raw and log10-transformed data
PathogenStats <- PathogenStats %>%
  add_column(skew=NA,
             log_skew=NA)
# If we look at the tibble, we will see that the columns now exist,
# and all values are NA
PathogenStats

# Let's set up a for loop to populate the skew columns.
# Since PathogenStats was set up as summary statistics for each bacteria and sampling day IN THAT ORDER
# we'll iterate over each bacterial species, then over sampling day within bacterial species.
Species<-unique(PathogenStats$Species)
Days<-unique(PathogenStats$Date)

# create an index (idx) to tell the loop where to put the data
idx<-1
# then loop over bacterial species
for (i in 1:length(Species)){
  # WITHIN EACH BACTERIAL SPECIES, iterate over all sampling days
  for (j in 1:length(Days)){
    # calculate skewness from the count and log-count data for bacteria "i" on sampling day "j"
    PathogenStats$skew[idx]<-skewness(PathogenCount$Count[PathogenCount$Species==Species[i] & PathogenCount$Date==Days[j]])
    PathogenStats$log_skew[idx]<-skewness(PathogenCount$logCount[PathogenCount$Species==Species[i] & PathogenCount$Date==Days[j]])
    idx<-idx+1 #move to the next row of PathogenStats now that this one is full
  }
}

# then calculate coefficient of variation, which is mean/sd, using base vector operations in R
PathogenStats$cv<-PathogenStats$mean/PathogenStats$std_dev
PathogenStats$log_cv<-PathogenStats$log_mean/PathogenStats$log_std_dev
glimpse(PathogenStats)  # did it work?

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
#~~~~~~~~~          Homework for summary, grouping and mutations
#
# A) Using the weather data frame, write code to get the mean and standard deviation temperature for each day in 2013 for NYC.
#
# B) Using the flights data frame, identify how many flights took off for each of the three airports for each carrier.
# B.1) Plot the number of flights for each airport and carrier with month as the x-axis.
#
# C) Consider the diamonds data frame included in the ggplot2 package.
diamonds

# C.1) Group these data by cut, then calculate mean, median, and IQR for price within each cut type.
# Store this information in a new tibble, then ungroup the data.
# C.2) Plot out price data grouped by (1) cut and (2) clarity. Create these plots again using a different plot type.
# Save all of these plots in a single image file using ggsave().
?ggsave


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
#              LOOPING AND ITERATION IN R: FOR LOOPS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
# RULE OF THUMB: Never copy/paste code more than twice.
# Instead, execute the SAME code repeatedly via iteration or functions.
# Pat Schloss (of mothur and Riffomonas) calls this DRY - Don't Repeat Yourself.
#
# Loops execute a block of code over and over again until the close condition is met.
# A for() loop takes a sequence and runs from start to end in units of 1 unless told otherwise.
# The for() loop has the format
# for(index in range) {do stuff}

for (i in 1:5) {
  print(i)
}

# It is technically possible to use increments other than 1 by creating a sequence for the loop to follow
# (and the brackets can be omitted if the loop contains only one line of code):
seq(0,10,2)
for (i in seq(10, 1, -1)) print(i)

# For loops can call other functions in R.
# Let's say we have a tibble:
df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

# and we want the medians for each column (but don't want to use summarize for some reason)
# We could go one by one:
median(df$a)
median(df$b)
median(df$c)
median(df$d)

# But this breaks our rule about copy-paste
# so instead we will use a for() loop.
# Each for() loop must have three components:
?seq_along
output <- vector("double", ncol(df))  # 1. output
for (i in seq_along(df)) {            # 2. sequence
  output[[i]] <- median(df[[i]])      # 3. body
}
output

# An older way of doing this in base R uses length() to set the number of iterations:
output <- vector("double", ncol(df))  # 1. output
for (i in 1:length(df)) {            # 2. sequence
  output[[i]] <- median(df[[i]])      # 3. body
}
output

# seq_along(x) does the same thing as 1:length(x), but safely in case of a zero-length argument
y <- vector("double", 0)
seq_along(y)
1:length(y)

# It’s common to see for loops that don’t pre-allocate the output and instead increase the length of a vector at each step.
# This is common if you don't know how many elements you will have, for example.
# In the code below, the c() function concatenates the old output with the new data (attaches new data at the end)
# and then re-writes the output object with the new, longer output.
# Let's see how the performance compares with and without pre-allocation. 
# We will use the function Sys.time() to get clock time on the local computer at the start and end of the code block.
# To run: Highlight all the rows from start_time to end_time and run them together, to avoid manual click delays
# No pre-allocation, where output starts as a numeric vector of length 0:
start_time<-Sys.time()
output <- vector("integer", 0)
for (i in seq_along(y)) {
  output <- c(output, median(df[[i]]))
}
output
end_time<-Sys.time()
end_time-start_time

# With pre-allocation, where output starts as a full-length empty vector:
start_time<-Sys.time()
output <- vector("double", ncol(df))  # 1. output
for (i in 1:length(df)) {            # 2. sequence
  output[[i]] <- median(df[[i]])      # 3. body
}
output
end_time<-Sys.time()
end_time-start_time

# In general, it will be slower and less efficient to update an object in each iteration.
# There are ways of avoiding over-writing even when we don't know how many results we will produce -
# we'll discuss that when we talk about functions next week.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#               HOMEWORK FOR LOOPS
# D) Pick one of the loops in section 21.2.1 of R for Data Science.
# For next week, write a for loop to perform that task. 

