# Vega lab meeting, summer 2022, week 12
# This week we will continue unpacking the mysteries of the chi-squared and F distributions. We will review linear regression on the way to our discussion of ANOVA as a partition of variance.
#
# Background information is from:
#   https://www.khanacademy.org/math/statistics-probability/advanced-regression-inference-transforming/inference-on-slope/v/intro-inference-slope
#   https://www.khanacademy.org/math/statistics-probability/analysis-of-variance-anova-library
#   https://www.statology.org/sst-ssr-sse-in-r/
#   https://induraj2020.medium.com/how-to-derive-b0-and-b1-in-linear-regression-4d4806b231fb
#   https://crumplab.com/statistics/07-ANOVA.html
#   https://www.jstor.org/stable/2682899?seq=1#page_scan_tab_contents
#   
# 
# with code etc from:
#   https://www.statology.org/sst-ssr-sse-in-r/
#   https://www.statmethods.net/graphs/density.html
#   http://www.cookbook-r.com/Graphs/Plotting_distributions_(ggplot2)/
#   http://www.sthda.com/english/wiki/ggplot2-density-plot-quick-start-guide-r-software-and-data-visualization
#   https://cran.r-project.org/web/packages/egg/vignettes/Ecosystem.html
#   https://ggplot2.tidyverse.org/reference/geom_contour.html
#   https://cran.r-project.org/web/packages/metR/vignettes/Visualization-tools.html
#   https://bookdown.dongzhuoer.com/hadley/ggplot2-book/scale-colour.html
#   https://r-graphics.org/recipe-colors-palette-discrete
#   https://stackoverflow.com/questions/55975973/how-to-add-labels-in-a-contour-plot-using-ggplot2

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# As usual we will load the libraries we need:
pacman::p_load(tidyverse, cowplot, effectsize, fBasics, grid, gridExtra, datasets, nycflights13, metR)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#         DESIGNING A CHI-SQUARED
# For your homework, you had to dip into the flights or dune data sets
# and design and carry out a chi-squared test. Let's bring up the data from flights:
flights

# There are a number of options in here. Chi-squared tests are intuitive for cases where:
#   - the response data are counts (frequencies)
#   - a categorical independent variable is used to designate groups
#   - the expected frequency in all groups is >1
#   - not more than 20% of the categories have expected frequency <5
#   - observations can reasonably be considered to be independent

# Let's take a look at the arrival delays, since we have worked with those before.
flights %>%
  group_by(carrier) %>%
  summarize(mean_delay=mean(arr_delay), count=n())

# whoops. What's wrong?
# We forgot to remove NAs from the mean calculation!
flights %>%
  group_by(carrier) %>%
  summarize(mean_delay=mean(arr_delay, na.rm=TRUE), count=n())

# or we can just take out any rows with NAs.
# We can slice the data set based on NAs in arr_delay:
idx<-which(is.na(flights$arr_delay))
flights2<-flights %>% slice(-idx)
flights2 %>%
  group_by(carrier) %>%
  summarize(mean_delay=mean(arr_delay), count=n())

# Or we can use complete.cases()
flights3<-flights[complete.cases(flights),]
flights_delay_summary<-flights3 %>%
  group_by(carrier) %>%
  summarize(mean_delay=mean(arr_delay), count=n())
flights_delay_summary

# Let's plot out just for fun
flights3 %>%
  ggplot(aes(x=arr_delay, y=carrier, colour=carrier))+
  geom_boxplot() +
  theme_classic()+
  theme(text=element_text(size=16), 
        plot.title=element_text(hjust=0.5, size=16),
        legend.title = element_blank()) + 
  labs(title="Arrival Delay by Carrier", x="Delay (min)", y="Carrier")

# Is minutes really the best unit here?
flights3 %>%
  mutate(arr_delay_h = arr_delay/60) %>%
  ggplot(aes(x=arr_delay_h, y=carrier, colour=carrier))+
  geom_boxplot() +
  theme_classic()+
  theme(text=element_text(size=16), 
        plot.title=element_text(hjust=0.5, size=16),
        legend.title = element_blank()) + 
  labs(title="Arrival Delay by Carrier", x="Delay (hrs)", y="Carrier")


# It looks like all groups have more than 5 values, so that's good.
# Now, delays probably aren't completely independent (a storm will delay a lot of flights on a given day)
# but over a year, it's not obvious that any given airline will be more or less subject
# to these sorts of external factors. So for our test, independence is not unreasonable.

# For a chi-squared test, we want frequency data (number of occurrences across outcomes).
# Let's convert arr_delay into this sort of frequency data by binning the numerical data
# (in minutes) into categorical outcomes: on time (arr_delay<=15) or late (arr_delay>15)
flights3<-flights3 %>%
  mutate(arr_delay_bin = (arr_delay<=15))
View(flights3)

# Overall, how many flights are NOT delayed?
sum(flights3$arr_delay_bin)

# How many flights do we have data for?
dim(flights3)

# If all flights in all carriers have the same chance of being late, what is that chance?
p1<-sum(flights3$arr_delay_bin)/dim(flights3)[1]
p1

# OK. Let's remake our summary object to indicate number of late flights per carrier.
# We can then generate an expectation per airline based on the number of flights, and
# calculate the fraction of all late flights that should belong to each airline:
flights_delay_summary<-flights3 %>%
  group_by(carrier) %>%
  summarize(mean_delay=mean(arr_delay), 
            count=n(),
            count_delay=sum(arr_delay_bin)) %>%
  mutate(count_delay_e = count*p1) %>%
  mutate(count_delay_p = count_delay_e/sum(count_delay_e))
flights_delay_summary
# Sanity check
sum(flights_delay_summary$count_delay_p)

# Now we can carry out the chi-squared test for goodness of fit
# under the null hypothesis H0: all airlines have the same probability of late flights
# and therefore Ha: all airlines do NOT have the same probability of late flights
?chisq.test
CT<-chisq.test(flights_delay_summary$count_delay, p=flights_delay_summary$count_delay_p)
CT

# This is a chi-squared with df = (carriers-1) = 15 and a stat of 974.09, p<2.2e-16
# and we reject the null hypothesis that all airlines have the same probability of late flights.

# This distribution can be plotted using familiar techniques,
# including a line to mark alpha=0.01, since our p-value is so small:
pChi_flights<-
  ggplot(data.frame(x = c(0, 50)), aes(x = x)) +
  stat_function(fun = dchisq, args=list(df=15))+
  geom_vline(xintercept=qchisq(0.99, df=15), color="red", linetype="dashed")+
  theme_classic()+
  theme(text=element_text(size=16), 
        plot.title=element_text(hjust=0.5, size=16)) + 
  labs(title="Chisq(df=15) pdf")
pChi_flights

# We can also look at power:
?pwr.chisq.test
effectsize(CT)
pwr.chisq.test(w=0.000588, df=15, N=dim(flights3)[1])

# Unfortunately, our effect size is tiny, and as such our power is very small.
# We are getting a low p-value mostly because the number of observations is so large;
# it's hard to have confidence in these results.


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                     THE F STATISTIC
# 
# The chi-squared statistic is essentially a measure of variation: collective distance from an expectation.
# If we want to know *how much variation* is accounted for in nested expectations on data, we have a statistic named F.
# As you saw in the Crump Lab's writeup of ANOVA, the F statistic is basically
# F = (how much variation we can explain)/(how much variation we can't)
# So how do we get there from the chi-squared?

# In your homework, you had to demonstrate that the ratio of two chi-squared r.v.'s, each divided by their degrees of freedom, was distributed as F(df1, df2).
# We can draw directly from last week's code, when we demonstrated that the sum of k squared N(0,1) r.v.'s was distributed as chisq(df=n).

# Let's start by generating values from a chisq(df=5) and a chisq(df=8), why not. The corresponding F distribution would be F(5,8).
?rchisq
?rf
df1<-5
df2<-8
x1 <- rchisq(1000, df=df1)
x2 <- rchisq(1000, df=df2)
x3<- rf(1000, df1=df1, df2=df2)

# Construct the ratio of chi-squared r.v.'s:
f_5_8<-(x1/df1)/(x2/df2)

#Sanity check:
x1[1]
x1[1]/df1
x2[1]
x2[1]/df2
(x1[1]/df1)/(x2[1]/df2)
f_5_8[1]

# Assemble into a single object for plotting:
myf_5_8<-tibble(data=c(f_5_8, x3),
               distr=c(rep("Ratio", 1000), rep("F", 1000)),
               df=rep(1, 2000))
glimpse(myf_5_8)

# We can plot out as density as before.
# First just each chi-squared:
data.frame(x1) %>%
  ggplot(aes(x=x1))+
  geom_density(color="grey", fill="red", alpha=0.2)+
  geom_vline(aes(xintercept=mean(x1)),
             color="black", linetype="dashed", size=1)+
  theme_classic()+
  theme(text=element_text(size=16), 
        plot.title=element_text(hjust=0.5, size=16)) + 
  labs(title="Chisq(df=5), n=1000")

data.frame(x2) %>%
  ggplot(aes(x=x2))+
  geom_density(color="grey", fill="red", alpha=0.2)+
  geom_vline(aes(xintercept=mean(x2)),
             color="black", linetype="dashed", size=1)+
  theme_classic()+
  theme(text=element_text(size=16), 
        plot.title=element_text(hjust=0.5, size=16)) + 
  labs(title="Chisq(df=8), n=1000")

# And plot the ratio vs. the corresponding F:
myf_5_8 %>%
  ggplot(aes(x=data, colour=distr))+
  geom_density()+
  theme_classic()+
  theme(text=element_text(size=16), 
        plot.title=element_text(hjust=0.5, size=16),
        legend.title = element_blank()) + 
  labs(title="F(5,8) vs Chisq(df=5)/Chisq(df=8), n=1000")

# Cool. We can do this with any df we like:
df1<-2
df2<-20
x1 <- rchisq(1000, df=df1)
x2 <- rchisq(1000, df=df2)
x3<- rf(1000, df1=df1, df2=df2)
f_2_20<-(x1/df1)/(x2/df2)
myf_2_20<-tibble(data=c(f_2_20, x3),
                distr=c(rep("Ratio", 1000), rep("F", 1000)),
                df=rep(1, 2000))
myf_2_20 %>%
  ggplot(aes(x=data, colour=distr))+
  geom_density()+
  theme_classic()+
  theme(text=element_text(size=16), 
        plot.title=element_text(hjust=0.5, size=16),
        legend.title = element_blank()) + 
  labs(title="F(2,20) vs Chisq(df=2)/Chisq(df=20), n=1000")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#           SUM OF SQUARED ERRORS AND LINEAR REGRESSION
# 
# Sum of squared errors calculations - which should be familiar from the (observed-expected)^2 term in the chi-squared test - are important in regression, ANOVA... basically many cases where we want to partition variance into what can be explained by a model and what is left over.
# We will introduce these ideas in the context of simple linear regression to see how model terms are estimated by minimizing "errors" (with some assumptions)and how the variation explained by a model is quantified.

# From the Khan Academy videos, we have the following conditions which must hold for a linear regression to be valid for inference:
#     [L]inear relationship between x and y
#     [I]ndependence of measurements
#     [N]ormality of residuals
#     [E]qual variance of residuals over the range of x
#     [R]andom sample 
# We will keep these in mind as we proceed.

# Let's first see how errors are calculated in this context as a general approach.
# To start, we will create a (fake) data set of number of hours studied and exam score received for 20 different college students.
df <- data.frame(hours=c(1, 1, 1, 2, 2, 2, 2, 2, 3, 3,
                         3, 4, 4, 4, 5, 5, 6, 7, 7, 8),
                 score=c(68, 76, 74, 80, 76, 78, 81, 84, 86, 83,
                         88, 85, 89, 94, 93, 94, 96, 89, 92, 97))

#view first six rows of data frame
head(df)

# Next, we will use the lm() function to fit a linear model of the form 
#           y = b1*x + b0 + error
# where 
#   y (the dependent variable) is exam score
#   x (the predictor or independent variable) is hours studied
#   errors (residuals) are assumed to be normal with a mean of 0 (centered).
model <- lm(score ~ hours, data = df)

#view model summary
summary(model)

# and the coefficients:
model$coefficients

# View the fit:
pTests<-df %>%
  ggplot(aes(x=hours, y=score)) +
  geom_point() +
  theme_classic()
pTests + geom_abline(intercept = model$coefficients[1], slope = model$coefficients[2], color = "blue")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                 LEAST SQUARES ESTIMATION
#
# What's happening here is that the linear model (y = b1*x + b0) is being fitted to these data using the LEAST SQUARES CRITERION.
# There are two basic ways to do this. The most common (and often the only choice, for more complicated problems than this one) is some sort of GRADIENT DESCENT. Let's see how this works. Recall that we are trying to minimize the sum of squared errors between the fitted and real values.
# Let's set up ranges for each of the two parameters in the model (slope and intercept). We'll move along these ranges slowly and calculate SSE as we go.

# ATTENTION: STUPID R TRICKS
# We can use the seq() functions to generate ranges. If we want a range of integers, we have a few options:
range_b0 <- seq(from=40, to=100, by=1)
range_b0 <- seq.int(from=40, to=100)
range_b0 <- seq.int(40, 100)

# For a walk on non-integer values, seq() has the necessary flexibility.
range_b1 <- seq(from=0.5, to=7.5, by=0.1)

# As always, we need storage. We'll generate one SSE for each pair {b0, b1}, so we need:
mysize<-length(range_b0)*length(range_b1)
lm_gradient<-tibble(b0=numeric(mysize), b1= numeric(mysize), mySSE=numeric(mysize))

# Now we can loop and calculate our SSE as we go. 
idx<-1
for(i in seq_along(range_b0)){
  for(j in seq_along(range_b1)){
    yhat<-range_b0[i] + range_b1[j]*df$hours # generate the model-based values yhat
    lm_gradient$mySSE[idx]<-sum((df$score-yhat)*(df$score-yhat))
    lm_gradient$b0[idx]<-range_b0[i]
    lm_gradient$b1[idx]<-range_b1[j]
    idx<-idx+1
  }
}

# Let's take a look at our data and draw the gradient using geom_contour(). This function works best when you have an evenly spaced grid in x and y (which we do). STUPID R TRICKS AHEAD.
lm_gradient

# The basic plot. I don't love this. We can do better.
lm_gradient %>%
  ggplot(aes(x=b0, y=b1, z=mySSE))+
  geom_contour() +
  theme_classic() +
  geom_hline(yintercept=3.25) +
  geom_vline(xintercept = 73.446) # Add lines that intersect at the fitted values

# Setting bins creates evenly spaced contours in the range of the data.
lm_gradient %>%
  ggplot(aes(x=b0, y=b1, z=mySSE))+
  geom_contour(bins=500) + 
  theme_classic() +
  geom_hline(yintercept=3.25) +
  geom_vline(xintercept = 73.446)

p_lm_gradient<-lm_gradient %>%
  ggplot(aes(x=b0, y=b1, z=mySSE, colour=stat(level))) + #Using stat() to delay colour processing until after levels are generated 
  geom_contour(binwidth=2000) + #Or we can set the width of the bins on SSE
  theme_classic() +
  geom_hline(yintercept=3.25) +
  geom_vline(xintercept = 73.446)

p_lm_gradient
p_lm_gradient + scale_color_viridis_c(option = "magma") # We can change the colour palette with the usual options from ggplot2
p_lm_gradient + scale_color_distiller(palette="GnBu")

# The package metR() contains functionality to add values to the contour lines, if we want:
p_lm_gradient + metR::geom_text_contour(aes(z = mySSE)) + theme(legend.position = "none")

# We can mess with the label placement by calling different functions to label.placer (default is label_placer_flattest()).
p_lm_gradient + metR::geom_text_contour(aes(z = mySSE), label.placer = label_placer_fraction(frac=0.5)) + theme(legend.position = "none") #Place the label at the midpoint of the contour
p_lm_gradient + metR::geom_text_contour(aes(z = mySSE), label.placer = label_placer_n(n=2)) + theme(legend.position = "none") #Add n=2 labels per contour

# For filled contour plots, we can instead use geom_contour_filled().
p_lm_gradient_fill<-lm_gradient %>%
  ggplot(aes(x=b0, y=b1, z=mySSE, colour=stat(level)))+
  geom_contour_filled(bins=100) + 
  geom_hline(yintercept=3.25) +
  geom_vline(xintercept = 73.446) +
  theme(legend.position = "none") 
p_lm_gradient_fill
p_lm_gradient_fill + scale_fill_viridis_d(direction=-1)
p_lm_gradient_fill + scale_fill_viridis_d(option="magma")
p_lm_gradient_fill + scale_fill_viridis_d(option="magma", direction=-1)

# Back to the point - we can see that all of these plots show SSE decreasing smoothly as the parameters approach the values actually estimated by lm() of {b0, b1} = {73.45, 3.25}
# However, for simple linear regression, we can find the solution directly. We'll go to the board to explain this.
# As an aside - It is also possible to use other criteria (e.g. maximum likelihood) to produce estimators - but we won't discuss that today.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#           LEAST-SQUARES PARAMETER ESTIMATES
#
# According to the LSE point estimator formulae we derived on the board just now, it should be possible to re-create the estimates for b0 and b1 as:
#         b0 = ybar - b1* xbar
#         b1 = [sum(xi*yi) - n*xbar*ybar]/[sum(xi^2) - n*xbar^2]
# Recall in our data that scores are the dependent variable (y) and hours studying is the independent variable (x).

# First we need to count the number of pairs:
dim(df)
n<-dim(df)[1]

# Then we can calculate our estimators:
df$hours
xy<-df$hours*df$score
xx<-df$hours*df$hours

b1<-(sum(xy) - n*mean(df$hours)*mean(df$score))/(sum(xx)-n*mean(df$hours)^2)
b0<-mean(df$score) - b1*mean(df$hours)

b0
b1
summary(model)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     PROPERTIES OF A REGRESSION LINE
# 1. The sum of the residuals is 0
sum(model$residuals)

# 2. The sum of the squared residuals is a minimum (this follows from the derivation of the estimators).
# 3. The sum of the observed values equals the sum of the fitted (estimated values):
sum(model$fitted.values)
sum(df$score)

# 4. The sum of the predictor-weighted residuals Xi*ei is 0
sum(model$residuals * df$hours)

#5. The sum of the fitted-value-weighted residuals Yi(est) * ei is also 0
sum(model$residuals * model$fitted.values)

# The regression line always goes through the point (xbar, ybar)
pTests + geom_abline(intercept = model$coefficients[1], slope = model$coefficients[2], color = "blue") +
  geom_hline(yintercept = mean(df$score), color="red") +
  geom_vline(xintercept = mean(df$hours), color="red")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#         SUMMARY STATISTICS OF LINEAR REGRESSION
#
# As we used LSE to get these estimators, it is intuitive to break down the variation
# into different SQUARED SUMS over the data.

# There are three sums of squares we are interested in.
# The SUM OF SQUARES REGRESSION (SSR) is
# the sum of squared differences between model-predicted data points (ŷi) 
# and the mean of the response variable (ybar).
ssr <- sum((fitted(model) - mean(df$score))^2)
ssr

# The SUM OF SQUARES ERROR (SSE) is
# the sum of squared differences between the model-predicted data points (ŷi) 
# and the observed (real) data points (yi) - aka the SQUARED RESIDUALS.
sse <- sum((fitted(model) - df$score)^2)
sse
sum(model$residuals^2)

# Note that we have lost two degrees of freedom in calculating b0 and b1.
# The MEAN SQUARE ERROR (MSE), which provides an unbiased estimator of the variance of Yi, 
# can therefore be calculated by dividing SSE/df:
mse<-sse/(n-2)
mse

# and the RSE (residual standard error), an estimator of the error SD, is therefore
sqrt(mse)

# The SUM OF SQUARES TOTAL (SST) is
# the sum of squared differences between individual data points (yi) 
# and the mean of the response variable (ybar).
sst <- sum((df$score-mean(df$score))^2)
sst

# This provides a measure of "total" variation against the center of the data.
# Note that, by implication,
ssr + sse

# We can manually calculate the R-squared from these errors as:
# Rsquared=SSR/SST; aka the fraction of total variation explained by the model.
ssr/sst
summary(model)

# This tells us that 73.48% of the variation in exam scores can be explained by the number of hours studied.
# However, we must use caution in interpreting Rsquared, as this is not the whole story...

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#        Linear Regression in R:
#    Introducing Anscombe's Quartet
#
# Let's talk about conditions for a linear fit.
# Anscombes quartet is a set of 4 (x,y) data sets that were 
# published by Francis Anscombe in a 1973 paper "Graphs in statistical analysis".
# It can be found in R in the datasets library.
# Let's look at the data set:
datasets::anscombe

# Let's load this data set in and assign it to a new object:
data<-datasets::anscombe
glimpse(data)

# We can summarize the data and find correlations using fBasics():
fBasics::basicStats(anscombe)

# or manually using sapply for simplicity
?sapply

# Mean
sapply(1:8, function(x) mean(anscombe[ , x]))

# Variance
sapply(1:8, function(x) var(anscombe[ , x]))

# Correlation between x and y for each pair
sapply(1:4, function(x) cor(anscombe[ , x], anscombe[ , x+4]))

# Let's create scatterplots for each pair of (x,y) data
p1 <- ggplot(anscombe) +
  geom_point(aes(x1, y1), color = "darkorange", size = 1.5) +
  scale_x_continuous(breaks = seq(0,20,2)) +
  scale_y_continuous(breaks = seq(0,12,2)) +
  expand_limits(x = 0, y = 0) +
  labs(x = "x1", y = "y1",
       title = "Dataset 1" ) +
  theme_bw()
p1
p2 <- ggplot(anscombe) +
  geom_point(aes(x2, y2), color = "darkorange", size = 1.5) +
  scale_x_continuous(breaks = seq(0,20,2)) +
  scale_y_continuous(breaks = seq(0,12,2)) +
  expand_limits(x = 0, y = 0) +
  labs(x = "x2", y = "y2",
       title = "Dataset 2" ) +
  theme_bw()
p3 <- ggplot(anscombe) +
  geom_point(aes(x3, y3), color = "darkorange", size = 1.5) +
  scale_x_continuous(breaks = seq(0,20,2)) +
  scale_y_continuous(breaks = seq(0,12,2)) +
  expand_limits(x = 0, y = 0) +
  labs(x = "x3", y = "y3",
       title = "Dataset 3" ) +
  theme_bw()
p4 <- ggplot(anscombe) +
  geom_point(aes(x4, y4), color = "darkorange", size = 1.5) +
  scale_x_continuous(breaks = seq(0,20,2)) +
  scale_y_continuous(breaks = seq(0,12,2)) +
  expand_limits(x = 0, y = 0) +
  labs(x = "x4", y = "y4",
       title = "Dataset 4" ) +
  theme_bw()

# ~~~~~~~~~~~~~~~~~   STUPID R TRICKS ~~~~~~~~~~~~~~~~~~~~~~~~
# This is grid.arrange() from gridExtra; it is similar to plot_grid.
# Both use the underlying grid() package to draw and place objects.
# They are similar, but have some specific differences; 
# grid.arrange doesn't allow you to tweak object alignments, 
# but it does make adding global titles and subtitles easy, for example.
grid.arrange(grobs = list(p1, p2, p3, p4), 
             ncol = 2, 
             top = "Anscombe's Quartet")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Now we will fit linear models to each using lm().
# How do we think this will go?
lm1 <- lm(y1 ~ x1, data = anscombe)
lm2 <- lm(y2 ~ x2, data = anscombe)
lm3 <- lm(y3 ~ x3, data = anscombe)
lm4 <- lm(y4 ~ x4, data = anscombe)

# How do the parameters compare?
lm1
lm2
lm3
lm4

# How do these fits look, according to R2?
summary(lm1)
summary(lm2)
summary(lm3)
summary(lm4)

# Let's plot the fitted lines on the data:
p1_fitted <- p1 + geom_abline(intercept = 3.0001, slope = 0.5001, color = "blue")
p2_fitted <- p2 + geom_abline(intercept = 3.001, slope = 0.500, color = "blue")
p3_fitted <- p3 + geom_abline(intercept = 3.0025, slope = 0.4997, color = "blue")
p4_fitted <- p4 + geom_abline(intercept = 3.0017, slope = 0.499, color = "blue")

grid.arrange(grobs = list(p1_fitted, p2_fitted,
                          p3_fitted, p4_fitted), 
             ncol = 2, 
             top = "Anscombe's Quartet")

# What happened here?
# Recall the conditions for inference from a linear regression:
#
# [L]inear relationship
# [I]ndependence of measurements
# [N]ormality of residuals
# [E]qual variance of residuals over the range of x
# [R]andom sample 
#
# These data are made up, so independence and random sampling aren't relevant. 
# Let's check whether the other conditions are true.
# We can assess linearity of the relationship between x and y from the plots - what do you think?

# To check the others, which are based on the data, we can plot diagnostics.
# Residuals vs. fitted and scale-location should have no trend (flat line)
# and the spread of points shouldn't vary with the fitted values.
# The QQ plot should be normal-ish.
# Residuals vs. leverage might be new;
# Leverage refers to the extent to which the coefficients in the regression model 
# would change if a particular observation was removed from the dataset;
# If any point in this plot falls outside of Cook’s distance (the red dashed lines) 
# then it is considered to be an influential observation.
# Basically we don't want anything in the red; no one point should have outsize influence.

# This is going to pop up a bunch of diagnostic plots in a row.
plot(lm1)

# We can also test the residuals directly for normality with Shapiro-Wilk:
names(lm1)
shapiro.test(lm1$residuals)

# or by plotting density:
d<-density(lm1[['residuals']])
plot(d,main='Residual KDE Plot 1',xlab='Residual value')

# How did the linear model do on the first pair?
# let's try the next pair (x2, y2).
plot(lm2)
d2<-density(lm2[['residuals']])
plot(d2,main='Residual KDE Plot 2',xlab='Residual value')
shapiro.test(lm2$residuals)

# And the others:
plot(lm3)
d3<-density(lm3[['residuals']])
plot(d3,main='Residual KDE Plot 3',xlab='Residual value')
shapiro.test(lm3$residuals)

plot(lm4)
d4<-density(lm4[['residuals']])
plot(d4,main='Residual KDE Plot 4',xlab='Residual value')
shapiro.test(lm4$residuals)

# How do these results look? What does this mean about the regressions?

