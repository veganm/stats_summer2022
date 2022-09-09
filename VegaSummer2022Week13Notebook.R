#' ---
#' title: "Week 13 Notebook"
# Knit back from the .Rmd using command
# knitr::purl("VegaSummer2022Week13Notebook.Rmd", documentation = 2)
#' ---
#' 
#' This week we will continue unpacking the mysteries of the chi-squared and F distributions. We will move our discussion of linear regression into a discussion of ANOVA as a partition of variance.
#' 
#' * Background information is from:
#'   + https://www.statology.org/sst-ssr-sse-in-r/
#'   + https://www.khanacademy.org/math/statistics-probability/sampling-distributions-library/sample-means/v/statistics-sample-vs-population-mean
#'   + https://www.khanacademy.org/math/statistics-probability/summarizing-quantitative-data#variance-standard-deviation-sample
#'   + https://www.khanacademy.org/math/statistics-probability/analysis-of-variance-anova-library
#'   + https://www.youtube.com/watch?v=KkaU2ur3Ymw
#'   + https://towardsdatascience.com/why-is-the-sample-variance-distributed-with-n-1-degrees-of-freedom-c9edcdada28b
#'   + https://statistics.laerd.com/statistical-guides/one-way-anova-statistical-guide-3.php
#'   + https://www.statisticshowto.com/hierarchical-model/
#'   + https://statisticsbyjim.com/anova/welchs-anova-compared-to-classic-one-way-anova/
#' 
#' * with code etc. from:
#'   + https://www.statology.org/sst-ssr-sse-in-r/ 
#'   + https://www.scribbr.com/statistics/anova-in-r/
#'   + https://stackoverflow.com/questions/26049762/erroneous-nesting-of-equation-structures-in-using-beginalign-in-a-multi-l
#'   + http://www.sthda.com/english/wiki/two-way-anova-test-in-r
#'   + http://www.sthda.com/english/wiki/manova-test-in-r-multivariate-analysis-of-variance
#' 
#' **********************
## ---------------------------------------------------------------------------------------------------------------------------------
# As usual we will load the libraries we need:
pacman::p_load(tidyverse, ggpubr, cowplot, grid, 
               gridExtra, datasets, multcomp, car, mvnormtest)


#' 
#' Last time, we carried out linear regression on a fake data set of number of hours studied and exam score received for 20 different college students.
#' 
## ---------------------------------------------------------------------------------------------------------------------------------
df <- data.frame(hours=c(1, 1, 1, 2, 2, 2, 2, 2, 3, 3,
                         3, 4, 4, 4, 5, 5, 6, 7, 7, 8),
                 score=c(68, 76, 74, 80, 76, 78, 81, 84, 86, 83,
                         88, 85, 89, 94, 93, 94, 96, 89, 92, 97))

#view first six rows of data frame
head(df)

#' 
#' Next, we used the *lm()* function to fit a linear model of the form $y = b1*x + b0 + error$, where 
#' 
#'    y (the dependent variable) is exam score
#' 
#'    x (the predictor or independent variable) is hours studied
#' 
#'    errors (residuals) are assumed to be normal with a mean of 0 (centered).
#' 
## ---------------------------------------------------------------------------------------------------------------------------------
model <- lm(score ~ hours, data = df)
#view model summary
summary(model)

#' 
#' We can view different components of the resulting linear model:
#' 
## ---------------------------------------------------------------------------------------------------------------------------------
#view model summary
summary(model)

## ---------------------------------------------------------------------------------------------------------------------------------
# and the coefficients:
model$coefficients

## ---------------------------------------------------------------------------------------------------------------------------------
# Plot the fit:
pTests<-df %>%
  ggplot(aes(x=hours, y=score)) +
  geom_point() +
  theme_classic()
pTests + geom_abline(intercept = model$coefficients[1], 
                     slope = model$coefficients[2], color = "blue")

#' 
#' 
#' #### R TRICKS:
#' If we weren't going to get into it with the analysis and just wanted to visualize the linear correlation, we could create and plot a smoothed conditional mean within the *ggplot* call using *geom_smooth()*. Here we assume we have a linear correlation between x and y, hence the use of *lm* as the method; the function can also use *glm*, *gam*, *loess*, or another function if explicitly called from its home package (e.g. *MASS::rlm*).
## ---------------------------------------------------------------------------------------------------------------------------------
df %>%
  ggplot(aes(x=hours, y=score)) +
  geom_point() +
  geom_smooth(formula="y~x", method="lm", se=FALSE)+
  theme_classic()

#' 
#' We can also tell this function that yes, we would like to see a confidence interval (default level is 0.95, but you can change this):
## ---------------------------------------------------------------------------------------------------------------------------------
df %>%
  ggplot(aes(x=hours, y=score)) +
  geom_point() +
  geom_smooth(formula="y~x", method="lm", se=TRUE, level=0.9)+
  theme_classic()

#' 
#' Today, however, we are going to do exactly that. We will review the squared and mean-squared sums that are relevant for partitioning the variation in this linear model fit, then we will see how these quantities are used to create the test statistics that we see in the summary: t-statistics for the estimators of the model parameters, and a F statistic for the overall variation explained by the model.
#' 
#' 
#' *********
#' ###         SSE, VARIANCE, AND CHI-SQUARED
#' 
#' Recall that the total number of degrees of freedom provided by a data set is the dimension of the data, or the number of observations *n*, and that degrees of freedom are lost when we calculate estimators.
#' 
#' Last week, we dissected the sum of squares total (SST) into SSE and SSR, and mentioned that each SS divided by its degrees of freedom becomes a MEAN SQUARES term. 
#' We'll revisit that briefly.
#' Recall that in this notation, $x_i$ ("x sub i") and $y_i$ ("y sub i") are the *i*th values in the data set containing *n* total observations of paired (x,y) values. $\bar{x}$ and $\bar{y}$ ("x-bar" and "y-bar") are the averages of all values for *x* and *y* respectively.
#' Recall also that in our data that scores are the dependent variable (y) and hours studying is the independent variable (x).
#' 
#' Let's recall also the constraints on linear regression, all of which follow directly from how the slope and intercept are calculated.
#' 
#' 1. The sum of the residuals is 0:
## ---------------------------------------------------------------------------------------------------------------------------------
sum(model$residuals)

#' 
#' 2. The sum of the squared residuals is a minimum (this follows from the derivation of the estimators).
#' 
#' 3. The sum of the observed values equals the sum of the fitted (estimated values):
## ---------------------------------------------------------------------------------------------------------------------------------
sum(model$fitted.values)
sum(df$score)

#' 
#' 4. The sum of the predictor-weighted residuals X~i~*e~i~ is 0:
## ---------------------------------------------------------------------------------------------------------------------------------
sum(model$residuals * df$hours)

#' 
#' 5. The sum of the fitted-value-weighted residuals $Y_i(est) * e_i$ is also 0:
## ---------------------------------------------------------------------------------------------------------------------------------
sum(model$residuals * model$fitted.values)

#' 
#' 6. The sum of errors between the fitted values and the average of the observed values for the response variable is 0 ($\sum(\hat{y}_i-\bar{y}) = 0$):
## ---------------------------------------------------------------------------------------------------------------------------------
sum(fitted(model) - mean(df$score))

#' 
#' 7. The regression line always goes through the point $(\bar{x}, \bar{y})$:
## ---------------------------------------------------------------------------------------------------------------------------------
pTests<-df %>%
  ggplot(aes(x=hours, y=score)) +
  geom_point() +
  theme_classic()
pTests + geom_abline(intercept = model$coefficients[1], slope = model$coefficients[2], color = "blue") +
  geom_hline(yintercept = mean(df$score), color="red") +
  geom_vline(xintercept = mean(df$hours), color="red")

#' 
#' 
#' 
#' OK, now on to sums of squares. First we need to count the number of pairs:
#' 
## ---------------------------------------------------------------------------------------------------------------------------------
dim(df)
n<-dim(df)[1]
n

#' 
#' (1) The SUM OF SQUARES REGRESSION (SSR) is the sum of squared differences between model-predicted data points ($\hat{y}_i$, or "y-hat sub i") and the mean of the response variable (y-bar, or $\bar{y}$).
## ---------------------------------------------------------------------------------------------------------------------------------
ssr <- sum((fitted(model) - mean(df$score))^2)
ssr

#' 
#' (1a) There are *n* predicted points, but they are all from the same model. This model has 2 parameters and is associated with 2 df; however, one of these df is lost in this calculation because of the constraint that $\sum(\hat{y}_i-\bar{y}) = 0$ (see constraint 6 above). The df here is therefore 1, and the MEAN SQUARE REGRESSION is:
## ---------------------------------------------------------------------------------------------------------------------------------
msr<-ssr/1
msr

#' 
#' 
#' (2) The SUM OF SQUARES ERROR (SSE) is the sum of squared differences between the model-predicted data points $\hat{y}_i$ and the observed (real) data points $y_i$ - aka the SQUARED RESIDUALS.
#' 
## ---------------------------------------------------------------------------------------------------------------------------------
sse <- sum((fitted(model) - df$score)^2)
sse
sum(model$residuals^2)

#' 
#' Note that there are *n* real data points, and we have lost two degrees of freedom in these data in calculating b~0~ and b~1~.
#' (2a) The MEAN SQUARE ERROR (MSE), which provides an unbiased estimator of the variance of Y~i~,can therefore be calculated by dividing SSE/df:
#' 
## ---------------------------------------------------------------------------------------------------------------------------------
mse<-sse/(n-2)
mse

#' 
#' 
#' (3) The SUM OF SQUARES TOTAL (SST) is the sum of squared differences between individual data points (y~i~) and the mean of the response variable ($\bar{y}$).
#' 
## ---------------------------------------------------------------------------------------------------------------------------------
sst <- sum((df$score-mean(df$score))^2)
sst

#' 
#' Note that there are *n* real data points, and we have lost one degrees of freedom in these data in calculating the mean, for a total *n-1* degrees of freedom. (We don't need to calculate the mean total error here, as we won't be using it.)
#' 
#' Note that these terms are cumulative:
## ---------------------------------------------------------------------------------------------------------------------------------
# Sum the SS associated with regression and error:
ssr + sse

#' 
## ---------------------------------------------------------------------------------------------------------------------------------
# Sum the degrees of freedom associated with regression and error:
(n-2) + 1

#' 
#' 
#' *********
#' ###         DISTRIBUTION OF THE ESTIMATORS b0 AND b1
#'  
#' Recall that {b~0~, b~1~} are ESTIMATORS of the true intercept and slope {B~0~, B~1~}. Since samples are subject to, well, sampling error, we want to know:
#' 
#'  a) how confident we are in these estimates (generate an interval); and
#' 
#'  b) whether the values are significantly different from 0, especially for the slope b~1~.
#' 
#' The point estimators were obtained last week as:
#' 
#' $$         
#' \begin{aligned} 
#' b_0&=\bar{y}-b_1\bar{x}\\
#' b_1&=\frac{\sum_{i=1}^{n}(x_iy_i)-n\bar{x}\bar{y}}{\sum_{i=1}^{n}(x_i^2)-n\bar{x}^2}
#' \end{aligned}
#' $$ 
#' 
#' Let's focus on b~1~ for a minute. The SAMPLING DISTRIBUTION of b~1~ refers to the different values of b~1~ that would be obtained with repeated sampling, when the levels of the predictor X are kept the same.
#' 
#' Assuming (as we have done) that errors are normal, then the mean of b~1~ is $E \{b_1\} = \beta_1$, and we can show that, if $var(y_i) = \sigma^2$, then $var \{b_1 \} = \sigma^2/\sum(X_i-\bar{X})^2$, and the estimator for this variance is obtained by replacing (sigma^2) with MSE, its unbiased estimator:
#'        $s^2 \{b_1\} = MSE/\sum(X_i-\bar{X})^2$
#' 
#' If all this is true, then we can generate a STUDENTIZED STATISTIC for b1, where:
#'      $(b_1-\beta_1)/s\{b_1\} ~ \sim \mathcal{t}(n-2)$ and 
#'      $SSE/(\sigma^2) \sim \chi^2 (n-2)$
#' 
#' This can be used to generate confidence intervals for b~1~. First we generate $s^2$:
#' 
## ---------------------------------------------------------------------------------------------------------------------------------
temp<-(df$hours-mean(df$hours))*(df$hours-mean(df$hours))
s2<-mse/sum(temp)
sqrt(s2)
summary(model)


#' 
#' And since  $(b_1-\beta_1)/s\{b_1\} ~ \sim \mathcal{t}(n-2)$, and since the Student's T distribution is symmetric around 0, we can calculate a symmetric CI for a given $\alpha$:
#' 
## ---------------------------------------------------------------------------------------------------------------------------------
alpha<-0.05
bint<-qt(p=(1-alpha/2), df=n-2)
b1<-model$coefficients[2]
b1 + bint*sqrt(s2)
b1 - bint*sqrt(s2)

#' 
#' The statistic, under H~0~: B~1~=0, becomes $t^* = b_1/s\{b_1\}$:
## ---------------------------------------------------------------------------------------------------------------------------------
tstar<-b1/sqrt(s2)
tstar
dt(tstar, df=n-2)

#' 
#' 
#' We can do the same for B~0~, if we care to (generally we won't). The sampling distribution of b~0~ is accordingly normal, with mean, variance, and estimated variance:
#' 
#' $$
#' \begin{aligned} 
#' E\{b_0 \}&=\beta_0\\
#' var\{b_0\}&=\sigma^2*(\frac{1}{n}+\frac{\bar{x}^2}{\sum{(x_i-\bar{x})^2}})\\
#' s^2\{b_0\}&=MSE*(\frac{1}{n}+\frac{\bar{x}^2 }{\sum{(x_i-\bar{x})^2}})
#' \end{aligned}
#' $$ 
#' 
## ---------------------------------------------------------------------------------------------------------------------------------
xbar<-mean(df$hours)
temp<-sum((df$hours-xbar)*(df$hours-xbar))
s2_b0 <-mse* (1/n + (xbar^2)/temp) 
sqrt(s2_b0)

#' 
#' and the studentized statistic $(b_0-\beta_0)/s\{b_0\} ~ \sim \mathcal{t}(n-2)$.
#' 
## ---------------------------------------------------------------------------------------------------------------------------------
b0<-model$coefficients[1]
tstar<-b0/sqrt(s2_b0)
tstar
dt(tstar, df=n-2)

#' 
#' *********
#' ###         ANOVA and Linear Regression: Mean Squared Deviations, Chi-Squared, and F
#' 
#' The t-tests are useful for determining significance of the individual parameters, which is fine for the simple linear model since the relationship between x and y is summarized in the single parameter $b_1$. However, as the models become more complicated, we will need to assess the overall utility of a given model against the data. So long as the model assumptions hold, this can be done with an ANOVA: Analysis Of Variance.
#' 
#' Broadly, ANOVA tests whether the mean value for a single quantitative dependent variable changes over levels of one or more independent variables. For simple linear regression, this is equivalent to a test of the null hypothesis that $\beta_1 = 0$ (e.g. is there a non-zero linear relationship between y and x; is the mean of $y_i$ a linear function of x).
#' 
## ---------------------------------------------------------------------------------------------------------------------------------
# Note that we can analyze the regression object directly:
test.aov<-aov(model)
summary(test.aov)

## ---------------------------------------------------------------------------------------------------------------------------------
# or perform ANOVA on the data:
test.aov<-aov(score~hours, data=df)
summary(test.aov)

#' 
## ---------------------------------------------------------------------------------------------------------------------------------
# Sanity check - are these our sums and means squared?
ssr
sse
mse

#' 
#' To move forward, we need to know the expectations for MSE and MSR. Since (as we said above) MSE is an unbiased estimator of the variance of Y~i~ (recall that we are expecting this to be a constant, since the variance of the residuals is supposed to be independent of *x* per the assumptions of linear regression), this means that $E[MSE]=E[\frac{SSE}{n-2}]=\sigma^2$. 
#' It is also true (we'll skip the proof today) that $E[MSR] = E[SSR/1] = \sigma^2 + \beta^2_1 \sum(X_i -\bar{X})^2$; notably, this simplifies to $\sigma^2$ when $\beta_1 = 0$. 
#' 
#' This suggests a comparison. If MSR and MSE are of the same order of magnitude (which will happen when $\beta_1 = 0$, so the expected value for both mean squares is $\sigma^2$), then we will get a test statistic near 1 (the central F distribution with any degrees of freedom always has a mean of 1), and we do not reject the null hypothesis that $\beta_1 = 0$. If, however, MSR > MSE, then $\beta_1 \neq 0$ (e.g. there is a non-zero linear relationship between x and y). This is the basic idea underlying the ANOVA.
#' 
#' The ANOVA uses a F test. We have implied that the statistic is MSR/MSE; when the ratio is large, the probability of getting that number from a F(1, n-2) distribution will be small. This implies - but it remains to be shown - that MSR/MSE is distributed as F(1, n-2) under the null hypothesis that $\beta_1 = 0$. 
#' 
#' For this to hold, it should be the case that when H0 is true, then $\frac{MSR}{MSE}=\frac{SSR/1}{SSE/n-2}~\sim \frac{\chi^2(1)/1}{\chi^2(n-2)/(n-2)}$. The critical value for the test will therefore be $F_{crit} = F(1-\alpha; 1, n-2)$, where if our test statistic F* > F~crit~, we will reject the null hypothesis that $\beta_1 = 0$. 
#' 
#' Let's visualize this. Recall that the mean of a $\chi^2_r$ distribution is *r*, the degrees of freedom. If X is distributed as $\chi^2_r$, then $\frac{X}{r}~\sim\chi^2_1$:
## ---------------------------------------------------------------------------------------------------------------------------------
# Simulate values from a chi-squared
chi18<-rchisq(1000, df=18)
chi18_r<-chi18/18
mychi18<-tibble(data=c(chi18, chi18_r),
               distr=c(rep("Chi18", 1000), rep("Chi18_18", 1000)))
mychi18 %>%  # Plot out as density
  ggplot(aes(x=data, colour=distr)) +
  geom_density()+
  theme_classic()+
  theme(text=element_text(size=16), 
        plot.title=element_text(hjust=0.5, size=16),
        legend.title = element_blank()) + 
  facet_wrap(vars(distr))

#' When we create the F statistic, under the null hypothesis, both the numerator and denominator have the same expected value of $\sigma^2$. Hence, the null hypothesis F statistic has an expectation of 1. If the statistic we observe is large (MSR>MSE, and therefore MSR/MSE > 1), the value for our statistic will be "surprising", and we reject the null hypothesis.
## ---------------------------------------------------------------------------------------------------------------------------------
# Code from last week's homework solution, 
# demonstrating emergence of the F 
# from the chi-squared distributions
df1<-1
df2<-18
x1 <- rchisq(1000, df=df1)
x2 <- rchisq(1000, df=df2)
x3<- rf(1000, df1=df1, df2=df2)

# Construct the ratio of chi-squared r.v.'s:
f_1_18<-(x1/df1)/(x2/df2)

# What are the means?
mean(f_1_18)
mean(x3)

## ---------------------------------------------------------------------------------------------------------------------------------
# Assemble into a single object for plotting:
myf_1_18<-tibble(data=c(f_1_18, x3),
               distr=c(rep("Ratio", 1000), rep("F", 1000)),
               df=rep(1, 2000))
# And plot the ratio vs. the corresponding F:
myf_1_18 %>%
  ggplot(aes(x=data, colour=distr))+
  geom_density()+
  geom_vline(xintercept=1, colour="red")+
  theme_classic()+
  theme(text=element_text(size=16), 
        plot.title=element_text(hjust=0.5, size=16),
        legend.title = element_blank()) + 
  labs(title="F(1,18) vs (Chisq(1)/1)/(Chisq(18)/18), n=1000")

#' 
#' As a special note - whatever the true value of $\beta_1$, if the rest of the test assumptions hold, then SSR and SSE are independent of each other, and $\frac{SSE}{\sigma^2}$ is distributed as $\chi^2$. However, $\frac{SSR}{\sigma^2}$ will be distributed as our familiar $\chi^2$ r.v. only when $\beta_1 = 0$. If this is not true, therefore, the test statistic that we calculate will not actually be from a F(df1, df2). This means that the value for this statistic may easily be "surprising" (low probability of occurring) under the assumption that it is from that distribution, allowing us to reject the null hypothesis at some reasonable $\alpha$. (If the rest of the test assumptions hold and if $\beta_1\neq0$, then it turns out that the test statistic still has a known distribution, called the *non-central F distribution*, but that's a bit of a mess, and we don't have to go into the details to understand what's happening here.)
#' 
#' For the current data, then:
#' 
## ---------------------------------------------------------------------------------------------------------------------------------
alpha<-0.05
fcrit<-qf(p=(1-alpha), df1=1, df2=n-2)
fsim<- rf(1000, df1=1, df2=n-2)

# Plot out
data.frame(fsim) %>%
  ggplot(aes(x=fsim))+
  geom_density(color="grey", fill="red", alpha=0.2)+
  geom_vline(aes(xintercept=fcrit),
             color="black", linetype="dashed", size=1)+
  theme_classic()+
  theme(text=element_text(size=16), 
        plot.title=element_text(hjust=0.5, size=16)) + 
  labs(title="F(1,18), n=1000")


#' Recall that the F statistic generated for our linear fit was 49.88, which is off this plot, consistent with the low p-value of ~1e-6.
#' 
#' This result indicates that we should reject the null hypothesis. The *stated* null is that $\beta_1 = 0$, but remember that the null hypothesis also includes all of the assumptions of the test itself. For the ANOVA, we are assuming that:
#' 
#' + the observations $Y_i$ represent independent random samples
#' + the relationship between the independent and dependent variables is actually linear
#' + the residuals are normally distributed as $N(0,\sigma^2)$ (e.g. centered around the regression line, with a constant shared variance; for this to be true, the variance of the data at each level/within each group must be identical and independent of the mean)
#' 
#' If ANY of these are untrue, our terms in the statistic aren't both actually chi-squared, and the test statistic cannot be from a F(df1, df2). If the assumptions of the test hold, then this will happen when $\beta_1 \neq 0$ - which is what we want to happen!
#' (It turns out that in this case, the MSE/$\sigma^2$ is actually distributed as a non-central $\chi^2$ r.v. and the ratio is distributed as a non-central F, so we can actually talk about the distribution of this statistic under the alternate hypothesis, but we don't need to go into the details right now.)
#' 
#' If the assumptions of the test do not hold, however, we have a problem. Now we can't assume that the things we wanted to assume about our mean-squares are true. Our test statistic cannot be distributed how we think it is, and we are not actually testing the stated null hypothesis. The results of the F test and the ANOVA become uninterpretable; this test no longer tells us something meaningful about our data.
#' 
#' In practice, the ANOVA is pretty robust to violations of the normality assumption, *as long as the number of points per group is equal*. When this is not true, ANOVA is in particular sensitive to violations of the assumption that variance is homogeneous. Further (as we saw in the linear regression examples from Anscombe's Quartet), this test (like any test of means) is very sensitive to outliers and extreme values. And - this is something of a universal rule - since ANOVA decomposes variance but does not consider co-variance, this test will not perform well if your data points are not actually an independent random sample.
#' 
#' ***********
#' ### Variants of ANOVA and Simple Linear Regression
#' 
#' #### Multiple Regression
#' 
#' What we have discussed here is the simple one-sample or one-way ANOVA, for data with one (numeric) dependent variable and one independent or "predictor" variable. For any number of (independent) predictor variables, the linear regression can be expanded to a *multiple regression*, and a F test can be used to test the null hypothesis that all of the linear relationship terms are 0. If we reject H0 for a multiple regression, we obviously will have some more work to do from here! This is a long story by idself, so we'll leave this here for now.
#' 
#' #### Two-Way ANOVA
#' 
#' If you have two independent variables, you can use a *two-way ANOVA*. The basic test requires a *balanced design* where all groups in the data contain the same number of observations. Let's see this in action.
#' 
#' Here, we’ll use the built-in R data set named *ToothGrowth*. It contains data from a study evaluating the effect of vitamin C on tooth growth in guinea pigs. The experiment has been performed on 60 pigs, where each animal received one of three dose levels of vitamin C (0.5, 1, and 2 mg/day) by one of two delivery methods, orange juice (OJ) or purified ascorbic acid (a form of vitamin C, coded as VC). Tooth length was measured, and these data are the dependent variable.
#' 
## ---------------------------------------------------------------------------------------------------------------------------------
# Store the data in the variable my_data
my_data <- ToothGrowth

# Show a random sample
set.seed(1234)
dplyr::sample_n(my_data, 10)

#' 
#' 
## ---------------------------------------------------------------------------------------------------------------------------------
# Check the structure
str(my_data)

#' 
#' Let's convert these data to a form where two-way ANOVA makes sense. Instead of having *dose* as a numeric, let's make this a factor:
## ---------------------------------------------------------------------------------------------------------------------------------
# Convert dose as a factor and recode the levels
# as "D0.5", "D1", "D2"
my_data$dose <- factor(my_data$dose, 
                  levels = c(0.5, 1, 2),
                  labels = c("D0.5", "D1", "D2"))
head(my_data)

#' 
#' We want to know if tooth length depends on the supplement and/or dose. Note that this is a *full factorial* design, where all levels of *dose* are present for each level of *supplement*. Further, we have the same number of observations at all levels - this is called a *balanced design*, and it is a good thing when we can manage it. We can see the balanced design by generating a frequency table:
#' 
## ---------------------------------------------------------------------------------------------------------------------------------
table(my_data$supp, my_data$dose)

#' We can generate some summary statistics for the data:
## ---------------------------------------------------------------------------------------------------------------------------------
group_by(my_data, supp, dose) %>%
  summarise(
    count = n(),
    mean = mean(len, na.rm = TRUE),
    sd = sd(len, na.rm = TRUE)
  )

#' 
#' 
#' #### R TRICKS AHEAD.
#' Let's visualize the data just to get an idea of what we are dealing with. First a quick boxplot:
## ---------------------------------------------------------------------------------------------------------------------------------
ggboxplot(my_data, x = "dose", y = "len", color = "supp",
          palette = c("#00AFBB", "#E7B800"))

#' And a similarly quick line plot. Note that "add" is a call that ggline() knows; here we can tell ggplot to add more stuff to the plot (default is "none"; see ?ggline and ?desc_statby for more details).
## ---------------------------------------------------------------------------------------------------------------------------------
# Line plots with multiple groups
# +++++++++++++++++++++++
# Plot tooth length ("len") by groups ("dose")
# Color box plot by a second group: "supp"
# Add error bars: mean_se
# (other values include: mean_sd, mean_ci, median_iqr, ....)
# Needs library("ggpubr")
ggline(my_data, x = "dose", y = "len", color = "supp",
       add = c("mean_se", "dotplot"),
       palette = c("#00AFBB", "#E7B800"))

## ---------------------------------------------------------------------------------------------------------------------------------
# Another option for this plot:
ggline(my_data, x = "dose", y = "len", color = "supp",
       add = c("median_q1q3", "jitter"),
       palette = c("#00AFBB", "#E7B800"))

#' Base R has something called an "interaction plot" that can be useful here:
## ---------------------------------------------------------------------------------------------------------------------------------
interaction.plot(x.factor = my_data$dose, trace.factor = my_data$supp, 
                 response = my_data$len, fun = mean, 
                 type = "b", legend = TRUE, 
                 xlab = "Dose", ylab="Tooth Length",
                 pch=c(1,19), col = c("#00AFBB", "#E7B800"))

#' OK, now to the ANOVA. We want to know if tooth length depends on supp and dose. We can perform the two-way ANOVA with the same *aov()* function as before to evaluate an *additive model* where each of the two factors are considered independently.
## ---------------------------------------------------------------------------------------------------------------------------------
res.aov2 <- aov(len ~ supp + dose, data = my_data)
summary(res.aov2)

#' 
#' We can also propose a model with an interaction effect by using an asterisk instead of a plus sign, or by spelling out the interaction:
## ---------------------------------------------------------------------------------------------------------------------------------
# Two-way ANOVA with interaction effect
# These two calls are equivalent
res.aov3 <- aov(len ~ supp * dose, data = my_data)
res.aov3 <- aov(len ~ supp + dose + supp:dose, data = my_data)
summary(res.aov3)

#' Note that if the interaction is not significant, this simplifies to the additive model.
#' 
#' Before proceeding, it is a good idea to check model diagnostics and see whether our fits are legit. First we check for homogeneity of variance using a residuals vs. fit plot:
## ---------------------------------------------------------------------------------------------------------------------------------
plot(res.aov3, 1)

#' The labeled points (23, 32, 49) are possible outliers. As ANOVA is sensitive to outliers, it may make sense to remove these points - but ANOVA is also sensitive to inhomogeneity of variances if we don't have a balanced design, which we will lose if we remove these points. Since the outliers aren't at all extreme, we'll leave them in.
#' 
#' We can test for homogeneity of variance directly using Levene's test from the *cars* package:
## ---------------------------------------------------------------------------------------------------------------------------------
library(car)
leveneTest(len ~ supp*dose, data = my_data)

#' Based on this result, we cannot reject the null hypothesis of equal variance between groups; our variance does appear to be homogeneous, which is what we want here.
#' 
#' We can check normality of residuals by plotting as usual:
## ---------------------------------------------------------------------------------------------------------------------------------
plot(res.aov3, 2)

#' and using the Shapiro-Wilk test:
## ---------------------------------------------------------------------------------------------------------------------------------
# Extract the residuals
aov_residuals <- residuals(object = res.aov3)
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals )

#' So far so good. Let's flip through the rest of the diagnostic plots to make sure we're OK:
## ---------------------------------------------------------------------------------------------------------------------------------
plot(res.aov3, 3)
plot(res.aov3, 4)
plot(res.aov3, 5)

#' 
#' Since there are no obvious violations, we can proceed to interpretation. From the ANOVA results, you can conclude the following, based on the p-values and a significance level of 0.05:
#' 
#' + the p-value of supp is 0.000429 (significant), which indicates that the levels of supp are associated with significant different tooth length.
#' + the p-value of dose is < 2e-16 (significant), which indicates that the levels of dose are associated with significant different tooth length.
#' + the p-value for the interaction between supp*dose is 0.02 (significant), which indicates that the relationships between dose and tooth length depends on the supp method.
#' 
#' However, at this point we don't know *which pairs of means* are significantly different. To assess this, we must perform *post hoc testing*. For ANOVA, we will use Tukey's tests. 
#' 
#' Tukey HSD (Tukey Honest Significant Differences, R function: TukeyHSD()) performs multiple pairwise-comparison between the means of groups. The function *TukeyHD()* takes the fitted ANOVA as an argument.
#' 
#' Note that we don’t need to perform the test for the “supp” variable because it has only two levels, which are significantly different by ANOVA test. Therefore, the Tukey HSD test will be done only for the factor variable “dose”.
## ---------------------------------------------------------------------------------------------------------------------------------
TukeyHSD(res.aov3, which = "dose")

#' All pairwise comparisons among doses are significant with an adjusted p-value < 0.05.
#' 
#' The *multcomp* package includes functions specifically for multiple comparisons testing for ANOVA. *glht* stands for general linear hypothesis tests. The simplified format is *glht(model, lincft)*, where *model* is a fitted model, such as an object returned by *aov()*; and *lincft()* is a specification of the linear hypotheses to be tested. Multiple comparisons in ANOVA models are specified by objects returned from the function *mcp()*.
#' 
#' We can use *glht()* to perform multiple pairwise comparisons, specifying the Tukey test for an ANOVA:
## ---------------------------------------------------------------------------------------------------------------------------------
summary(glht(res.aov2, linfct = mcp(dose = "Tukey")))

#' 
#' #### MANOVA
#' 
#' If you have two dependent variables measured on the same samples, instead you may want a *MANOVA (Multivariate ANalysis Of Variance)*. For example, we may conduct an experiment where we give two treatments (A and B) to two groups of mice, and we are interested in the weight and height of mice. In that case, the weight and height of mice are two dependent variables, and our hypothesis is that both together are affected by the difference in treatment. A multivariate analysis of variance could be used to test this hypothesis.
#' 
#' As with other forms of ANOVA, MANOVA has expectations:
#' 
#' + The dependent variables should be normally distributed within groups. The R function mshapiro.test( )[in the mvnormtest package] can be used to perform the Shapiro-Wilk test for multivariate normality.
#' + Variances must be homogeneous across the range of predictors.
#' + There must be linearity between all pairs of dependent variables, all pairs of covariates, and all dependent variable-covariate pairs in each cell.
#' 
#' 
#' If the global multivariate test is significant, we conclude that the corresponding effect (treatment) is significant. In that case, the next question is to determine if the treatment affects only the weight, only the height or both. In other words, we want to identify the specific dependent variables that contributed to the significant global effect.
#' 
#' To answer this question, we can use one-way ANOVA (or univariate ANOVA) to examine separately each dependent variable.
#' 
#' We'll use the iris data set that we saw earlier:
## ---------------------------------------------------------------------------------------------------------------------------------
iris_data <- as_tibble(iris)
glimpse(iris_data)

#' We will conduct MANOVA using sepal length and petal length as observations. First, as usual, we will plot out to see what we are dealing with.
#' 
#' First, some code that doesn't work. The notebook won't compile if this is executable, so I will leave it here:
#' 
#' > iris_data %>%
#'   select(Sepal.Length, Petal.Length, Species) %>%
#'   pivot_longer(!Species, names_to="feature", values_to = "length") %>%
#'   ggplot(aes(x=Species, y=length, color = feature)) +
#'   geom_jitter(width=0.1)+
#'   theme_classic()
#'   
#' And the output, if you try this, is:
#' 
#' > Error in select(., Sepal.Length, Petal.Length, Species) : 
#'   unused arguments (Sepal.Length, Petal.Length, Species)
#'   
#' It turns out that some functions in *dplyr* don't like it when names have dots in them. We could rename the columns, or we can go back to basics and subset by number instead of name:
## ---------------------------------------------------------------------------------------------------------------------------------
iris_data[,-c(2,4)] %>%
  pivot_longer(!Species, names_to="feature", values_to = "length") %>%
  ggplot(aes(x=Species, y=length, color = feature)) +
  geom_jitter(width=0.1)+
  theme_classic()

#' I am not convinced that these groups have equal variances. Let's look again:
## ---------------------------------------------------------------------------------------------------------------------------------
temp<-iris_data[,-c(2,4)] %>%
  pivot_longer(!Species, names_to="feature", values_to = "length")
temp$feature<-as.factor(temp$feature)
temp %>%
  ggplot(aes(x = Species, y = length, color=Species)) +
  geom_boxplot()+
  geom_jitter(width=0.1) +
  theme_classic()+
  facet_wrap(vars(feature))

#' Well, the variances DEFINITELY aren't equal, but we're going to proceed anyway and see what happens. Let's carry out the MANOVA:
## ---------------------------------------------------------------------------------------------------------------------------------
# MANOVA test
res.man <- manova(cbind(Sepal.Length, Petal.Length) ~ Species, data = iris)
summary(res.man)

#' 
#' We can see which groups differ, per the MANOVA:
## ---------------------------------------------------------------------------------------------------------------------------------
summary.aov(res.man)

#' Before we make any conclusions, we want to know that the assumptions are OK. MANOVA assumes multivariate normality, which can be assessed using *mshapiro.test()* from *mvnormtest*:
#' 
## ---------------------------------------------------------------------------------------------------------------------------------
mshapiro.test(t(iris_data[,c(1,3)]))

#' While *plot()* isn't implemented for MANOVA yet, we can pull the residuals and qqplot them ourselves:
## ---------------------------------------------------------------------------------------------------------------------------------
dfr<-data.frame(residuals=res.man$residuals)
glimpse(dfr)

#' Note that the object contains residuals for each of the dependent variables! We have to pivot this to use the simple QQ plot:
## ---------------------------------------------------------------------------------------------------------------------------------
dfr <- dfr %>%
  pivot_longer(everything(), names_to="feature")
ggqqplot(dfr, x="value")

## ---------------------------------------------------------------------------------------------------------------------------------
shapiro.test(dfr$value)

#' From the output above, it can be seen that the two variables are highly significantly different among species of iris. However, the QQ plot is a little weird, and the Shapiro-Wilks test rejects normality of the residuals - we can reasonably have doubts about the appropriateness of the model.
#' 
#' ***********
#' ### Violation of Assumptions for ANOVA: What Now?
#' 
#' If your data are not suitable for ANOVA, there are a few things you can do depending on the problem.
#' 
#' If your observations aren't independent: this is the wrong kind of model. One common example is time series data, where the data contain repeated measurements on the same subject, but this can also happen when you have un-acknowledged "clustering" in your data where some sets of points are more similar to one another than others (e.g. batch effects, cage effects, genotype effects...). You will need to instead use a method that accounts for the covariances in your data. *Hierarchical models* and *mixed effect models* fall into this category. (This is definitely true for the iris data set - why?)
#' 
#' If variance is not homogeneous: (a) try transforming your data; (b) use another test (Welch's one-way ANOVA with Games-Powell post-hoc; Kruskal-Wallace non-parametric one-way ANOVA with Dunn's post-hoc) that doesn't have this assumption. 
#' 
#' If the relationship between your dependent and independent variables isn't linear: (a) try transforming your data; (b) use a nonlinear regression instead.
#' 
