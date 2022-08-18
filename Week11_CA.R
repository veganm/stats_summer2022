#During lab meeting, we said that we could find the critical values associated with alpha=0.05, but we left this as an exercise. Assuming that your test statistic under the null hypothesis is distributed as a standard normal N(0,1), find the critical values associated with one-tailed (upper and lower) and two-tailed testing using alpha = 0.05 and alpha = 0.1. Do the same for a Student's T distribution with some reasonable number of degrees of freedom (your choice).

##Normal Distribution
#N(0,1) with a= 0.05 lower tail
qnorm(0.05, mean=0, sd=1, lower.tail = TRUE)

#N(0,1) with a= 0.05 upper tail
qnorm(0.05, mean=0, sd=1, lower.tail = FALSE)

#N(0,1) with a= 0.05 two tail
qnorm((0.05/2), mean=0, sd=1, lower.tail = FALSE)

#N(0,1) with a= 0.1 lower tail
qnorm(0.1, mean=0, sd=1, lower.tail = TRUE)

#N(0,1) with a= 0.1 upper tail
qnorm(0.1, mean=0, sd=1, lower.tail = FALSE)

#N(0,1) with a= 0.1 two tail
qnorm((0.1/2), mean=0, sd=1, lower.tail = FALSE)

##Student's T Distribution
#Central T with a= 0.05 lower tail, df=10
qt(0.05, 10, lower.tail = TRUE)

#Central T with a= 0.05 upper tail, df=10
qt(0.05, 10, lower.tail = FALSE)

#Central T with a= 0.05 two tail, df=10
qt((0.05/2), 10, lower.tail = FALSE)

#Central T with a= 0.1 lower tail, df=10
qt(0.1, 10, lower.tail = TRUE)

#Central T with a= 0.1 upper tail, df=10
qt(0.1, 10, lower.tail = FALSE)

#Central T with a= 0.1 two tail, df=10
qt((0.1/2), 10, lower.tail = FALSE)