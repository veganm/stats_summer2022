pacman::p_load(tidyverse, effectsize, pwr, cowplot, ggpubr)

mean <- 0
sd <- 1

#NORMAL DISTRIBUTION:

# alpha = 0.05
#Two Tail, alpha = 0.05
qnorm(0.975, mean=mean, sd=sd)
#1.96 and -1.96

#One Tail (Upper), alpha = 0.05
qnorm(0.95, mean=mean, sd=sd)
#1.64

#One Tail (lower), alpha = 0.05
qnorm(0.05, mean=mean, sd=sd)
#-1.64

#alpha = 0.01
#Two Tail, alpha = 0.01
qnorm(0.995, mean=mean, sd=sd)
#2.58 and -2.58

#One Tail (Upper), alpha = 0.01
qnorm(0.99, mean=mean, sd=sd)
#2.33

#One Tail (lower), alpha = 0.01
qnorm(0.01, mean=mean, sd=sd)
#-2.33


#STUDENT'S T DISTRIBUTION

# alpha = 0.05
#Two Tail, alpha = 0.05
qt(p=0.975, df =20)
#2.09 and -2.09

#One Tail (Upper), alpha = 0.05
qt(p=0.95, df =20)
#1.72

#One Tail (lower), alpha = 0.05
qt(p=0.05, df =20)
#-1.72

#alpha = 0.01
#Two Tail, alpha = 0.01
qt(p=0.995, df =20)
#2.84 and -2.84

#One Tail (Upper), alpha = 0.01
qt(p=0.99, df =20)
#2.53

#One Tail (lower), alpha = 0.01
qt(p=0.01, df =20)
#-2.53


#Chi Squared Distribution Problem

x <- seq(-5,5, by=0.1) #creating a range of x values from -5 to 5 with spacing every 0.1
y_norm = dnorm(x, mean=mean,sd=sd) #creates y values of a normal distribution
y_chi = dchisq(x,df=1) #creates y values of a chi square distribution with a df of 1

#Plotting
ggplot()+
  geom_line(mapping = aes(x, y_norm))+ #normal distribution
  geom_line(mapping = aes(x, y_chi), color = "red")+ #chi square distribution
  geom_line(mapping = aes(abs(x),(y_norm)^2), color = "green")+ #My attempt to demonstrate that squaring a random variable leads to a chi squared distribution 
  theme_classic()+
  theme(text=element_text(size=16), 
        plot.title=element_text(hjust=0.5, size=16)) + 
  labs(title="Chi Square pdfs")


# I was unable to figure out how to square a normal distribution so 
#that its distribution would match the chi square distribution. Some insight would be appreciated.

#If I were to figure it out, I would then add together multiple
#random variables to demonstrate that it matches the chi square degrees of freedom.
