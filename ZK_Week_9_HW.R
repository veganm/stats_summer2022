pacman::p_load(tidyverse, effectsize, pwr, readxl, ggdark)

#Using an Example from the Week 7 Code 
  #Paired sample test

before <-c(200.1, 190.9, 192.7, 213, 241.4, 196.9, 172.2, 185.5, 205.2, 193.7)
# Weight of the mice after treatment
after <-c(392.9, 393.2, 345.1, 393, 434, 427.9, 422, 383.9, 392.3, 352.2)
# Create a data frame
my_data <- data.frame( 
  group = rep(c("before", "after"), each = 10),
  weight = c(before,  after)
)

# and plot out paired data
pd <- paired(before, after)
plot(pd, type = "profile") + theme_bw()

#### Are the differences normally distributed?
# compute the difference
d <- with(my_data, 
          weight[group == "before"] - weight[group == "after"])
# Shapiro-Wilk normality test for the differences
shapiro.test(d) 

# or as a data frame, depending on how we create the call
res <- t.test(weight ~ group, data = my_data, paired = TRUE)
res

#Power and Effective Size Analysis
effectsize(res)
pwr.t.test(d=6.96, n=10, sig.level = 0.05, alternative = "two.sided")

# # of Data points needed to get a specific power

pwr.t.test(d=6.96, n=2, sig.level = 0.05, alternative = "two.sided")
  #Since the effect size is so big, the smallest n possible has a power of 0.91


#Looking at My Wake Forest Data
  #will be looking at the results of 3 trials instead of the one

"ICC_data_df" <-read_excel("ICC_data_Week8.xlsx")

sixtymin_df <- ICC_data_df %>% 
  filter(treatment == "veh60" | treatment == "mbcd60")

#Summary Stats
summary_df <-sixtymin_df %>% 
  group_by(treatment) %>% 
  summarise(
    count = n(),
    mean = mean(relative_ratio),
    sd = sd(relative_ratio)
  )


#Plot it out
sixtymin_df %>% 
  ggplot(mapping = aes(treatment,relative_ratio))+
  geom_violin(fill = '#FDC314')+
  dark_mode(theme_classic())+
  labs(x = "Time (min)", #Adds the labels
       y = "Surface/Internal mGluR2")


#Is it normal and has equal variance?

with(sixtymin_df,shapiro.test(relative_ratio[treatment == "mbcd60"]))

with(sixtymin_df,shapiro.test(relative_ratio[treatment == "veh60"]))


res.ftest <- var.test(relative_ratio~treatment, data = sixtymin_df)
res.ftest

#Running the statistical tests

wt <- wilcox.test(relative_ratio~treatment, data = sixtymin_df, conf.int = TRUE)


tt<- t.test(relative_ratio~treatment, data = sixtymin_df,
            var.equal = TRUE)


# Power and Effect Size Analysis

effectsize(tt) #Could not figure out how to do this for the wilcox test. I was getting an error

pwr.t.test(d=-0.48, n=120, sig.level = 0.05, alternative="two.sided") #for a power of 0.8

# #data points needed to get a specific power

pwr.t.test(d=-0.48, n=70, sig.level = 0.05, alternative="two.sided") #for a power of 0.8

pwr.t.test(d=-0.48, n=12, sig.level = 0.05, alternative="two.sided") #for a power of 0.2
