# week 3 homeworks
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
#~~~~~~~~~          Homework for summary, grouping and mutations
#
# A) Using the weather data frame, write code to get the mean and standard deviation temperature for each day in 2013 for NYC.
glimpse(weather)

# One way to NOT do this:
weather %>%
  group_by(day) %>%
  summarize(day_mean=mean(temp),
            day_sd=sd(temp))

# This generates a 31 x 3 tibble.
# How many days are in a year?
# We have to group by month first!
weather %>%
  group_by(month, day) %>%
  summarize(day_mean=mean(temp),
            day_sd=sd(temp))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
# B) Using the flights data frame, identify how many flights took off for each of the three airports for each carrier.
glimpse(flights)

# One way to NOT do this:
flights %>%
  group_by(carrier) %>%
  group_by(origin) %>%
  summarize(flight_count=n())


# B.1) Plot the number of flights for each airport and carrier with month as the x-axis.

# From Zee:
flights %>% 
  group_by(origin,carrier,month) %>% 
  mutate(n_flights = n()) %>% 
  ggplot(mapping=aes(factor(month),n_flights,fill=carrier))+
  facet_wrap(~origin,
             scales = "fixed",
             nrow=3)+
  geom_bar(stat="identity", position="dodge")+
  labs(title = "Number of Flights from each Airport and Carrier in Each Month",
       x = "Months",
       y = "Number of Flights")+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size=20))+
  theme(legend.title = element_blank())+
  theme_bw()

# One way you probably shouldn't do this:
# first we have to get this information
flights %>%
  group_by(month, origin, carrier) %>%
  summarise(flight_count=n()) %>%
  # then we can plot it out
  ggplot(aes(x=month, y=flight_count, color=factor(carrier))) +
  geom_line(size=1)

# A better way
flight_counts_monthly<-flights %>%
  group_by(month, origin, carrier) %>%
  summarise(flight_count=n())
flight_counts_monthly %>%
  ggplot(aes(x=month, y=flight_count)) +
  scale_y_log10()+
  geom_line(size=1) +
  facet_grid(rows=vars(origin), cols=vars(carrier), scales="fixed")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

# C) Consider the diamonds data frame included in the ggplot2 package.
diamonds

# C.1) Group these data by cut, then calculate mean, median, and IQR for price within each cut type.
# Store this information in a new tibble, then ungroup the data.

# Pretty basic at this point
diamonds_by_cut<-diamonds %>%
  group_by(cut) %>%
  summarize(mean_by_cut=mean(price),
            median_by_cut=median(price),
            IQR_by_cut=IQR(price)) %>%
  ungroup()


# C.2) Plot out price data grouped by (1) cut and (2) clarity. Create these plots again using a different plot type.
# Save all of these plots in a single image file using ggsave().

# From Zee:
diamonds %>% 
  group_by(cut,clarity) %>% 
  summarize(mean = mean(price)) %>% 
  ggplot(mapping=aes(cut,mean,color=clarity))+ #Needs to be fill
  geom_bar(stat="identity",
           position = "dodge")+
  scale_fill_viridis_d(option = "inferno")+
  labs(title = "Cut vs Clarity",
       x = "Diamond Cut",
       y = "Price ($)")+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size=20))+
  theme(legend.title = element_blank())+
  theme_dark()

# Also from Zee:
diamonds %>% 
  ggplot(mapping=aes(cut,price,fill=clarity))+
  geom_violin()+
  scale_y_log10()+
  facet_wrap(~clarity,
             scales = "fixed")+
  scale_fill_viridis_d(option = "plasma")+
  labs(x = "Diamond Cut",
       y = "Price ($)")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size=8))+
  theme(legend.position="none")

?scale_fill_viridis_d

# One way that technically works but gives bad results:
diamonds %>%
  ggplot(aes(x=price)) +
  geom_histogram(color = "white", fill = "steelblue")+
  facet_grid(rows=vars(cut), cols=vars(clarity))
ggsave("diamond_price_by_cut_clarity,png", width=12, height=8, units="in")

# A slight improvement:
diamonds %>%
  ggplot(aes(x=price)) +
  geom_histogram(color = "white", fill = "black")+
  theme_classic()+
  scale_x_log10()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(x=expression(log[10](price)))+
  facet_grid(rows=vars(cut), cols=vars(clarity))
ggsave("diamond_price_by_cut_clarity,png", width=12, height=8, units="in")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#               HOMEWORK FOR LOOPS
# D) Pick one of the loops in section 21.2.1 of R for Data Science.
# For next week, write a for loop to perform that task.

# Let's pick the second one:
# Determine the type of each column in nycflights13::flights.
glimpse(flights)

# We need to know how many columns there are (19) to know how long to iterate
dim(flights)
dim(flights)[2]

# And we need to be able to take data types
?str
str(flights)
?class
class(flights[[1]])

# Now we have to establish the three parts of our for loop
output<-vector("character", length=dim(flights)[2]) # Someplace to keep the output
for (i in seq_along(dim(flights)[2])){        # A loop that iterates over a range
  output[i]<-class(flights[i])  # and does something each time
}

# What went wrong?
class(flights[1])
class(flights[[1]])
dim(flights)[2]

# Fix
output<-vector("character", length=dim(flights)[2]) # Someplace to keep the output
length(dim(flights)[2])
for (i in seq_along(flights)){        # A loop that iterates over a range
  #print(i) # For troubleshooting
  #print(class(flights[[i]])) # For troubleshooting
  output[i]<-class(flights[[i]])[1]  # and does something each time
}
output

# want to do this without a for() loop?
?sapply
sapply(flights, "class")
