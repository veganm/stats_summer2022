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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
# B) Using the flights data frame, identify how many flights took off for each of the three airports for each carrier.
glimpse(flights)

# One way to NOT do this:
flights %>%
  group_by(carrier) %>%
  group_by(origin) %>%
  summarize(flight_count=n())


# B.1) Plot the number of flights for each airport and carrier with month as the x-axis.

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
  geom_line(size=1) +
  facet_grid(rows=vars(origin), cols=vars(carrier), scales="free_y")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

# C) Consider the diamonds data frame included in the ggplot2 package.
diamonds

# C.1) Group these data by cut, then calculate mean, median, and IQR for price within each cut type.
# Store this information in a new tibble, then ungroup the data.

# A start; how do we finish?
diamonds %>%
  group_by(cut) %>%
  summarize(mean_by_cut=mean(price),
            median_by_cut=median(price),
            IQR_by_cut=IQR(price))


# C.2) Plot out price data grouped by (1) cut and (2) clarity. Create these plots again using a different plot type.
# Save all of these plots in a single image file using ggsave().

# One way that technically works but gives bad results:
diamonds %>%
  ggplot(aes(x=price)) +
  geom_histogram(color = "white", fill = "steelblue")+
  facet_grid(rows=vars(cut), cols=vars(clarity))
ggsave("diamond_price_by_cut_clarity,png", width=12, height=8, units="in")

# A slight improvement:
diamonds %>%
  ggplot(aes(x=price)) +
  geom_histogram(color = "white", fill = "steelblue")+
  scale_x_log10()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(x=expression(log[10](price)))+
  facet_grid(rows=vars(cut), cols=vars(clarity))
ggsave("diamond_price_by_cut_clarity,png", width=12, height=8, units="in")

