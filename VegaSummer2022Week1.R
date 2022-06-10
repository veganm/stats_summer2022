# Week 1, Vega lab meeting 2022
# Today we will be exploring quantitative data
# using code and other material from
#   https://moderndive.com/1-getting-started.html and https://moderndive.com/2-viz.html
#
# Background material for this section is from
#   https://www.khanacademy.org/math/statistics-probability/analyzing-categorical-data
#   http://www.biostathandbook.com/variabletypes.html
#   http://www.biostathandbook.com/analysissteps.html
#   https://amplitude.com/blog/data-types
#   https://hackr.io/blog/float-vs-double
#
# If you don't know anything about data frames and tibbles in R, read these:
#   https://www.tutorialspoint.com/r/r_data_frames.htm
#   https://r4ds.had.co.nz/tibbles.html
#   https://tibble.tidyverse.org/
#
# We will:
# - discuss data types in general and as represented on computers
# - learn to import publicly available data in R
# - learn to manually input data to create an object in R
# - explore options for plotting quantitative data

# First, let's import data in the "nycflights13" library
# These are data relating to domestic flights from NYC.
# Make sure all the necessary packages are installed and loaded

packages<-c("tidyverse","nycflights13", "knitr", "cowplot")
install.packages(setdiff(packages, rownames(installed.packages())))

library(tidyverse)
library(nycflights13)
library(knitr)
library(cowplot)

# What's in the data set?
# One of the included data frames is named "flights"; let's look.
flights

View(flights)
flights %>%
  View()

print(flights, n = 10, width = Inf)
flights %>%
  print(n=10, width=Inf)

glimpse(flights)

?flights

#another data frame is called "airlines"
# knitr's "kable" function makes pretty tables
airlines
kable(airlines)

# some variables are "identification variables"
# others are "measurement variables" or "quantitative variables"
# see also http://www.biostathandbook.com/variabletypes.html
glimpse(airports)

# We can subset tibbles and data frames by column name
names(airlines)
airlines[["name"]]
airlines[[2]]
airlines[,2]
airlines[,"name"]
airlines %>% .$name
airlines %>% .[["name"]]

# and by identification variable
# look at your environment!
# what happened? why?
# we'll use this object in a minute
alaska_flights <- flights %>% 
  filter(carrier == "AS")
View(alaska_flights)

# printing all to screen is fine for smaller data sets, 
# but we need some other functions to summarize data from larger frames
flights$year
unique(flights$year)
unique(flights$origin)
unique(flights$dest)
unique(flights$carrier)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#########     Creating summary plots
######        RULE 0: SHOW THE DATA
#
#~~~~~~   Plot type 1: The scatterplot

alaska_flights %>%
  ggplot(aes(x = dep_delay, y = arr_delay)) +
  geom_point()

# What happens if no object type is specified?
alaska_flights %>%
  ggplot(aes(x = dep_delay, y = arr_delay))

# let's add some plotting options
pAlaskaFlights<-alaska_flights %>%
  ggplot(aes(x = dep_delay, y = arr_delay)) +
  geom_point(size=2, alpha=0.2) +
  theme_classic() +
  theme(axis.text=element_text(size=14), 
        axis.title=element_text(size=14, face="bold"), 
        plot.title=element_text(hjust=0.5, size=14),
        legend.text=element_text(size=14),
        legend.title=element_blank(),
        legend.position = "none") +
  labs(title="Alaska flights, 2013", y="Departure delay (min)", x="Arrival delay (min)")
pAlaskaFlights

# and some more plots, using another carrier
kable(airlines)
jetblue_flights <- flights %>% 
  filter(carrier == "B6")
pJetBlueFlights<-jetblue_flights %>%
  ggplot(aes(x = dep_delay, y = arr_delay)) +
  geom_point(size=1, alpha=0.1) +
  theme_classic() +
  theme(axis.text=element_text(size=14), 
        axis.title=element_text(size=14, face="bold"), 
        plot.title=element_text(hjust=0.5, size=14),
        legend.text=element_text(size=14),
        legend.title=element_blank(),
        legend.position = "none") +
  labs(title="JetBlue flights, 2013", y="Departure delay (min)", x="Arrival delay (min)")
pJetBlueFlights
plot_grid(pAlaskaFlights, pJetBlueFlights)
plot_grid(pAlaskaFlights, pJetBlueFlights, rel_widths = c(1,1.5))
plot_grid(pAlaskaFlights, pJetBlueFlights, ncol=1)

# we can jitter points instead of adding transparency
pAA10<-alaska_flights %>%
  ggplot(aes(x = dep_delay, y = arr_delay)) +
  geom_jitter(height=10, width=10) +
  theme_classic() +
  labs(title="Alaska flights, 2013, jitter10", y="Departure delay (min)", x="Arrival delay (min)")
pAA30<-alaska_flights %>%
  ggplot(aes(x = dep_delay, y = arr_delay)) +
  geom_jitter(height=30, width=30) +
  theme_classic() +
  labs(title="Alaska flights, 2013, jitter30", y="Departure delay (min)", x="Arrival delay (min)")
plot_grid(pAA10, pAA30, labels="AUTO")
plot_grid(pAA10, pAA30, labels="auto")
ggsave("pAA_jitter_example.png")
ggsave("pAA_jitter_example.png", width=6, height=4, units="in")

# Why is ggplot removing so many data points?
min(jetblue_flights$dep_delay)
min(jetblue_flights$dep_delay, na.rm=TRUE)
max(jetblue_flights$dep_delay, na.rm=TRUE)
sum(is.na(jetblue_flights))
sum(is.na(jetblue_flights$dep_delay))
sum(is.na(jetblue_flights$arr_delay))

# let's find what entries (rows) have NA values
idx<-which(is.na(jetblue_flights$arr_delay))
jetblue_flights %>% slice(idx)

# OK, let's use this to create new objects without NA values in delay
jetblue_flights <- flights %>% 
  filter(carrier == "B6") %>%
  slice(-idx)

# same for Alaska Airlines data
sum(is.na(alaska_flights$dep_delay))
sum(is.na(alaska_flights$arr_delay))
idx<-which(is.na(alaska_flights$arr_delay))
alaska_flights <- flights %>% 
  filter(carrier == "AS") %>%
  slice(-idx)
# Go back to lines 105 and 122. Remake these plots. Are the warnings gone?

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~  Plot type 2: Line graphs
# using the "weather" data set
# see also
# http://www.sthda.com/english/wiki/ggplot2-line-plot-quick-start-guide-r-software-and-data-visualization
# http://www.sthda.com/english/wiki/ggplot2-point-shapes

View(weather)
glimpse(weather)

# Filter data from the first two weeks of January
early_january_weather <- weather %>% 
  filter(origin == "EWR" & month == 1 & day <= 15)

early_january_weather %>%
  ggplot(aes(x = time_hour, y = temp))+
  geom_line()

# as before we can add some flavor
early_january_weather %>%
  ggplot(aes(x = time_hour, y = temp))+
  geom_line(color="red", linetype = "dashed", size=1) +
  geom_point() +
#  geom_point(shape=15) +
#  theme_minimal()+
# theme_dark() +
# theme_light() +
# theme_void() + 
  theme(axis.text=element_text(size=14), 
        axis.title=element_text(size=14, face="bold"), 
        plot.title=element_text(hjust=0.5, size=14),
        legend.text=element_text(size=14)) +
  labs(title="Temperature at Newark Airport", y="Temperature", x="Time")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~ Plot type 3: Histograms

weather %>%
  ggplot(aes(x = temp)) +
  geom_histogram()

weather %>%
  ggplot(aes(x = temp)) +
  geom_histogram(color = "white", fill = "steelblue")

pTemp_40bins<-weather %>%
  ggplot(aes(x = temp)) +
  geom_histogram(bins=40, color = "white") +
  theme_classic()+
  labs(title="With 40 bins", y="Temperature", x="Count")
pTemp_10bw<-weather %>%
  ggplot(aes(x = temp)) +
  geom_histogram(binwidth=10, color = "white") +
  theme_classic()+
  labs(title="With bid width 10", y="Temperature", x="Count")
plot_grid(pTemp_40bins, pTemp_10bw, ncol=1)

# Plot across values of an identification variable with facet_wrap()
weather %>%
  ggplot(aes(x = temp)) +
  geom_histogram(binwidth=5, color = "white") +
  facet_wrap(~month)

weather %>%
  ggplot(aes(x = temp)) +
  geom_histogram(binwidth=5, color = "white") +
  facet_wrap(~month, nrow=4)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~ Plot type 4: Boxplots and their relatives

# Filter November data
# and take some summary statistics
november_weather <- weather %>% 
  filter(month == 11) 
idx<-which(is.na(november_weather$temp))

mean(november_weather$temp)
min(november_weather$temp)
max(november_weather$temp)
quantile(november_weather$temp, 0.25)
quantile(november_weather$temp, 0.5)
quantile(november_weather$temp, 0.75)

# Plot out November data only
p11_data<-november_weather %>%
  ggplot(aes(x=factor(month), y=temp))+
  geom_jitter(width=0.1, alpha=0.2)+
  geom_hline(yintercept=max(november_weather$temp), linetype="dashed")+
  geom_hline(yintercept=quantile(november_weather$temp, 0.75), linetype="dashed")+
  geom_hline(yintercept=median(november_weather$temp), linetype="dashed")+
  geom_hline(yintercept=quantile(november_weather$temp, 0.25), linetype="dashed")+
  geom_hline(yintercept=min(november_weather$temp), linetype="dashed")+
  labs(title="Data only", y="Temperature", x="Month")
p11_data_box<-november_weather %>%
  ggplot(aes(x=factor(month), y=temp))+
  geom_jitter(width=0.1, alpha=0.2)+
  geom_boxplot()+
  geom_hline(yintercept=max(november_weather$temp), linetype="dashed")+
  geom_hline(yintercept=quantile(november_weather$temp, 0.75), linetype="dashed")+
  geom_hline(yintercept=median(november_weather$temp), linetype="dashed")+
  geom_hline(yintercept=quantile(november_weather$temp, 0.25), linetype="dashed")+
  geom_hline(yintercept=min(november_weather$temp), linetype="dashed")+
  theme(plot.title=element_text(hjust=0.5, size=14))+
  labs(title="Data + boxplot", y="Temperature", x="Month")
p11_box<-november_weather %>%
  ggplot(aes(x=factor(month), y=temp))+
  geom_boxplot()+
  theme(plot.title=element_text(hjust=0.5, size=14))+
  labs(title="Boxplot only", y="Temperature", x="Month")
p11_data_violin<-november_weather %>%
  ggplot(aes(x=factor(month), y=temp))+
  geom_jitter(width=0.1, alpha=0.2)+
  geom_violin(fill=NA)+
  theme(plot.title=element_text(hjust=0.5, size=14))+
  labs(title="Data + violin plot", y="Temperature", x="Month")
plot_grid(p11_data, p11_data_violin, p11_data_box, p11_box, ncol=2)

# Plot out all months
# What's wrong with this picture?
weather %>%
  ggplot(aes(x = month, y = temp)) +
  geom_boxplot()

# let's try that again, with month as a FACTOR
weather %>%
  ggplot(aes(x = factor(month), y = temp)) +
  geom_boxplot()
weather %>%
  ggplot(aes(x = factor(month), y = temp)) +
  geom_violin()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Plot type 5: Barplots and frequency data

fruits <- tibble(
  fruit = c("apple", "apple", "orange", "apple", "orange")
)
fruits_counted <- tibble(
  fruit = c("apple", "orange"),
  number = c(3, 2)
)
fruits
fruits_counted

# a simple barplot on uncounted data
fruits %>%
  ggplot(aes(x = fruit))+
  geom_bar()

# to do the same with counted data, use column plotting with geom_col()
fruits_counted %>%
  ggplot(aes(x = fruit, y=number))+
  geom_col()

# back to the bigger data set of flight data
# note that these are not pre-counted
flights %>%
  ggplot(aes(x = carrier)) +
  geom_bar()

# Stacked bars: carrier and origin
# With fill by origin:
flights %>%
  ggplot(aes(x = carrier, fill=origin)) +
  geom_bar()
  
# with outline by origin:
flights %>%
  ggplot(aes(x = carrier, color=origin)) +
  geom_bar()  

# Side by side barplot, with fill by origin:
flights %>%
  ggplot(aes(x = carrier, fill=origin)) +
  geom_bar(position="dodge")

# and a more robust interpretation, with position_dodge()
# https://ggplot2.tidyverse.org/reference/position_dodge.html
# note that "preserve" asks,
# Should dodging preserve the total width of all elements at a position, or the width of a single element?
# "padding" is the amount of space between bars within an x-axis element (default is 0.1)
flights %>%
  ggplot(aes(x = carrier, fill=origin)) +
  geom_bar(position=position_dodge2(preserve = "single", padding=0.2))

# Once again, we can facet this out instead
# and create a different plot for each origin
flights %>%
  ggplot(aes(x = carrier, fill=origin)) +
  geom_bar()+
  facet_wrap(~ origin, ncol = 1)
