# Vega Lab Meeting Week 8
# Today we will analyze data badly
# using code and other materials from
#   https://rpubs.com/Roeland-KINDT/694016
# 
# Background information is from
#   https://www.khanacademy.org/math/statistics-probability/significance-tests-one-sample/error-probabilities-and-power/v/introduction-to-type-i-and-type-ii-errors

# First, as usual, we will load the libraries we need
packages<-c("tidyverse", "ggsci", "ggrepel", "ggforce", "BiodiversityR", 
            "tcltk")
install.packages(setdiff(packages, rownames(installed.packages())))

library(BiodiversityR) # also loads vegan
library(tidyverse)
library(ggsci)
library(ggrepel)
library(ggforce)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#              BAD HYPOTHESIS TESTING
#
# The "dune" dataset comes as an example data set in vegan.
# "dune" is a data frame containing vegetation cover class data
# for 30 species on 20 sites, and
# "dune.env" is a data frame of 5 variables across 20 sites.
# For details, see
?dune

# Also there is an expanded version (dune2), described at:
# https://www.davidzeleny.net/anadat-r/doku.php/en:data:dune

# Call the data files in
data(dune)
data(dune.env)
dune
summary(dune.env)
dune.env

# For some plotting methods, it is necessary that the environmental data set is attached:
?attach
attach(dune.env)

# Let's run some tests!
# These are multivariate data, so we SHOULD be doing multivariate analyses - 
# but we won't.
# Also, these data are heavily zero-enriched, which is a different problem - 
# but we won't talk about that yet.
# Instead we will do a lot of pairwise comparisons!

names(dune)

# That's a lot of plant species to work with. 
# Let's start with the first one (name Achimill,index 1).
# Recall that we can access these data by index
dune[[1]]

# or by name
dune$Achimill

# Let's see whether Achimill coverage is significantly different across management types
# $Management is a factor with levels: 
# BF (Biological farming), HF (Hobby farming), NM (Nature Conservation Management), and SF (Standard Farming).
unique(dune.env$Management)

# Since we have attached dune.env to the environment, we can just ask:
unique(Management)

# Let's see how many data points we have for each
length(which(Management=="SF"))
length(which(Management=="BF"))
length(which(Management=="HF"))
length(which(Management=="NM"))

# small, nice. Let's look at the data:
dune$Achimill[Management=="BF"]
dune$Achimill[Management=="HF"]
dune$Achimill[Management=="SF"]
dune$Achimill[Management=="NM"]

# and let's t-test. ALL the t-tests.
t.test(dune$Achimill[Management=="BF"],
       dune$Achimill[Management=="HF"])
t.test(dune$Achimill[Management=="BF"],
       dune$Achimill[Management=="SF"])
t.test(dune$Achimill[Management=="BF"],
       dune$Achimill[Management=="NM"])
t.test(dune$Achimill[Management=="HF"],
       dune$Achimill[Management=="SF"])
t.test(dune$Achimill[Management=="HF"],
       dune$Achimill[Management=="NM"])
t.test(dune$Achimill[Management=="SF"],
       dune$Achimill[Management=="NM"])

# Anything? Let's move on.
mydata<-dune$Agrostol
t.test(mydata[Management=="BF"],
       mydata[Management=="HF"])
t.test(mydata[Management=="BF"],
       mydata[Management=="SF"])
t.test(mydata[Management=="BF"],
       mydata[Management=="NM"])
t.test(mydata[Management=="HF"],
       mydata[Management=="SF"])
t.test(mydata[Management=="HF"],
       mydata[Management=="NM"])
t.test(mydata[Management=="SF"],
       mydata[Management=="NM"])

# Better? let's try a few more just to be sure.
mydata<-dune$Airaprae # Where do the NAs come from?
mydata<-dune$Alopgeni
mydata<-dune$Anthodor

# How many positives have we racked up, at p<0.05?
# How many things did we do wromg?

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#             DO BETTER
# First, we should explore the data set. 
# It's a lot of factors, so it isn't obvious where to start
# if we wanted to, say, plot out a bunch of histograms or violin plots like before.
# We still can if we want to:

dune %>%
  pivot_longer(everything(), names_to="species", values_to="cover") %>%
  ggplot(aes(x=factor(species), y=cover, color=factor(species))) +
  geom_jitter(width=0.1)+
  theme_classic()+
  theme(text=element_text(size=16), 
        axis.text.x = element_text(angle=90, size=14),
        plot.title=element_text(hjust=0.5, size=16),
        legend.position = "none") + 
  labs(title="Dune", x="Species")

# but it isn't obvious what to do with the info in dune.env.

# For this kind of data, we generally start with exploratory analyses like ordination.
# Here, an ordination diagram is generated via the ordiplot function. 
# For this example, the ordination method is non metric multidimensional scaling, 
# available via function metaMDS.
?metaMDS

# What does "distance='bray'" mean?

Ordination.model1 <- metaMDS(dune, distance='bray', k=2, trymax=20, 
                             autotransform=TRUE, noshare=0.1, expand=TRUE, 
                             trace=1, plot=FALSE)

# The best solution can be plotted by ordiplot
# (if we only wanted to use ggplot, we should specify tidy=TRUE to return a data.frame)
plot1 <- ordiplot(Ordination.model1, choices=c(1,2))

# or, on the MDS object, we can we extract information on the locations of sites (circles in the ordiplot) via function sites.long. 
# Information on characteristics of the sites is added via the argument of env.data
?sites.long
sites.long1 <- sites.long(plot1, env.data=dune.env)
head(sites.long1)

# and the same for species using species.long
species.long1 <- species.long(plot1)

# Information on the labeling of the axes is obtained with function axis.long. 
# This information is extracted from the ordination model and not the ordiplot, 
# hence it is important to select the same axes via argument ‘choices’. 
axis.long1 <- axis.long(Ordination.model1, choices=c(1, 2))
axis.long1

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#       ADDING A THEME
# BioR.theme is supposed to be provided in BiodiversityR
# bit it wasn't working on my install
# so I am adding it manually here from the vignette
# You can do this too if there is a theme you want to re-use!

BioR.theme <- theme(
  panel.background = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.line = element_line("gray25"),
  text = element_text(size = 12),
  axis.text = element_text(size = 10, colour = "gray25"),
  axis.title = element_text(size = 14, colour = "gray25"),
  legend.title = element_text(size = 14),
  legend.text = element_text(size = 14),
  legend.key = element_blank())

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Now we can generate the plot, 
# where we differentiate among different categories of Management…
plotgg1 <- ggplot() + 
  geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
  geom_hline(yintercept = c(0), color = "grey70", linetype = 2) +  
  scale_x_continuous(sec.axis = dup_axis(labels=NULL, name=NULL)) +
  scale_y_continuous(sec.axis = dup_axis(labels=NULL, name=NULL)) +    
  geom_point(data=sites.long1, 
             aes(x=axis1, y=axis2, colour=Management, shape=Management), 
             size=5) +
  BioR.theme +
  ggsci::scale_colour_npg() +
  coord_fixed(ratio=1)+
  labs(x="NMDS1", y="NMDS2")

plotgg1

# The functions that extract the ‘long’ data from the ordiplot can also be used directly 
# when using a ggplot object, combining steps 2 and 3 as below:
plotgg1 <- ggplot() + 
  geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
  geom_hline(yintercept = c(0), color = "grey70", linetype = 2) +  
  xlab("NMDS1") +
  ylab("NMDS2") +  
  scale_x_continuous(sec.axis = dup_axis(labels=NULL, name=NULL)) +
  scale_y_continuous(sec.axis = dup_axis(labels=NULL, name=NULL)) +    
  geom_point(data=sites.long(plot1, env.data=dune.env), 
             aes(x=axis1, y=axis2, colour=Management, shape=Management), 
             size=5) +
  BioR.theme +
  ggsci::scale_colour_npg() +
  coord_fixed(ratio=1)

plotgg1

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#         Adding structure with ordispider
# 
# Function centroids.long obtains data on the centroids of each grouping. 
# It is possible to directly add their results as ‘ordispider diagrams’ in ggplot. 


# Here also another layer is added of superellipses obtained from the ggforce::geom_mark_ellipse function.

plotgg3 <- ggplot() + 
  geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
  geom_hline(yintercept = c(0), color = "grey70", linetype = 2) +  
  xlab(axis.long1[1, "label"]) +
  ylab(axis.long1[2, "label"]) +  
  scale_x_continuous(sec.axis = dup_axis(labels=NULL, name=NULL)) +
  scale_y_continuous(sec.axis = dup_axis(labels=NULL, name=NULL)) +
  geom_mark_ellipse(data=sites.long1, 
                    aes(x=axis1, y=axis2, colour=Management, 
                        fill=after_scale(alpha(colour, 0.2))), 
                    expand=0, size=0.2, show.legend=FALSE) +
  geom_segment(data=centroids.long(sites.long1, grouping=Management), 
               aes(x=axis1c, y=axis2c, xend=axis1, yend=axis2, colour=Management), 
               size=1, show.legend=FALSE) +
  geom_point(data=sites.long1, 
             aes(x=axis1, y=axis2, colour=Management, shape=Management), 
             size=5) +
  BioR.theme +
  ggsci::scale_colour_npg() +
  coord_fixed(ratio=1)
plotgg3

# I hate starplot lines, so let's take those out
# and un-fill the ellipses so we aren't drawing the eye QUITE so much
plotgg3 <- ggplot() + 
  geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
  geom_hline(yintercept = c(0), color = "grey70", linetype = 2) +  
  xlab(axis.long1[1, "label"]) +
  ylab(axis.long1[2, "label"]) +  
  scale_x_continuous(sec.axis = dup_axis(labels=NULL, name=NULL)) +
  scale_y_continuous(sec.axis = dup_axis(labels=NULL, name=NULL)) +
  geom_mark_ellipse(data=sites.long1, 
                    aes(x=axis1, y=axis2, colour=Management), 
                    expand=0, size=0.2, show.legend=FALSE) +
  geom_point(data=sites.long1, 
             aes(x=axis1, y=axis2, colour=Management, shape=Management), 
             size=5) +
  BioR.theme +
  ggsci::scale_colour_npg() +
  coord_fixed(ratio=1)
plotgg3

