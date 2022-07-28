library(vegan)
library(tidyverse)
library(cowplot)

# The "dune" dataset comes as an example data set in vegan.
# "dune" is a data frame containing vegetation cover class data
# for 30 species on 20 sites, and
# "dune.env" is a data frame of 5 variables across 20 sites.
# For details, see
?dune

# Call the data files in
data(dune)
data(dune.env)
dune
dune.env

# Let's run some tests!
# These are multivariate data, so we SHOULD be doing multivariate testing
# but we won't.
# Instead we will do a lot of pairwise comparisons!

names(dune)
