# stats_summer2022
R code and data files for Vega lab meetings in summer 2022, re: statistical fundamentals and stupid R tricks for fun and profit.

PathogenCount.csv contains single worm data for colonization of N2 adult worms on plates of S. enterica-GFP (SE) or S. aureus-GFP (SA)

sim_means_4dist.R script contains (awkward, memory-inefficient) code for generating averages from samples drawn from four different distributions (normal, lognormal, Poisson, exponential) all with expectation mean=1

sim_means_4dist_fast.R script contains (slightly less memory-inefficient) code for generating averages from samples drawn from four different distributions (normal, lognormal, Poisson, exponential) all with expectation mean=1

wormboot.R script contains (awkward, memory-inefficient) code for creating simulated batch-based CFU/worm data by resampling a user-input vector of CFU/worm data
