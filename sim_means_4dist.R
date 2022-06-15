sim_means_4dist<-function(n_reps, sample_sizes, shared_mean){
  # A function to generate simulated distributions of means
  # using the normal, lognormal, exponential, and Poisson distributions 
  # Data generated are average values for simulated "samples"
  # n_reps is the number of "samples" to generate
  # sample_sizes is a vector of sample sizes
  # shared_mean is the population mean for all three distributions
  # Returns a tibble where observed mean values are stored for each "sample"
  
  # Create an empty tibble with three named columns
  distribution_means<-tibble(dist_name=character(),
                             sample_size=numeric(),
                             means=numeric())
  # and populate it
  # Iterate over each of the values in the vector of sample sizes
  for (i in 1:length(sample_sizes)){
    # Within each sample size, execute the indicated number of replicates
    for (j in 1:n_reps){
      # For each replicate, update the distribution_means object with the new data
      distribution_means<-rbind(distribution_means,
                                tibble(
                                  dist_name=c("Norm", "LNorm", "Pois", "Exp"),
                                  sample_size=sample_sizes[i],
                                  means=c(mean(rnorm(sample_sizes[i], mean=shared_mean, sd=shared_mean)),
                                          mean(rlnorm(sample_sizes[i], 
                                                      mean=(log(shared_mean)-(shared_mean^2)/2), 
                                                      sd=log(shared_mean)+shared_mean)),
                                          mean(rpois(sample_sizes[i], lambda=shared_mean)),
                                          mean(rexp(sample_sizes[i], rate=1/shared_mean)))
                                )
      )
    }
  }
  return(distribution_means)
}
