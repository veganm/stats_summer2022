sim_means_4dist_fast<-function(n_reps, sample_sizes, shared_mean){
  # A function to generate simulated distributions of means
  # using the normal, lognormal, exponential, and Poisson distributions 
  # Data generated are average values for simulated "samples"
  # n_reps is the number of "samples" to generate
  # sample_sizes is a vector of sample sizes
  # shared_mean is the population mean for all three distributions
  # Returns a tibble where observed mean values are stored for each "sample"
  
  # Create an empty temporary vector of defined size to hold the data
  temp<-vector("list", length=length(sample_sizes)*n_reps)
    
  # and populate it
  # Establish an index which will be used to store data
  idx<-1
  # Iterate over each of the values in the vector of sample sizes
  for (i in 1:length(sample_sizes)){
    #print(i) # for debugging
    # Within each sample size, execute the indicated number of replicates
    for (j in 1:n_reps){
      #print(j) # for debugging
      # Within each replicate, create a tibble with the mean from each sample
      # and staple it into the temporary vector
      temp[[idx]]<-tibble(dist_name=c("Norm", "LNorm", "Pois", "Exp"),
                          sample_size=sample_sizes[i],
                          means=c(mean(rnorm(sample_sizes[i], mean=shared_mean, sd=shared_mean)),
                                  mean(rlnorm(sample_sizes[i], 
                                              mean=(log(shared_mean)-(shared_mean^2)/2), 
                                              sd=log(shared_mean)+shared_mean)),
                                  mean(rpois(sample_sizes[i], lambda=shared_mean)),
                                  mean(rexp(sample_sizes[i], rate=1/shared_mean)))
                          )
      idx<-idx+1
    }
  }
  # Rearrange the finished data from temp into a proper tibble
  # with columns dist_name, sample_size, and means
  distribution_means<-dplyr::bind_rows(temp)
  return(distribution_means)
}
