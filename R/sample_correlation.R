
s_corr <- function(experiment){
  # get shape of dataframe
  shape = dim(experiment);

  # make sure the passed dataframe has a compatible shape (must be 2D)
  if (length(shape) != 2){
    stop('dataframe must have only two dimensions')
  }

  # calculate sample means for use during calculation
  sample_means = s_mean(experiment)

  # create PxP matrix to store correlation coefficient
  result = replicate(shape[2],replicate(shape[2],0));

  # step along rows of covariance
  for (i in (1:shape[2])){
    # step along columns of covariance
    for (k in (1:shape[2])){
      # numerator term
      num = 0;
      # denuminator terms
      den1 = 0;
      den2 = 0;
      # step through each trial
      for (j in (1:shape[1])){
        # numerator accumation
        num = num + (experiment[j,i] - sample_means[i])*(experiment[j,k] - sample_means[k]);

        # denominator accumulations
        den1 = den1 + (experiment[j,i] - sample_means[i])^2;
        den2 = den2 + (experiment[j,k] - sample_means[k])^2;
      }

      # Combine partial solutions into final result
      result[i,k] = num / (sqrt(den1)*sqrt(den2));
    }
  }
  # return biased covariance for the experimental data
  return(result)
}
