
s_cov <- function(experiment){
  # get shape of dataframe
  shape = dim(experiment);

  # make sure the passed dataframe has a compatible shape (must be 2D)
  if (length(shape) != 2){
    stop('dataframe must have only two dimensions')
  }

  # calculate sample means for the experiment
  sample_means = s_mean(experiment)

  # create PxP matrix to store covariance
  result = replicate(shape[2],replicate(shape[2],0));

  # step along rows of covariance
  for (i in (1:shape[2])){
    # step along columns of covariance
    for (k in (1:shape[2])){
      # step through each trial
      for (j in (1:shape[1])){
        # inner summation
        result[i,k] = result[i,k] + (experiment[j,i] - sample_means[i])*(experiment[j,k] - sample_means[k]);
      }

      # scale accumation by the number of trials
      result[i,k] = result[i,k] / shape[1];
    }
  }
  # return biased covariance for the experimental data
  return(result)
}