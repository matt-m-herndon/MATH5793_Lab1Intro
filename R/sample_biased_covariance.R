#' Biased Sample Covariance
#'
#' Calculates the biased sample covariance matrix for a given NxP
#' multivariate dataframe, where N is the number of trials in the
#' experiment and P is the number of variables.
#'
#' @param experiment A two dimensional dataframe of size NxP
#'
#' @return The RxR covariance matrix for the experimental data.
#' @export
#'
#' @examples
#' # generate 10x3 dataframe of random values for testing
#' fake_data = data.frame(replicate(3,sample(100,10,rep=TRUE))) / 100
#'
#' # run fake data through the custom sample covariance function
#' test_result = s_cov(fake_data);
#'
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
    # step along columns of covariance starting from current row.  Since covariance is
    # symmetric, we only need to calculate the upper triangle and mirror those values
    # onto the lower triangle
    for (k in (i:shape[2])){
      # step through each trial
      for (j in (1:shape[1])){
        # inner summation
        result[i,k] = result[i,k] + (experiment[j,i] - sample_means[i])*(experiment[j,k] - sample_means[k]);
      }
      # scale accumation by the number of trials
      result[i,k] = result[i,k] / shape[1];
      # fill in lower triangle at transposed location
      result[k,i] = result[i,k];
    }
  }
  # return biased covariance for the experimental data
  return(result)
}
