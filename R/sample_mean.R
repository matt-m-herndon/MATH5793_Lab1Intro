#' Biased Sample Covariance
#'
#' Calculates the biased sample covariance for a given 2 dimensional dataframe
#'
#' @param experiment
#'
#' @return
#' @export
#'
#' @examples
s_mean <- function(experiment){
  # get shape of dataframe
  shape = dim(experiment);

  # make sure the passed dataframe has a compatible shape (must be 2D)
  if (length(shape) != 2){
    stop('dataframe must have only two dimensions')
  }

  # create an array of zeros of length k to store accumulations for each
  # variable along the last dimension of the dataframe (dim 2)
  sample_mean = replicate(shape[2],0);

  # increment through each variable
  for (k in (1:shape[2])){

    # increment through each trial
    for (j in (1:shape[1])){
      # accumulate trials for each variable
      sample_mean[k] = sample_mean[k] + experiment[j,k];
    }

    # scale accumation for each variable by the number of trials
    sample_mean[k] = sample_mean[k] / shape[1];
  }
  # return the sample-wise mean
  return(sample_mean)
}
