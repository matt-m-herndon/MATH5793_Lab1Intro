#' Sample Mean
#'
#' Calculates the sample mean for a given NxP multivariate dataframe,
#' where N is the number of trials in the experiment and P is the number
#' of variables.
#'
#' @param experiment A two dimensional dataframe of size NxP
#'
#' @return The 1xR vector of sample means for the experimental data.
#' @export
#'
#' @examples
#' # generate 10x3 dataframe of random values for testing
#' fake_data = data.frame(replicate(3,sample(100,10,rep=TRUE))) / 100
#'
#' # run fake data through the custom sample mean function
#' test_result = s_mean(fake_data);
#'
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
