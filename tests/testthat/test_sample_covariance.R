context("testing sample covariance")

test_that("sample covariance does not match with ground truth", {
  test_vars = 3
  test_trials = 10

  # generate 10x3 dataframe of random values for testing
  fake_data = data.frame(replicate(test_vars,sample(100,test_trials,rep=TRUE))) / 100

  # run fake data through the custom sample covariance function
  test_result = s_cov(fake_data);

  # 1. Calculate unbiased covariance matrix using cov
  # 2. Convert biased estimate to unbiased estimate using scaling factor
  # 3. Remove names so that types match
  ground_truth = unname(cov(fake_data) * (test_trials-1)/test_trials);

  # compare ground truth cov with s_cov
  expect_equal(test_result, ground_truth)
})
