context("testing sample correlation")

test_that("sample correlation does not match with ground truth", {
  # generate 10x3 dataframe of random values for testing
  fake_data = data.frame(replicate(3,sample(100,10,rep=TRUE))) / 100

  # run fake data through the custom sample correlation function
  test_result = s_corr(fake_data);

  # 1. Calculate ground truth with cor.
  # 2. Remove names so that types match
  ground_truth = unname(cor(fake_data));

  # compare ground truth mean with custom sample mean
  expect_equal(test_result,ground_truth)
})
