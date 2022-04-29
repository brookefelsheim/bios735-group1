test_that("errors for bad input to glmm_model_fit", {
  expect_error(glmm_model_fit(list(c(1, "test")), london, "yes", seoul))
  expect_error(glmm_model_fit(glmm_fit, iris, "yes", seoul))
  expect_error(glmm_model_fit(matrix(1:16, nrow = 4), london, "yes", seoul))
  expect_error(glmm_model_fit(list(c(1, "test")), london, "yes", seoul))
  expect_error(glmm_model_fit(london, "yes", seoul))
  expect_error(glmm_model_fit())
})
