test_that("errors for bad input to MCEM_algorithm", {
  expect_error(MCEM_algorithm(iris))
  expect_error(MCEM_algorithm(matrix(1:16, nrow = 4)))
  expect_error(MCEM_algorithm("test"))
  expect_error(MCEM_algorithm())
  expect_error(MCEM_algorithm(london, beta_initial = c(1,3,4)))
  expect_error(MCEM_algorithm(london, beta_initial = 3))
  expect_error(MCEM_algorithm(london, theta_initial = "test"))
})
