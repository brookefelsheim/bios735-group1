test_that("errors for bad input to train_random_forest", {
  expect_error(train_random_forest(iris))
  expect_error(train_random_forest(matrix(1:16, nrow = 4)))
  expect_error(train_random_forest("test"))
  expect_error(train_random_forest())
})

test_that("errors for bad input to plot_rf_importance", {
  expect_error(plot_rf_importance(iris))
  expect_error(plot_rf_importance(matrix(1:16, nrow = 4)))
  expect_error(plot_rf_importance("test"))
  expect_error(plot_rf_importance())
})
