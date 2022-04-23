test_that("errors for bad input", {
  expect_error(trainRandomForest(iris))
  expect_error(trainRandomForest(matrix(1:16, nrow = 4)))
})
