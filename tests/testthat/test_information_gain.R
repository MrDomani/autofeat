context("information_gain working properly")

data("iris")
splitted_Species <- split(iris$Species, kmeans(iris[,-5], 3)[['cluster']])
test_that("calculate_entropy working properly",{
  expect_equal(calculate_entropy(factor(LETTERS[c(1,1,1)])), 0)
  expect_length(calculate_entropy(iris$Species), 1)
  expect_true(is.numeric(calculate_entropy(iris$Species)))
})

test_that("information_gain working properly", {
  expect_length(information_gain(splitted_Species), 1)
  expect_true(is.numeric(information_gain(splitted_Species)))
  expect_gt(information_gain(splitted_Species), 0)
})
