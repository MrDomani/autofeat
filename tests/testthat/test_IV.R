context("information_value working properly")

data("Seatbelts")
Seatbelts <- as.data.frame(Seatbelts)
Seatbelts$law <- factor(Seatbelts$law)
splitted_law <- split(Seatbelts$law, kmeans(Seatbelts[,-8], 2)[['cluster']])
test_that("information_value working properly", {
  expect_length(IV(splitted_law), 1)
  expect_true(is.numeric(IV(splitted_law)))
  expect_gt(IV(splitted_law), 0)
})
