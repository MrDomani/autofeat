context("SAFE utils working properly")

library(xgboost)
data("agaricus.train")
data(iris)
expected_split <- list(iris[iris$Sepal.Length < 5 & iris$Sepal.Width < 3,"Species"],
                       iris[iris$Sepal.Length >= 5 & iris$Sepal.Length < 6 & iris$Sepal.Width < 3,"Species"],
                       iris[iris$Sepal.Length >= 6 & iris$Sepal.Width < 3,"Species"],
                       iris[iris$Sepal.Length < 5 & iris$Sepal.Width >= 3,"Species"],
                       iris[iris$Sepal.Length >= 5 & iris$Sepal.Length < 6 & iris$Sepal.Width >= 3,"Species"],
                       iris[iris$Sepal.Length >= 6 & iris$Sepal.Width >= 3,"Species"])
bst <- xgboost(data = agaricus.train$data, label = agaricus.train$label, nrounds = 5)
custom_feat_combos <- list(data.frame(Feature = LETTERS[1:3],
                                      Split = 1:3),
                           data.frame(Feature = LETTERS[1:4][-2],
                                      Split = 1:3),
                           data.frame(Feature = LETTERS[3:6],
                                      Split = 4:1))


test_that("feat_combos pulled properly",{
  expect_silent(feat_combos <- constitute_feat_combos(bst))
  expect_true(all(sapply(feat_combos,
                         function(feats) length(setdiff(feats[,"Feature"], agaricus.train$data@Dimnames[[2]])) == 0 &&
                           is.numeric(feats[,"Split"]))))
})

test_that("determine_jobs working properly", {
  expect_silent(cfc <- determine_jobs(custom_feat_combos, 2:3))
  expect_length(cfc, 2)
  expect_true(all(sapply(cfc, is.list)))
  expect_true(all(sapply(cfc, function(ls){
    all(sapply(ls, is.data.frame))
  })))
})

test_that("execute_job working properly", {
  expect_silent(res <- execute_job(iris[-5], iris$Species, data.frame(Feature = c("Sepal.Length", "Sepal.Length", "Sepal.Width"),
                                             Split = c(5,6,3))))
  expect_setequal(res, expected_split)
})
