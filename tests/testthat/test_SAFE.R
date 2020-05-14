context("SAFE works properly")

library(xgboost)
data("agaricus.train", package = "xgboost")
X <- as.matrix(agaricus.train$data)
y <- factor(agaricus.train$label)

i <- sample(1:nrow(X), round(0.3 * nrow(X)))
X_train <- X[i,]
y_train <- y[i]
X_valid <- X[-i,]
y_valid <- y[-i]
res <- SAFE(X_train, y_train, X_valid, y_valid)
test_that("SAFE works properly",{
  expect_length(res, 2)
  expect_equal(names(res), c("X_train", "X_valid"))
  expect_true(nrow(res[[1]]) == nrow(X_train))
  expect_true(nrow(res[[2]]) == nrow(X_valid))
})
