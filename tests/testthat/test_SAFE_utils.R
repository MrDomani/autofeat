context("SAFE utils working properly")

library(xgboost)
data("agaricus.train")
bst <- xgboost(data = agaricus.train$data, label = agaricus.train$label, nrounds = 5)

test_that("feat_combos pulled properly",{
  expect_silent(feat_combos <- constitute_feat_combos(bst))
  expect_true(all(sapply(feat_combos,
                         function(feats) length(setdiff(feats[,"Feature"], agaricus.train$data@Dimnames[[2]])) == 0 &&
                           is.numeric(feats[,"Split"]))))
})
