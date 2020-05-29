# devtools::install_github("MrDomani/autofeat")
library(autofeat)
library(mlr)

data("titanic_imputed", package = "DALEX")

i <- sample(1:nrow(titanic_imputed), size = round(0.7 * nrow(titanic_imputed)))

X_train <- data.matrix(titanic_imputed[i,c("age", "fare", "sibsp", "parch")])
y_train <- factor(titanic_imputed$survived[i])

X_test <- data.matrix(titanic_imputed[-i,c("age", "fare", "sibsp", "parch")])
y_test <- factor(titanic_imputed$survived[-i])

X_SAFEd <- SAFE(X_train, y_train, X_test, y_test, n_iter = 5)
X_train_SAFEd <- X_SAFEd$X_train
X_test_SAFEd <- X_SAFEd$X_valid

task1 <- makeClassifTask("original_train", data.frame(X_train, target = y_train), "target")
task2 <- makeClassifTask("SAFE_train", data.frame(X_train_SAFEd, target = y_train), "target")

task1_test <- makeClassifTask("original_test", data.frame(X_test, target = y_test), "target")
task2_test <- makeClassifTask("SAFE_test", data.frame(X_test_SAFEd, target = y_test), "target")

lrn <- makeLearner("classif.ranger", predict.type = "prob")

crossval(lrn, task1, measures = list(auc, ppv))$aggr
crossval(lrn, task2, measures = list(auc, ppv))$aggr

ranger1 <- train(lrn, task1)
ranger2 <- train(lrn, task2)

performance(predict(ranger1, task1), auc)
performance(predict(ranger2, task2), auc)

performance(predict(ranger1, task1_test), auc)
performance(predict(ranger2, task2_test), auc)
