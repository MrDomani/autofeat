context('checks working properly')

data("iris")
data("freeny")
discretize <- function(x){
  factor(round(x * 10))
}
i <- sample(1:nrow(freeny.x), 0.7 * nrow(freeny.x))
freeny_train <- freeny.x[i,]
freeny_valid <- freeny.x[-i,]
test_that("check_dataset working properly",{
  expect_error(check_dataset(iris[,-5], iris$Species))
  expect_error(check_dataset(freeny.x[-1,], freeny.y))
  expect_error(check_dataset(freeny.x, freeny.x[,1:2]))
  expect_silent(check_dataset(freeny.x, discretize(freeny.y)))
})

test_that("check_train_valid",{
  expect_error(check_train_valid(freeny_train[,-1], freeny_valid))
  expect_silent(check_train_valid(freeny_train, freeny_valid))
})

test_that("check_operators working properly", {
  expect_error(check_operators(`+`))
  expect_error(check_operators(NULL,
                               NULL,
                               list(`+`)))
  expect_error(check_operators(list(max)))
  expect_silent(check_operators(list(NULL,
                                     `+`)))
  expect_silent(check_operators(list(NULL,
                                     list(`+`, `-`, `*`, `/`))))
})

test_that("check_n_iter working properly",{
  expect_error(check_n_iter("A"))
  expect_error(check_n_iter(1:2))
  expect_error(check_n_iter(-1))
})

test_that("check_splitted_labels working properly",{
  expect_error(check_splitted_labels(factor(LETTERS[1:5])))
  expect_error(check_splitted_labels(list()))
  expect_error(check_splitted_labels(list(a = 1:5, b = factor(LETTERS[1:5]))))
  expect_silent(check_splitted_labels(list(a = factor(LETTERS[c(1,1,1,2,2,3)]),
                                           b = factor(LETTERS[c(2,3,3)]))))
})
