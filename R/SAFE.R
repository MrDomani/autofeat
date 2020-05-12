#' Scalable Automatic Feature Engineering
#'
#' More description here
#'
#' @param X_train matrix
#' @param y_train factor
#' @param X_valid matrix
#' @param y_valid factor
#' @param operators A \code{list} of lists of functions. Ith list of funcitons contains functions accepting i vectors of equal length and returning 1 vector of the same length
#' @param n_iter Integer
#' @export

SAFE <- function(X_train, y_train, X_valid, y_valid,
                 operators = list(NULL, list(`+`, `-`, `*`, `/`)), n_iter){
  check_dataset(X_train, y_train)
  check_dataset(X_valid, y_valid)
  check_train_valid(X_train, X_valid)
}
