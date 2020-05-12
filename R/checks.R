#' Check dataset
#'
#' Check format and data type of input dataset (train or valid)
#' @noRd

check_dataset <- function(X, y){
  X_name <- paste0("`", as.character(substitute(X)), "`")
  y_name <- as.character(substitute(y))
  if(!is.matrix(X)) rlang::abort(paste0(X_name,
                                        " must be of matrix class, not ",
                                        class(X)[1]))
  if(!is.factor(y)) rlang::abort(paste0(y_name,
                                        " must be of factor class, not ",
                                        class(y)[1]))
  if(length(y) != nrow(X)) rlang::abort(paste0(X_name,
                                               " and ",
                                               y_name,
                                               "must be of equal length"))
}

#' Check train-valid
#'
#' Check format of train and valid data
#'
#' @noRd
check_train_valid <- function(X_train, X_valid){
  if(ncol(X_train) != ncol(X_valid)) rlang::abort("`X_train` and `X_valid` must have equal number of columns")
}

#' Check operators
#'
#' Is it a list of lists?
#' @importFrom stats rnorm
#' @noRd
#'
check_operators <- function(operators){
  if(!is.list(operators)) rlang::abort(paste0("`operators` must be of list class, not ", class(operators)[1]))
  for(i in 1:length(operators)){
    functions <- operators[[i]]
    if(is.function(functions)) functions <- list(functions)
    for(fs in functions){
      if(is.null(fs)) next()
      if(!is.function(fs)) rlang::abort("all elements of sublists in `operators` must be of function class")
      res <- do.call(fs, lapply(1:i, function(j) rnorm(10)))
      if(!is.numeric(res) || length(res) != 10) rlang::abort("Functions supplied in `operators` must return a single numeric vector of length equal to input vectors")
    }
  }
}

#' Check n_iter
#'
#' @noRd
check_n_iter <- function(n_iter){
  if(!is.integer(n_iter) || n_iter < 1 || length(n_iter) != 1) rlang::abort("`n_iter` must be a single positive integer")
}
