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
  if(length(attr(y, "levels")) != 2) rlang::abort(paste0(y_name,
                                        " must be of 2 levels, not ",
                                        length(attr(y, "levels"))))
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
  if(!rlang::is_integerish(n_iter) || n_iter < 1 || length(n_iter) != 1) rlang::abort("`n_iter` must be a single positive integer")
}

#' Check nrounds
#'
#' @noRd
check_nrounds <- function(nrounds){
  if(!rlang::is_integerish(nrounds) || nrounds < 1 || length(nrounds) != 1) rlang::abort("`nrounds` must be a single positive integer")
}

#' Check alpha
#'
#' @noRd
check_alpha <- function(alpha){
  if(!is.numeric(alpha) || alpha <= 0 || length(alpha) != 1) rlang::abort("`alpha` must be a single positive number")
}

#' Check gamma
#'
#' @noRd
check_gamma <- function(gamma){
  if(!rlang::is_integerish(gamma) || gamma < 1 || length(gamma) != 1) rlang::abort("`gamma` must be a single positive integer")
}

#' Check bins
#'
#' @noRd
check_bins <- function(bins){
  if(!rlang::is_integerish(bins) || bins < 1 || length(bins) != 1) rlang::abort("`bins` must be a single positive integer")
}

#' Check theta
#'
#' @noRd
check_theta <- function(theta){
  if(!is.numeric(theta) || theta <= 0 || length(theta) != 1) rlang::abort("`theta` must be a single positive number")
}

#' Check beta
#'
#' @noRd
check_beta <- function(beta){
  if(!rlang::is_integerish(beta) || beta < 1 || length(beta) != 1) rlang::abort("`beta` must be a single positive integer")
}

#' Check splitted_labels - information_gain function
#'
#' @noRd
check_splitted_labels <- function(labels){
  if(!is.list(labels)) rlang::abort(paste0("`operators` must be of list class, not ", class(labels)[1]))
  if(length(labels) == 0) rlang::abort("`splitted_labels` is empty")
  if(!all(sapply(labels, is.factor))) rlang::abort("all elements of `splitted_labels` must be of factor class")
}

#' Check splitted_labels - information_value function
#'
#' @noRd
check_splitted_binlabels <- function(labels){
  check_splitted_labels(labels)
  if(!all(sapply(labels, function(lab)length(attr(lab, "levels")) <= 2))) rlang::abort("all elements of `splitted_labels` must be a binary factor")
}
