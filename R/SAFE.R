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
#' @param nrounds Integer for \code{\link[xgboost]{xgboost}}
#' @export

SAFE <- function(X_train, y_train, X_valid, y_valid,
                 operators = list(NULL, list(`+`, `-`, `*`, `/`)), n_iter, nrounds){
  # 0 - check parameters
  check_dataset(X_train, y_train)
  check_dataset(X_valid, y_valid)
  check_train_valid(X_train, X_valid)
  check_operators(operators)
  check_n_iter(n_iter)

  X <- rbind(X_train, X_valid)
  for(i in 1:n_iter){
    # 1. train xgboost
    bst <- xgboost(data = X_train, label = y_train, nrounds = nrounds)

    # 2. Constitute feature combinations
    feat_combos <- constitute_feat_combos(bst)

    # 3. Sort and filter feature combinations

    selected_feat_combos <- sort_filter_combos(feat_combos, top_n)





  }
}

#' Constitute Feature Combinations
#' @import data.table
#' @return a list of vectors of feature names
#' @noRd
constitute_feat_combos <- function(bst){
  nrounds <- bst$niter
  model_dt <- xgb.model.dt.tree(model = bst, trees = nrounds - 1, use_int_id = TRUE)
  temp_env <- rlang::new_environment()
  assign("result", list(), envir = temp_env)
  rec_search <- function(id, history){
    # print(history)
    # history - vector
    # id - id of current node
    current_feature <- model_dt[Node == id, Feature]
    if(current_feature == 'Leaf'){
      assign("result", append(get("result", envir = temp_env), list(history)), envir = temp_env)
      # print(get("result", envir = temp_env))
      return()
    }
    history <- rbind(history, data.frame(Feature = current_feature,
                                         Split = model_dt[Node == id, Split]))
    rec_search(model_dt[Node == id, Yes], history)
    rec_search(model_dt[Node == id, No], history)
  }
  rec_search(0, data.frame(Feature = character(0),
                           Split = numeric(0)))
  get("result", envir = temp_env)
}

