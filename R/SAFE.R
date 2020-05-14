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
    new_feats <- lapply(selected_feat_combos, function(feat_combo){
      as.data.frame(lapply(operators[[length(feat_combo)]], function(op){
        do.call(op, as.data.frame(X_train[,feat_combo]))
      }))
    })
    new_feats <- data.matrix(do.call(cbind, new_feats))
    new_X <- cbind(X_train, new_feats)
    colnames(new_X) <- paste0("SAFE", 1:ncol(new_feats))
  }
}

#' Constitute Feature Combinations
#' @import data.table
#' @return a list of data.frames with pairs feature-value. Single feature might contain multiple values
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

#' Sort & filter feature combinations
#'
#' @noRd
sort_filter_combos <- function(feat_combos, top_n){
  # job - a set of features and values to split by
  necessary_jobs <- determine_jobs(feat_combos, which(sapply(operators, function(op) !is.null(op))))
  labels_splitted <- lapply(necessary_jobs, function(jobs_q){
    lapply(jobs_q, function(job) execute_job(X_train, y_train, job))
  })
  scores <- lapply(labels_splitted, function(labels_q){
    sapply(labels_q, information_gain)
  })
  unlisted_scores <- unlist(scores, recursive = FALSE)
  unlisted_jobs <- unlist(necessary_jobs, recursive = FALSE)
  unlisted_job_names <- lapply(unlisted_jobs, function(job) job$Feature)[order(unlisted_scores, decreasing = TRUE)]
  unique(unlisted_job_names)[1:top_n]
}

#' Determine necessary combinations to look at
#'
#' @param feat_combos A list of data.frames from constitute_feat_combos
#' @param seq_lengths Vector of lenghts of supplied operators. Combinations of how many features take into account?
#' @return A nested list. On top of length == length(seq_lengths). Each element is a list of data.frames. Each of data.frames is a job to execute
#' @noRd
determine_jobs <- function(feat_combos, seq_lengths){
  lapply(seq_lengths, function(q){
    l <- lapply(feat_combos, function(combo){
      feats <- unique(combo$Feature)
      if(length(feats) < q) return(NULL)
      all_combos <- combn(feats, q)
      apply(all_combos, MARGIN = 2, function(row){
        merge(data.frame(Feature = row), combo)
      })
    })
    unique(unlist(l, recursive = FALSE))
  })
}

#' Split data
#'
#' Split data according to info in `job`
#' @param x Matrix
#' @param y Vector of labels
#' @param job A data.frame with 2 columns: Feature and Split. Feature must contain names from colnames(x)
#' @return A list of labels splitted accordingly `x` and `job`
#' @noRd

execute_job <- function(x, y, job){
  jobs_splitted <- split(job, factor(job$Feature))
  facs <- lapply(jobs_splitted, function(single_job){
    vec <- x[,single_job$Feature[1]]
    cut(vec, c(min(vec), single_job$Split, max(vec) + 1e-1), right = FALSE)
  })
  combined_facs <- do.call(forcats::fct_cross, facs)
  split(y, combined_facs)
}
