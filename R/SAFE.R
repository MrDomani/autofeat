#' Scalable Automatic Feature Engineering
#'
#' Generate automatically new features based on older ones for further modelling,
#'  using SAFE algoritm proposed in a paper by Shi, Zhang, Li, Yang and Zhou.
#'  This is a direct implementation of the pseudo-algoritm proposed in the paper, with its conventions, denotements and flaws.
#'
#' @param X_train Matrix - data used to train model. Must be numerical.
#' @param y_train Factor - labels for training data. Must be \strong{binary}.
#' @param X_valid Matrix - data used to test model. Must be numerical.
#' @param y_valid Factor - labels for testing data. Must be \strong{binary}.
#' @param operators A \code{list} of lists of functions. Ith list of funcitons contains functions accepting \code{i} vectors of equal length and returning 1 vector of the same length.
#' @param n_iter Integer; Amount of iterations for the alghoritm to perform.
#' @param gamma Integer; Amount of most important feature combinations to be selected in each iteration.
#' @param nrounds Integer for \code{\link[xgboost]{xgboost}}.
#' @param alpha Threshold for \code{link{IV}}. Features with IV < alpha will be dropped.
#' @param bins Integer; amount of bins to create to discretize features.
#' @param theta Threshold for Pearson's correlation. Features with correlation above theta will be dropped.
#' @param beta Integer; Maximum amount of features to be selected at the end of each loop. Set to \code{Inf} to select all features.
#'
#' @return
#' A \code{list} with 2 elements: \code{X_train} and \code{X_test}.
#' Both contain transformed train and test sets, ready for further modelling.
#' Unfortunately, this is in contrary to algoritm mentioned in the paper (which returns a function) - at least for now.
#' @export

SAFE <- function(X_train, y_train, X_valid, y_valid,
                 operators = list(NULL, list(`+`, `-`, `*`)),
                 n_iter = 10,
                 nrounds = 5,
                 alpha = 0.1,
                 gamma = 10,
                 bins = 30,
                 theta = 0.8,
                 beta = Inf){
  # 0. check parameters
  check_dataset(X_train, y_train)
  check_dataset(X_valid, y_valid)
  check_train_valid(X_train, X_valid)
  check_operators(operators)
  check_n_iter(n_iter)
  check_nrounds(nrounds)
  check_alpha(alpha)
  check_gamma(gamma)
  check_bins(bins)
  check_theta(theta)
  check_beta(beta)
  # Though not mentioned explicitely in the paper, the algoritm is trained on both train and test data
  X <- rbind(X_train, X_valid)
  y <- forcats::fct_c(y_train, y_valid)
  for(i in 1:n_iter){
    # 1. train xgboost
    bst <- xgboost::xgboost(data = X, label = y, nrounds = nrounds, verbose = 0)

    # 2. Constitute feature combinations
    feat_combos <- constitute_feat_combos(bst)

    # 3. Sort and filter feature combinations

    selected_feat_combos <- sort_filter_combos(X, y, feat_combos, seq_lengths = which(sapply(operators, function(op) !is.null(op))), gamma = gamma)

    # 4. Generate new features
    new_feats <- lapply(selected_feat_combos, function(feat_combo){
      as.data.frame(lapply(operators[[length(feat_combo)]], function(op){
        do.call(op, as.data.frame(X[,feat_combo]))
      }))
    })
    new_feats <- data.matrix(do.call(cbind, new_feats))
    new_X <- cbind(X, new_feats)
    colnames(new_X) <- paste0("SAFE", 1:(ncol(new_feats) + ncol(X)))

    # 5. Remove features with low predictive power
    informative_X <- remove_uninformative_features(new_X, y, alpha = alpha, bins = bins)
    nonredundant_X <- remove_redundant_features(informative_X, y, bins = bins, theta = theta)

    # 6. Final feature sorting and selection
    final_bst <- xgboost::xgboost(data = nonredundant_X, label = y, nrounds = nrounds, verbose = 0)
    feat_gains <- calculate_avg_gain(final_bst, colnames(nonredundant_X))
    sorted_gains <- feat_gains[order(feat_gains$gain, decreasing = TRUE)[1:min(nrow(feat_gains), beta)],]
    X <- nonredundant_X[,sorted_gains[["Feature"]]]
    cat(paste0("\n", i, "th iteration out of ", n_iter, " complete"))
  }
  cat("\n")
  list(X_train = X[1:nrow(X_train),],
       X_valid = X[(nrow(X_train) + 1):nrow(X),])
}

#' Constitute Feature Combinations
#' @param bst An object of \code{xgb.Booster} class
#' @import data.table
#' @return
#' a list of data.frames with 2 columns: `Feature` (name of variable) and `Split` (value at which split in tree occured).
#' Single feature might contain multiple split values.
#' @noRd
constitute_feat_combos <- function(bst){
  nrounds <- bst$niter
  model_dt <- xgb.model.dt.tree(model = bst, trees = nrounds - 1, use_int_id = TRUE)
  temp_env <- rlang::new_environment()
  assign("result", list(), envir = temp_env)

  # Search the tree recursively to find all paths
  rec_search <- function(id, history){
    # history - vector
    # id - id of current node
    current_feature <- model_dt[Node == id, Feature]
    if(current_feature == 'Leaf'){
      assign("result", append(get("result", envir = temp_env), list(history)), envir = temp_env)
      return()
    }
    history <- rbind(history, data.frame(Feature = current_feature,
                                         Split = model_dt[Node == id, Split]))
    rec_search(model_dt[Node == id, Yes], history)
    rec_search(model_dt[Node == id, No], history)
  }
  # Initiate the search
  rec_search(0, data.frame(Feature = character(0),
                           Split = numeric(0)))

  # Return the result
  get("result", envir = temp_env)
}

#' Sort & filter feature combinations
#' @param X Matrix
#' @param y Vector of labels
#' @param feat_combos A list a list of data.frames with 2 columns: `Feature` and `Split`. See `constitue_feat_combos`.
#' @param seq_lengths Vector of integers with info combinations of how many features to consider.
#'     For instance, when user supplied only binary operators, only combinations of 2 features will be calculated.
#' @return A `list` of character vectors (feature combinations). Each vector contains feature names.
#' @noRd
sort_filter_combos <- function(X, y,feat_combos, seq_lengths, gamma){

  # job - a set of features and values to split by
  # To optimize (as suggested in the paper) the possible combinations are determined and redundant ones are filtered
  necessary_jobs <- determine_jobs(feat_combos, seq_lengths)

  # Split data
  labels_splitted <- lapply(necessary_jobs, function(jobs_q){
    lapply(jobs_q, function(job) execute_job(X, y, job))
  })

  # Drop the highest level
  labels_splitted <- unlist(labels_splitted, recursive = FALSE)

  # Calculate information gain
  scores <- sapply(labels_splitted, information_gain)

  # Return the most informative combinations
  unlisted_jobs <- unlist(necessary_jobs, recursive = FALSE)
  unlisted_job_names <- lapply(unlisted_jobs, function(job) unique(job$Feature))[order(scores, decreasing = TRUE)]
  out <- unique(unlisted_job_names)
  out[1:min(gamma, length(out))]
}

#' Determine necessary combinations to look at
#'
#' @param feat_combos A list a list of data.frames with 2 columns: `Feature` and `Split`. See `constitue_feat_combos`.
#' @param seq_lengths Vector of integers with info combinations of how many features to consider.
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
#' @param X Matrix
#' @param y Vector of labels
#' @param job A data.frame with 2 columns: Feature and Split. Feature must contain names from colnames(x)
#' @return A list of labels splitted accordingly `x` and `job`
#' @noRd

execute_job <- function(X, y, job){
  jobs_splitted <- split(job, factor(job$Feature))
  facs <- lapply(jobs_splitted, function(single_job){
    vec <- X[,single_job$Feature[1]]
    cut(vec, c(min(vec), single_job$Split, max(vec) + 1e-1), right = FALSE)
  })
  combined_facs <- do.call(forcats::fct_cross, facs)
  split(y, combined_facs)
}

#' Remove features with low predictive power
#'
#' @param X Matrix
#' @param y Factor of labels
#' @param alpha threshold > 0. Features with IV < alpha will be discarded.
#' @param bins integer
#' @noRd

remove_uninformative_features <- function(X, y, alpha, bins){
  IVs <- calculate_IVs(X, y, bins)
  X[,IVs >= alpha]
}

#' Calculate information values
#'
#' @param X Matrix
#' @param y Factor of labels
#' @param bins Integer
#' @noRd
calculate_IVs <- function(X, y, bins){
  apply(X, 2, function(feat){
    breaks <- sort(feat)[round(1:(bins - 1) / bins * length(feat))]
    breaks <- unique(c(min(feat) - 1e-2, breaks, max(feat)))
    if(length(breaks) >= 3){
     bin_assignments <- forcats::fct_drop(cut(feat, breaks))
     y_splitted <- split(y, bin_assignments)
     IV(y_splitted)
    }
    # If length(breaks) == 2 then no bins were created - the variable is constant and has no information
    else 0
  })
}

#' Remove highly correlated features
#'
#' @param X Matrix
#' @param y Factor of labels
#' @param alpha threshold > 0. Features with IV < alpha will be discarded.
#' @param theta Threshold
#' @noRd

remove_redundant_features <- function(X, y, bins, theta){
  cors <- cor(X)
  IVs <- calculate_IVs(X, y, bins)
  indices <- which.matrix(abs(cors) >= theta, ncol(X))
  final_indices <- sapply(indices, function(i){
    i[which.max(IVs[i])]
  })
  X[,unique(final_indices)]
}

#' Which indices in matrix are TRUE?
#'
#' @param logvec - logical vector, created by `mat` `operator` `value`, i.e. M > 0
#' @param ncol number of columns in matrix
#'
#' @return A list of pairs (row_index, col_index) of elements in matrix which are TRUE.
#' Diagonals omitted
#' @noRd

which.matrix <- function(logvec, ncol){
  wh <- which(logvec)
  l <- lapply(wh, function(w){
    modulos <- w %% ncol
    out <- c(replace(modulos, modulos == 0, ncol), ceiling(w / ncol))
    if(out[1] > out[2]) NULL
    else out
  })
  l[sapply(l, function(el) !is.null(el))]
}


calculate_avg_gain <- function(bst, all_feats){
  dt <- xgboost::xgb.model.dt.tree(model = bst)
  gains <- dt[Feature != "Leaf", .(gain = mean(Quality)),Feature]
  all_gains <- merge(data.frame(Feature = all_feats), gains, all.x = TRUE)
  all_gains[is.na(all_gains[["gain"]]), "gain"] <- 0
  all_gains
}

