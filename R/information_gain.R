#' Calculate information gain
#'
#' Based on changed entropy of the data, calculated information gain.
#'
#' @param splitted_labels A list. Each element of it is a factor of labels.
#'
#' @details It is assumed, that y are splitted labels of a greater dataset.
#' When combined together, they should form a vector equivalent to labels in original dataset (order may differ).
#' @export
information_gain <- function(splitted_labels){
  entropies <- sapply(splitted_labels, calculate_entropy)
  all_labels <- do.call(forcats::fct_c, splitted_labels)
  weights <- sapply(splitted_labels, function(vec) length(vec) / length(all_labels))
  weighted_entropies <- entropies * weights
  calculate_entropy(all_labels) - sum(weighted_entropies)
}


#'Calculate entropy in a dataset with labels
#'@param labels A factor
#' @noRd
calculate_entropy <- function(labels){
  props <- table(labels) / length(labels)
  props <- props[props > 0]
  -sum(props * log2(props))
}
