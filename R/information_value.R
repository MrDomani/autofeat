#' Calculate information value
#'
#' @param splitted_labels A list of binary factors
#'
#' @inheritSection information_gain Details
#' @export
IV <- function(splitted_labels){
  check_splitted_binlabels(splitted_labels)

  # Make sure, that all labels have correct levels
  all_labels <- do.call(forcats::fct_c, splitted_labels)
  general_levels <- attr(all_labels, "levels")

  # Calculate proportions of each level
  props <- table(all_labels) / length(all_labels)
  n_p <- props[1]
  n_n <- props[2]
  sum(sapply(splitted_labels, function(label){
    label <- factor(label, general_levels)
    props <- table(label) / length(label)
    # When only values of 1 level are present, the IV is uncalculable, but should be considered as high
    if(any(props == 0)) 0.5
    else
      (props[1]/ n_p - props[2] / n_n) * log(props[1] / n_p * n_n / props[2])
  }))
}


