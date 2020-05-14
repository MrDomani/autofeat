#' Calculate information value
#'
#' @param splitted_labels A list of binary factors
#'
#' @inheritSection information_gain Details
#' @export
IV <- function(splitted_labels){
  check_splitted_binlabels(splitted_labels)
  all_labels <- do.call(forcats::fct_c, splitted_labels)
  general_levels <- attr(all_labels, "levels")
  props <- table(all_labels) / length(all_labels)
  n_p <- props[1]
  n_n <- props[2]
  sum(sapply(splitted_labels, function(label){
    label <- factor(label, general_levels)
    props <- table(label) / length(label)
    if(any(props == 0)) 0
    else
      (props[1]/ n_p - props[2] / n_n) * log(props[1] / n_p * n_n / props[2])
  }))
}


