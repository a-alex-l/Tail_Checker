get_percentile_delimeter <- function(percentile) {
  percentile_delimeter <- function(data) {
    delimeter <- unname(quantile(unlist(data), probs=percentile))
    return(delimeter)
  }
}