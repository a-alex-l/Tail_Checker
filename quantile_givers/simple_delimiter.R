get_delimeter <- function(percentile) {
  delimeter_by_data <- function(data) {
    delimeter <- quantile(unlist(data), probs=percentile)
    return(unlist(delimeter[1]))
  }
}