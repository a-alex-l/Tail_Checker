library(extremeStat)

get_delimeter <- function(percentile) {
  delimeter_by_data <- function(data) {
    delimeter <- q_gpd(unlist(data), package="evir",truncate=0.8, probs=percentile)
    return(unlist(delimeter[1]))
  }
}