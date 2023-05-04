library(extremeStat)

get_delimeter <- function(percentile) {
  delimeter_by_data <- function(data) {
    delimeter <- q_gpd(unlist(data), package="extRemes", method="GMLE", truncate=0.5, probs=percentile)
    return(unlist(delimeter[1]))
  }
}