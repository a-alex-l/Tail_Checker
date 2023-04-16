library(extremeStat)

get_delimeter <- function(percentile) {
  delimeter_by_data <- function(data) {
    delimeters <- c(
      q_gpd(unlist(data), package="extRemes", method="Lmoments", truncate=0.8, probs=percentile)[1],
      q_gpd(unlist(data), package="evir", truncate=0.8, probs=percentile)[1],
      q_gpd(unlist(data), package="fExtremes", truncate=0.8, probs=percentile)[1]
    )
    return(mean(unlist(delimeters)) * 0.7 + median(unlist(delimeters)) * 0.3)
  }
}