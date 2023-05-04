library(extremeStat)

get_delimeter <- function(percentile) {
  delimeter_by_data <- function(data) {
    p1 <- as.integer(length(data) * 0.5)
    p2 <- as.integer(length(data))
    delimeters <- c(
      q_gpd(unlist(data)       , package="extRemes", method="Lmoments", truncate=0.5, probs=percentile)[1],
      q_gpd(unlist(data)       , package="extRemes", method="GMLE", truncate=0.5, probs=percentile)[1]
    )
    return(max(unlist(delimeters)))
  }
}