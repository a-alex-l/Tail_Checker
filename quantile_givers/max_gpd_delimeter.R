library(extremeStat)

get_delimeter <- function(percentile) {
  delimeter_by_data <- function(data) {
    u_vec <- quantile(unlist(data), probs = c(0.95, 0.8, 0.7, 0.55))
    answer <- summary(ithresh(unlist(data), u_vec, n=100))
    p1 <- as.integer(length(data) * 0.5)
    p2 <- as.integer(length(data))
    delimeters <- c(
      q_gpd(unlist(data)       , package="extRemes", method="Lmoments", truncate=answer[4] / 100, probs=percentile)[1],
      q_gpd(unlist(data)       , package="extRemes", method="GMLE", truncate=answer[4] / 100, probs=percentile)[1]
    )
    return(max(unlist(delimeters)))
  }
}