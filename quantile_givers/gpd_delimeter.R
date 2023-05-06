library(extremeStat)

get_delimeter <- function(percentile) {
  delimeter_by_data <- function(data) {
    u_vec <- quantile(unlist(data), probs = c(0.95, 0.8, 0.7, 0.55))
    answer <- summary(ithresh(unlist(data), u_vec, n=100))
    delimeter <- q_gpd(unlist(data), package="fExtremes", truncate=answer[4] / 100, probs=percentile)
    return(unlist(delimeter[1]))
  }
}