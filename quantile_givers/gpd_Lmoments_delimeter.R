library(threshr)
library(extremeStat)

get_delimeter <- function(percentile) {
  delimeter_by_data <- function(data) {
    # u_vec <- quantile(unlist(data), probs = c(0.8, 0.65, 0.5, 0.35))
    # answer <- summary(ithresh(unlist(data), u_vec, n=100))
    # delimeter <- q_gpd(unlist(data), package="extRemes", method="Lmoments", truncate=answer[4] / 100, probs=percentile)
    delimeter <- q_gpd(unlist(data), package="extRemes", method="Lmoments", truncate=0.5, probs=percentile)
    return(unlist(delimeter[1]))
  }
}