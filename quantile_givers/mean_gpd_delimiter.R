library(threshr)
library(extremeStat)

treshold_global <- {}
treshold_global.trsh <- -1
get_delimeter <- function(percentile) {
  delimeter_by_data <- function(data) {
    if (treshold_global.trsh < 0 || sample(1:5000, 1) == 1) {
      u_vec <- quantile(unlist(data), probs = c(0.85, 0.8, 0.7, 0.55))
      treshold_global.trsh <<- ithresh(unlist(data), u_vec, trans = "BC",  n=100)$v_ps / 100
    }
    delimeters <- c(
      q_gpd(unlist(data)       , package="extRemes", method="Lmoments", truncate=treshold_global.trsh, probs=percentile)[1],
      q_gpd(unlist(data)       , package="extRemes", method="GMLE", truncate=treshold_global.trsh, probs=percentile)[1]
    )
    return(mean(unlist(delimeters)))
  }
}