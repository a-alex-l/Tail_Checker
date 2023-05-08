library(threshr)
library(extremeStat)

treshold_global <- {}
treshold_global.trsh <- -1
get_delimeter <- function(percentile) {
  delimeter_by_data <- function(data) {
    if (treshold_global.trsh < 0 || sample(1:1000, 1) == 1) {
      u_vec <- quantile(unlist(data), probs = c(0.85, 0.8, 0.7, 0.55))
      treshold_global.trsh <<- ithresh(unlist(data), u_vec, trans = "BC",  n=100)$v_ps / 100
    }
    delimeter <- q_gpd(unlist(data), package="fExtremes", truncate=treshold_global.trsh, probs=percentile)
    return(unlist(delimeter[1]))
  }
}