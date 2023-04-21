library(extremeStat)

get_delimeter <- function(percentile) {
  delimeter_by_data <- function(data) {
    lower_border <- 0.1 / (1 - percentile)
    upper_border <- 0.3 / (1 - percentile)
    if (length(data) < lower_border) {
      delimeter <- q_gpd(unlist(data), package="extRemes", method="Lmoments", truncate=0.8, probs=percentile)
    } else {
      if (length(data) > upper_border) {
        delimeter <- q_gpd(unlist(data), package="extRemes", method="GMLE", truncate=0.8, probs=percentile)
      } else {
        delimeter_lmom <- q_gpd(unlist(data), package="extRemes", method="Lmoments", truncate=0.8, probs=percentile)
        delimeter_gmle <- q_gpd(unlist(data), package="extRemes", method="GMLE", truncate=0.8, probs=percentile)
        alpha <- (length(data) - lower_border) / (upper_border - lower_border)
        delimeter <- delimeter_lmom[1] * alpha + delimeter_gmle[1] * (1 - alpha)
      }
    }
    return(unlist(delimeter[1]))
  }
}