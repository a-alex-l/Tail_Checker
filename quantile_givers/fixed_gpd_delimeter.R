library(threshr)
library(extremeStat)


get_best_delimeter <- function(dl, dr, weights, data, petcentiles, petcentile, sd) {
  for (i in 1:1000) {
    dm <- dl / 2 + dr / 2
    m_norm <- dnorm(data, mean=dm, sd=sd)
    m_norm[is.na(m_norm)] <- 0 # Fixing Nan
    pm <- sum(petcentiles * m_norm * weights) / sum(weights * m_norm)
    if (abs(pm - petcentile) < 0.00001) return (dm)
    if (pm < petcentile) {
      dl <- dm
    } else {
      dr <- dm
    }
  }
  return (dl / 2 + dr / 2)
}


treshold_global <- {}
treshold_global.trsh <- -1
get_delimeter <- function(percentile) {
  delimeter_by_data <- function(data) {
    if (treshold_global.trsh < 0 || sample(1:500, 1) == 1) {
      u_vec <- quantile(unlist(data), probs = c(0.85, 0.8, 0.7, 0.55))
      treshold_global.trsh <<- ithresh(unlist(data), u_vec, trans = "BC",  n=100)$v_ps / 100
    }
    n <- 1000
    petcentiles <- seq(from=1 - (1 - percentile) * 10,
                       to=1 - (1 - percentile) / 10,
                       length.out=n)
    delimeters <- q_gpd(unlist(data), package="extRemes", method="Lmoments", truncate=treshold_global.trsh, probs=petcentiles)
    weights <- delimeters[2:n] - delimeters[1:n - 1]
    ans <- get_best_delimeter(min(delimeters), max(delimeters), weights,
                              delimeters[2:n], petcentiles[2:n],
                              percentile, sd=sd(delimeters))
    return(unlist(ans))
  }
}