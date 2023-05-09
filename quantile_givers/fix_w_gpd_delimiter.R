library(threshr)
library(extremeStat)

get_best_delimeter <- function(dl, dr, weights, data, petcentiles, petcentile, sd) {
  for (i in 1:12) {
    if (dr == dl + 1) return (data[dl])
    dm <- as.integer(dl / 2 + dr / 2)
    m_norm <- dnorm(data, mean=data[dm], sd=sd)
    m_norm[is.na(m_norm)] <- 0 # Fixing Nan
    pm <- sum(petcentiles * m_norm * weights) / sum(weights * m_norm)
    if (pm < petcentile) {
      dl <- dm
    } else {
      dr <- dm
    }
  }
  return(data[as.integer(dl / 2 + dr / 2)])
}

treshold_global <- {}
treshold_global.trsh <- -1
get_delimeter <- function(percentile) {
  delimeter_by_data <- function(data) {
    if (treshold_global.trsh < 0 || sample(1:5000, 1) == 1) {
      u_vec <- quantile(unlist(data), probs = c(0.85, 0.8, 0.7, 0.55))
      treshold_global.trsh <<- ithresh(unlist(data), u_vec,  n=100)$v_ps / 100
    }
    n <- 1000
    petcentiles <- c(seq(from=0.9, to=0.975, length.out=200),
                     seq(from=0.975, 0.999, length.out=400),
                     seq(from=0.999, 0.9999, length.out=400))
    p1 <- as.integer(length(data) * 0.5)
    p2 <- as.integer(length(data))
    delimeters_lmom <- q_gpd(unlist(data)       , package="extRemes", method="Lmoments", truncate=treshold_global.trsh, probs=petcentiles)
    delimeters_gmle <- q_gpd(unlist(data)       , package="extRemes", method="GMLE", truncate=treshold_global.trsh, probs=petcentiles)
    delimeters_mix <- q_gpd(unlist(data[1:p1]) , package="extRemes", method="Lmoments", truncate=treshold_global.trsh, probs=petcentiles) * 0.5 + 
      q_gpd(unlist(data[p1:p2]), package="extRemes", method="GMLE", truncate=treshold_global.trsh, probs=petcentiles) * 0.5
    delimeters <- (abs(delimeters_mix - delimeters_lmom) * delimeters_gmle +
                     abs(delimeters_mix - delimeters_gmle) * delimeters_lmom) / 
                  (abs(delimeters_mix - delimeters_lmom) +
                     abs(delimeters_mix - delimeters_gmle))
    sd <- delimeters[601] - delimeters[201]
    weights <- delimeters[2:n] - delimeters[1:n - 1]
    ans <- get_best_delimeter(1, n - 1, weights,
                              delimeters[2:n], petcentiles[2:n],
                              percentile, sd=sd)
    return(unlist(ans))
  }
}