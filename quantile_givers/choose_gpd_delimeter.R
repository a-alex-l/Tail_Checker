library(extremeStat)

get_delimeter <- function(percentile) {
  delimeter_by_data <- function(data) {
    u_vec <- quantile(unlist(data), probs = c(0.95, 0.8, 0.7, 0.55))
    answer <- summary(ithresh(unlist(data), u_vec, n=100))
    p1 <- as.integer(length(data) * 0.5)
    p2 <- as.integer(length(data))
    delimeters <- c(
      q_gpd(unlist(data)       , package="extRemes", method="Lmoments", truncate=answer[4] / 100, probs=percentile)[1],
      q_gpd(unlist(data)       , package="extRemes", method="GMLE", truncate=answer[4] / 100, probs=percentile)[1],
      q_gpd(unlist(data[1:p1]) , package="extRemes", method="Lmoments", truncate=answer[4] / 100, probs=percentile)[1] * 0.5 + 
        q_gpd(unlist(data[p1:p2]), package="extRemes", method="GMLE", truncate=answer[4] / 100, probs=percentile)[1] * 0.5
      
    )
    if (abs(delimeters[3] - delimeters[1]) < abs(delimeters[3] - delimeters[2])) {
      return(delimeters[1])
    } else {
      return(delimeters[2])
    }
  }
}