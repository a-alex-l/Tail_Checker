library(gtools)

get_data <- function(size) {
  data <- as.numeric(read.csv("tests/random_ping_51k.txt", sep=" ",
                              check.names=FALSE, header=FALSE))
  data <- data[1:size]
  params <- list()
  current_params <- runif(2, 1, 2)
  for (i in 1:size) {
    params <- append(params, current_params)
  }
  ans <- list("data" = data, "params" = params)
}


get_pos <- function(q, data, start, end) {
  mid = as.integer((start + end) / 2)
  if (end == start + 1) return(start)
  if (data[mid] <= q) {
    return(get_pos(q, data, mid, end))
  } else {
    return(get_pos(q, data, start, mid))
  }
}


get_positions <- function(quantiles, params, size) {
  data <- as.numeric(read.csv("tests/random_ping_51k.txt", sep=" ",
                              check.names=FALSE, header=FALSE))
  n <- length(data)
  data <- sort(data)
  positions <- list()
  for (i in 1:size) {
    pos <- get_pos(quantiles[i], data, 1, n)
    start <- max(0, pos - 3)
    end <- min(pos + 3, n)
    probs <- seq(from=start / (1 + n), to=end / (1 + n), length.out=1 + end - start)
    ones <- rep(1, 1 + end - start)
    input <- list('data'=probs, 'x0'=ones, 'x1'=unlist(data[start:end]))
    relation <- lm(data~x0 + x1, input)
    pred <- predict(relation, list('x0'=1, 'x1'=quantiles[i]))
    positions <- append(positions, min(1, max(0, pred)))
  }
  return(positions)
}