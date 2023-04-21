get_data <- function(size) {
  data <- list()
  params <- list()
  current_params <- runif(2, 1, 2)
  for (i in 1:size) {
    params <- append(params, current_params)
    data <- append(data, rlnorm(1, current_params[1], current_params[2]))
    if (sample(1:10, 1) == 1) {
      current_params[1] <- current_params[1] * runif(1, 0.975, 1.1)
    }
  }
  ans <- list("data" = data, "params" = params)
}


get_positions <- function(quantiles, params, size) {
  positions <- list()
  for (i in 1:size) {
    positions <- append(positions, plnorm(quantiles[i], params[[i * 2 - 1]], params[[i * 2]]))
  }
  return(positions)
}