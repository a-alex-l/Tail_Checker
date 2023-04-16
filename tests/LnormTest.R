get_data <- function(size) {
  data <- list()
  params <- list()
  current_params <- runif(2, 1, 2)
  for (i in 1:size) {
    params <- append(params, current_params)
    data <- append(data, rlnorm(1, current_params[1], current_params[2]))
    current_params[0] <- current_params[0] + runif(1, -current_params[0] / 200, current_params[0] / 100)
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