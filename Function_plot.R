source("quantile_givers/gpd_delimeter.R")

get_lnorm_data <- function(size, count) {
  answer <- list()
  params <- rlnorm(2, 0, 1)
  for (x in 1:count) {
    answer <- append(answer, rlnorm(size, params[1], params[2]))
  }
  ans <- list(
    "delimeter" = unname(qlnorm(TARGET_TN, params[1], params[2])),
    "data" = matrix(answer, nrow=1, ncol=DATA_SIZE)
  )
}


dataset <- get_lnorm_data(DATA_SIZE, 1)

answer <- list()
for (i in 50:10000) {
  retry_ans <- list()
  for (j in 1:max(10, 10000 / i)) {
    set.seed(j * 10 + i ** 2 + 1)
    rng <- rlnorm(i, 2, 1)
    delimeter <- unname(qlnorm(0.99, 2, 1))
    count = length(rng[rng >= delimeter])
    retry_ans <- append(retry_ans, count)
  }
  answer <- append(answer, mean(as.numeric(retry_ans)))
}

plot(50:10000, answer)

