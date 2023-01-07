TARGET_TN <- 0.999
SEED <- 5
TEST_COUNT <- 100
DATA_SIZE <- 1000
set.seed(SEED)


get_lnorm_data <- function(size, count) {
  answer <- list()
  params <- rlnorm(2, 0, 1)
  for (x in 1:count) {
    answer <- append(answer, rlnorm(size, params[1], params[2]))
  }
  ans <- list(
    "delimeter" = unname(qlnorm(TARGET_TN, params[1], params[2])),
    "data" = matrix(answer, nrow=TEST_COUNT, ncol=DATA_SIZE)
  )
}


test <- function(data, algorithm) {
  answer <- list()
  for (i in 1:TEST_COUNT) {
    answer <- append(answer, algorithm(as.numeric(data[i,])))
  }
  return(unlist(answer))
}


dataset <- get_lnorm_data(DATA_SIZE, TEST_COUNT)
print(dataset$delimeter)

source("percentile_delimeter.R")
percentile_delimeter_ans <- test(dataset$data, get_percentile_delimeter(TARGET_TN))
hist(percentile_delimeter_ans, breaks=20, col="red")
abline(v=dataset$delimeter, col="green", lwd=4)
abline(v=mean(percentile_delimeter_ans), col="red", lwd=3, lty=2)
print(mean(percentile_delimeter_ans))

source("gpd_delimeter.R")
gpd_delimeter_ans <- test(dataset$data, get_approx_delimeter(TARGET_TN))
hist(gpd_delimeter_ans, breaks=20, col="blue")
abline(v=dataset$delimeter, col="green", lwd=4)
abline(v=mean(gpd_delimeter_ans), col="blue", lwd=2)
print(mean(gpd_delimeter_ans))

source("approx_delimeter.R")
approx_delimeter_ans <- test(dataset$data, get_approx_delimeter(TARGET_TN))
hist(approx_delimeter_ans, breaks=20, col="green")
abline(v=dataset$delimeter, col="green", lwd=4)
abline(v=mean(approx_delimeter_ans), col="green", lwd=2)
print(mean(approx_delimeter_ans))

