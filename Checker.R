library(foreach)
library(doFuture)
registerDoFuture()
plan(multisession, workers = 6)

TARGET_TN <- 0.99
SEED <- 276
TEST_COUNT <- 1000
DATA_SIZE <- 100
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
  answer <- foreach (i = 1:TEST_COUNT, .combine=c) %dopar% {
    algorithm(as.numeric(data[i,]))
  }
  return(answer)
}


dataset <- get_lnorm_data(DATA_SIZE, TEST_COUNT)

source("percentile_delimeter.R")
percentile_delimeter_ans <- test(dataset$data, get_percentile_delimeter(TARGET_TN))
hist_percentile <- hist(percentile_delimeter_ans, breaks=20, plot = FALSE)

source("gpd_delimeter.R")
gpd_delimeter_ans <- test(dataset$data, get_gpd_delimeter(TARGET_TN))
hist_gpd <- hist(gpd_delimeter_ans, breaks=20, plot = FALSE)

source("approx_delimeter.R")
approx_delimeter_ans <- test(dataset$data, get_approx_delimeter(TARGET_TN))
hist_approx <- hist(approx_delimeter_ans, breaks=20, plot = FALSE)

left = min(dataset$delimeter, percentile_delimeter_ans,
           approx_delimeter_ans, gpd_delimeter_ans)
right = max(dataset$delimeter, percentile_delimeter_ans,
            approx_delimeter_ans, gpd_delimeter_ans)
par(mfrow = c(2, 2))
plot(hist_percentile, xlim=c(left, right), las = 0, main = "percentile")
abline(v=mean(percentile_delimeter_ans), col="blue", lwd=2)
abline(v=dataset$delimeter, col="green", lwd=3)
plot(hist_gpd,        xlim=c(left, right), las = 1, main = "gpd")
abline(v=mean(gpd_delimeter_ans), col="blue", lwd=2)
abline(v=dataset$delimeter, col="green", lwd=3)
plot(hist_approx,     xlim=c(left, right), las = 2, main = "approx")
abline(v=mean(approx_delimeter_ans), col="blue", lwd=2)
abline(v=dataset$delimeter, col="green", lwd=3)
plot(hist_approx,     xlim=c(left, right), las = 3, main = "gpd_new")
abline(v=mean(approx_delimeter_ans), col="blue", lwd=2)
abline(v=dataset$delimeter, col="green", lwd=3)

print(dataset$delimeter)
print(c("percentile mse = ", mean((percentile_delimeter_ans - dataset$delimeter)**2)))
print(c("gpd mse = ", mean((gpd_delimeter_ans - dataset$delimeter)**2)))
print(c("approx mse = ", mean((approx_delimeter_ans - dataset$delimeter)**2)))
print(c("gpd_new mse = ", mean((approx_delimeter_ans - dataset$delimeter)**2)))
      