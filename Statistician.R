library(foreach)
library(doFuture)
library(glue)
registerDoFuture()
plan(multisession, workers=6)
options(scipen=999)

TARGET_TN <- 0.999
SEED <- 8
DATA_SIZE <- 50000
MIN_WORK_SIZE <- 100
set.seed(SEED)

test_alg_on_data <- function(data, fixer, algorithm) {
  quantiles <- foreach (i = MIN_WORK_SIZE:DATA_SIZE, .combine=c) %dopar% {
    algorithm(fixer(as.numeric(tail(head(data, i), MIN_WORK_SIZE))))
  }
  return(quantiles)
}

quantile_givers = list(
  list("source" = "quantile_givers/max_delimeter.R", "name" = "max"),
#  list("source" = "quantile_givers/simple_delimeter.R", "name" = "simple_q"),
  list("source" = "quantile_givers/linear_delimeter.R", "name" = "linear_q"),
#  list("source" = "quantile_givers/gpd_evir_delimeter.R", "name" = "gpd_evir"),
  list("source" = "quantile_givers/gpd_gmle_delimeter.R", "name" = "gpd_gmle"),
#  list("source" = "quantile_givers/gpd_delimeter.R", "name" = "gpd_stnd"),
  list("source" = "quantile_givers/my_gpd_delimeter.R", "name" = "gpd_mine"),
  list("source" = "quantile_givers/new_delimeter.R", "name" = "gpd__new"),
  list("source" = "quantile_givers/gpd_Lmoments_delimeter.R", "name" = "gpd_lmom")
)

tests <- list(
  list("source" = "tests/LnormTest.R", "name" = "lnorm"),
  list("source" = "tests/StudTest.R", "name" = "stud"),
  list("source" = "tests/NormTest.R", "name" = "norm"),
  list("source" = "tests/CauchyTest.R", "name" = "cauchy")
)

fixers <- list(
  # list("source" = "distrib_fixers/block_fixer.R", "name" = "block"),
  list("source" = "distrib_fixers/nothing_fixer.R", "name" = "")
)

for (i in 1:length(tests)) {
  test_enviroment <- new.env()
  source(tests[[i]]$source, local=test_enviroment)
  dataset <- test_enviroment$get_data(DATA_SIZE)
  par(mfrow=c(length(fixers) * 2, length(quantile_givers) / 2))
  for (f in 1:length(fixers)) {
    fixer_enviroment <- new.env()
    source(fixers[[f]]$source, local=fixer_enviroment)
    for (j in 1:length(quantile_givers)) {
      giver_enviroment <- new.env()
      source(quantile_givers[[j]]$source, local=giver_enviroment)
      quantiles <- test_alg_on_data(dataset$data, fixer_enviroment$distrib_fixer, giver_enviroment$get_delimeter(TARGET_TN))
      for (ind in 2:length(quantiles)) {
        quantiles[ind] <- quantiles[ind] * 0.01 + quantiles[ind - 1] * 0.99
      }
      percent <- as.numeric(test_enviroment$get_positions(quantiles, dataset$params, DATA_SIZE - MIN_WORK_SIZE))
      left <- min(min(percent), TARGET_TN)
      right <- max(max(percent), TARGET_TN)
      hist(percent, breaks=50, 
           xlim=c(left, right),
           main=paste(tests[[i]]$name, fixers[[f]]$name, quantile_givers[[j]]$name, '=', signif(mean(percent), digits=5 )))
      abline(v=TARGET_TN, col="green", lwd=4)
      abline(v=mean(percent), col="blue", lwd=2)
      print(paste(tests[[i]]$name, fixers[[f]]$name, quantile_givers[[j]]$name,
                  "equals to", signif(abs(TARGET_TN - exp(mean(log(percent)))), digits=3),
                  "mean is", signif(exp(mean(log(percent))), digits=5),
                  "std(var) is", signif(sd(percent), digits=5)))
    }
  }
}