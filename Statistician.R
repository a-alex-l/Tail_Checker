library(foreach)
library(doFuture)
library(glue)
registerDoFuture()
plan(multisession, workers=5)
options(scipen=999)

TARGET_TN <- 0.999
SEED <- 9 # 34
DATA_SIZE <- 1000
MIN_WORK_SIZE <- 100
set.seed(SEED)

test_alg_on_data <- function(data, algorithm) {
  quantiles <- foreach (i = MIN_WORK_SIZE:DATA_SIZE, .combine=c) %dopar% {
    algorithm(as.numeric(tail(head(data, i), MIN_WORK_SIZE)))
  }
  return(quantiles)
}

quantile_givers = list(
  list("source" = "quantile_givers/gpd_evir_delimeter.R", "name" = "gpd_evir"),
  list("source" = "quantile_givers/gpd_gmle_delimeter.R", "name" = "gpd_gmle"),
  list("source" = "quantile_givers/gpd_delimeter.R", "name" = "gpd_stnd"),
  list("source" = "quantile_givers/my_gpd_delimeter.R", "name" = "gpd_mine"),
  list("source" = "quantile_givers/gpd_Lmoments_delimeter.R", "name" = "gpd_lmom"),
  list("source" = "quantile_givers/new_delimeter.R", "name" = "gpd__new")
)

tests <- list(
  list("source" = "tests/LnormTest.R", "name" = "lnorm"),
  list("source" = "tests/NormTest.R", "name" = "norm"),
  list("source" = "tests/CauchyTest.R", "name" = "cauchy")
)

percents <- list()
for (i in 1:length(tests)) {
  test_enviroment <- new.env()
  source(tests[[i]]$source, local=test_enviroment)
  dataset <- test_enviroment$get_data(DATA_SIZE)
  print("created dataset")
  for (j in 1:length(quantile_givers)) {
    giver_enviroment <- new.env()
    source(quantile_givers[[j]]$source, local=giver_enviroment)
    quantiles <- test_alg_on_data(dataset$data, giver_enviroment$get_delimeter(TARGET_TN))
    percents[[1 + (i - 1) * length(quantile_givers) + (j - 1)]] <- test_enviroment$get_positions(quantiles, dataset$params, DATA_SIZE - MIN_WORK_SIZE)
    print("found all quantiles")
  }
  par(mfrow=c(2, length(quantile_givers) / 2))
  for (j in 1:length(quantile_givers)) {
    show_data <- as.numeric(percents[[1 + (i - 1) * length(quantile_givers) + (j - 1)]])
    left <- min(min(show_data), TARGET_TN)
    right <- max(max(show_data), TARGET_TN)
    hist(show_data, breaks=20, 
         xlim=c(left, right),
         main=paste(tests[[i]]$name, quantile_givers[[j]]$name))
    abline(v=TARGET_TN, col="green", lwd=4)
    abline(v=mean(show_data), col="blue", lwd=2)
    print(paste(tests[[i]]$name, quantile_givers[[j]]$name,
                "equals to", signif(abs(TARGET_TN - mean(show_data)), digits = 3),
                "mean is", signif(mean(show_data), digits = 5)))
  }
  print("added chart")
}