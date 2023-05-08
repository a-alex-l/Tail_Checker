library(foreach)
library(doFuture)
library(doRNG)
library(glue)
registerDoFuture()
plan(multisession, workers=6)
options(scipen=999)

TARGET_TN <- 0.999
SEED <- 18
DATA_SIZE <- 50000
TEST_COUNT <- 20000
MIN_WORK_SIZE <- 100
set.seed(SEED)

test_alg_on_data <- function(data, fixer, algorithm) {
  # for (i in 1:TEST_COUNT) {
  #   algorithm(sample(as.numeric(data), MIN_WORK_SIZE))
  # }
  quantiles <- foreach (i = 1:TEST_COUNT, .combine=c) %do% {
    algorithm(sample(as.numeric(data), MIN_WORK_SIZE))
  }
  return(quantiles)
}

quantile_givers = list(
#  list("source" = "quantile_givers/max_delimiter.R", "name" = "max"),
#  list("source" = "quantile_givers/simple_delimiter.R", "name" = "simple_q"),
#  list("source" = "quantile_givers/linear_delimiter.R", "name" = "linear_q"),
#  list("source" = "quantile_givers/gpd_evir_delimiter.R", "name" = "gpd_evir"),
  list("source" = "quantile_givers/gpd_Lmoments_delimiter.R", "name" = "gpd_lmom"),
#  list("source" = "quantile_givers/pick_gpd_delimiter.R", "name" = "gpd_pick"),
  list("source" = "quantile_givers/gpd_gmle_delimiter.R", "name" = "gpd_gmle")
#  list("source" = "quantile_givers/fix_l_gpd_delimiter.R", "name" = "gpd_f_lmom"),
#  list("source" = "quantile_givers/my_gpd_delimiter.R", "name" = "gpd_mine"),
#  list("source" = "quantile_givers/new_delimiter.R", "name" = "gpd_new"),
#  list("source" = "quantile_givers/mean_gpd_delimiter.R", "name" = "gpd_mean"),
#  list("source" = "quantile_givers/fix_g_gpd_delimiter.R", "name" = "gpd_f_gmle")
)

tests <- list(
  list("source" = "tests/LnormTest.R", "name" = "lnorm"),
  list("source" = "tests/PingTest.R", "name" = "ping"),
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
  print('dataset created')
  for (j in 1:length(quantile_givers)) {
    giver_enviroment <- new.env()
    source(quantile_givers[[j]]$source, local=giver_enviroment)
    quantiles <- test_alg_on_data(dataset$data, fixer_enviroment$distrib_fixer, giver_enviroment$get_delimeter(TARGET_TN))
    print('quantiles created')
    # for (ind in 2:length(quantiles)) {
    #   quantiles[ind] <- quantiles[ind] * 0.01 + quantiles[ind - 1] * 0.99
    # }
    percent <- as.numeric(test_enviroment$get_positions(c(mean(quantiles), mean(quantiles) - sd(quantiles)), dataset$params, 2))
    print(percent)
    print(mean(quantiles))
    print(sd(quantiles))
  }
}