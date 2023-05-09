library(foreach)
library(doFuture)
library(doRNG)
library(glue)
registerDoFuture()
plan(multisession, workers=20)
options(scipen=999)

TARGET_TN <- 0.999
SEED <- 17
DATA_SIZE <- 50000
TEST_COUNT <- 10000
MIN_WORK_SIZE <- 100
set.seed(SEED)

test_alg_on_data <- function(data, fixer, algorithm) {
  # for (i in 1:TEST_COUNT) {
  #  algorithm(sample(as.numeric(data), MIN_WORK_SIZE))
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
  #  list("source" = "quantile_givers/gpd_Lmoments_delimiter.R", "name" = "gpd_lmom"),
  #  list("source" = "quantile_givers/gpd_gmle_delimiter.R", "name" = "gpd_gmle"),
  #  list("source" = "quantile_givers/mean_gpd_delimiter.R", "name" = "gpd_mean"),
  #  list("source" = "quantile_givers/mean_hulf_gpd_delimiter.R", "name" = "gpd_half_mean"),
  #  list("source" = "quantile_givers/new_gpd_delimiter.R", "name" = "gpd_w_mean"),
  #  list("source" = "quantile_givers/median_gpd_delimiter.R", "name" = "gpd_median"),
  #  list("source" = "quantile_givers/pick_gpd_delimiter.R", "name" = "gpd_pick"),
  #  list("source" = "quantile_givers/max_gpd_delimiter.R", "name" = "gpd_max"),
  list("source" = "quantile_givers/fix_l_gpd_delimiter.R", "name" = "gpd_f_lmom"),
  list("source" = "quantile_givers/fix_g_gpd_delimiter.R", "name" = "gpd_f_gmle"),
  list("source" = "quantile_givers/fix_m_gpd_delimiter.R", "name" = "gpd_f_mean"),
  list("source" = "quantile_givers/fix_hm_gpd_delimiter.R", "name" = "gpd_f_half_mean"),
  list("source" = "quantile_givers/fix_w_gpd_delimiter.R", "name" = "gpd_f_w_mean"),
  list("source" = "quantile_givers/fix_md_gpd_delimiter.R", "name" = "gpd_f_median"),
  list("source" = "quantile_givers/fix_p_gpd_delimiter.R", "name" = "gpd_f_pick")
)

tests <- list(
  list("source" = "tests/LnormTest.R", "name" = "lnorm"),
  list("source" = "tests/PingTest.R", "name" = "ping"),
  list("source" = "tests/FileTest.R", "name" = "file"),
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
  print('dataset creating')
  dataset <- test_enviroment$get_data(DATA_SIZE)
  print('dataset done')
  par(mfrow=c(length(fixers) * 2, length(quantile_givers) / 2))
  for (j in 1:length(quantile_givers)) {
    giver_enviroment <- new.env()
    source(quantile_givers[[j]]$source, local=giver_enviroment)
    quantiles <- test_alg_on_data(dataset$data, fixer_enviroment$distrib_fixer, giver_enviroment$get_delimeter(TARGET_TN))
    print('quantiles found')
    percent <- as.numeric(test_enviroment$get_positions(quantiles, dataset$params, TEST_COUNT))
    print('percents found')
    left <- min(min(percent), TARGET_TN)
    right <- max(max(percent), TARGET_TN)
    # hist(percent, breaks=50, 
    #      xlim=c(left, right),
    #      main=paste(tests[[i]]$name, quantile_givers[[j]]$name, '=', signif(mean(percent), digits=5 )))
    # abline(v=TARGET_TN, col="green", lwd=4)
    # abline(v=mean(percent), col="blue", lwd=2)
    print(paste(tests[[i]]$name, quantile_givers[[j]]$name,
                "equals to", signif(abs(TARGET_TN - mean(percent)), digits=3),
                "mean is", signif(mean(percent), digits=5),
                "sd(var) is", signif(sd(percent), digits=5)))
    new_percents <- as.numeric(test_enviroment$get_positions(mean(quantiles), dataset$params, 1))
    print(paste("fixed mean of quantiles", signif(new_percents, digits=5)))
    
    write(paste(tests[[i]]$name, quantile_givers[[j]]$name,
                  "equals to", signif(abs(TARGET_TN - mean(percent)), digits=3),
                  "mean is", signif(mean(percent), digits=5),
                  "sd(var) is", signif(sd(percent), digits=5),
                  "quantile mean is", signif(mean(quantiles[100:9900]), digits=5),
                  "fixed mean of quantiles", signif(new_percents, digits=5)),
          file="output.txt",
          append=TRUE)
  }
}