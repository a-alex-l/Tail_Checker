library(extremeStat)

get_gpd_delimeter <- function(percentile) {
  gpd_delimeter <- function(data) {
    delimeters <- q_gpd(x=data, probs=c(percentile),
                                list=TRUE, quiet=TRUE)
    print(delimeters$quant)
    return(delimeters$quant[1, 1])
  }
}