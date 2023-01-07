library(extremeStat)

get_approx_delimeter <- function(percentile) {
  approx_delimeter <- function(data) {
    delimeters <- distLquantile(x=data, probs=c(percentile),
                                list=TRUE, quiet=TRUE, gpquiet=TRUE)
    # print(delimeters$quant)
    return(delimeters$quant[1, 1])
  }
}