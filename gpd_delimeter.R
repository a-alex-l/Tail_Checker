library(extremeStat)

get_gpd_delimeter <- function(percentile) {
  gpd_delimeter <- function(data) {
    delimeters <- distLquantile(x=data, probs=c(percentile), selection="gpa",
                                list=TRUE, quiet=TRUE, gpquiet=TRUE)
    # print(delimeters$quant)
    return(delimeters$quant[1, 1])
  }
}