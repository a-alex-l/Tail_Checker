library(extremeStat)

get_delimeter <- function(percentile) {
  delimeter_by_data <- function(data) {
    #delimeters <- distLquantile(x=data, probs=c(percentile), selection="gpa",
    #                            list=TRUE, quiet=TRUE, gpquiet=TRUE)
    #print(delimeters$quant)
    #return(delimeters$quant[1, 1])
    delimeter <- q_gpd(unlist(data), package="fExtremes", truncate=0.8, probs=percentile)
    return(unlist(delimeter[1]))
  }
}