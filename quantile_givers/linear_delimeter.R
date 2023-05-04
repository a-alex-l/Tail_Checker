library(lqmix)

get_delimeter <- function(percentile) {
  delimeter_by_data <- function(data) {
    n <- length(data)
    m <- as.integer(length(data) * 0.9)
    ones <- c(1, 1, 1, 1, 1, 1, 1, 1 ,1 ,1)
    probs <- seq(from=m / (1 + n), to=n / (1 + n), length.out=n - m)
    probs2 <- probs * probs
    probs4 <- probs2 * probs2
    probs8 <- probs4 * probs4
    input <- list('data'=unlist(tail(sort(data), n - m)),
                  'x0'=ones, 'x1'=probs, 'x2'=probs2, 'x4'=probs4, 'x8'=probs8)
    relation <- lm(data~x1 + x2, input)
    delimeter <- predict(relation, list('x0'=1, 'x1'=percentile, 'x2'=percentile**2,
                                        'x4'=percentile**4, 'x8'=percentile**8))
    return(unlist(delimeter[1]))
  }
}