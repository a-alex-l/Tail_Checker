distrib_fixer <- function(data) {
  block_size <- ceiling(length(data)**0.5)
  block_count <- ceiling(length(data) / block_size)
  target_mean <- median(tail(data, block_size))
  ans <- data
  for (i in 2:block_count) {
    l <- i * block_size
    r <- min((i + 1) * block_size, length(data))
    current_mean <- median(data[l:r])
    ans[l:r] <- data[l:r] * target_mean / current_mean
  }
  return(ans)
}