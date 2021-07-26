# function operating on vectors of pixels (for mnist)

#' perform a smoothing/convolution of an image vector
#'
#' @param x vector with image data
#'
#' @return vector with smoothed values
smooth_pixels <- function(x, min_threshold=80) {
  n <- sqrt(length(x))
  raw <- matrix(as.numeric(x), nrow=n, ncol=n)
  left <- cbind(raw[, -1], raw[, n])
  right <- cbind(raw[,1], raw[, -n])
  top <- rbind(raw[-1,], raw[n, ])
  bottom <- rbind(raw[1,], raw[-n, ])
  result <- rbind(as.numeric(raw),
                  as.numeric(left),
                  as.numeric(right),
                  as.numeric(top),
                  as.numeric(bottom))
  result <- round(apply(result, 2, mean, na.rm=TRUE))
  result[result<min_threshold] <- 0
  pmax(as.numeric(x), as.numeric(result), na.rm=TRUE)
}



impute_missing_pixels <- function(x) {
  .missing <- is.na(x)
  result <- x
  result[.missing] <- smooth_pixels(x)[.missing]
  result
}
