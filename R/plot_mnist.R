
#' draw a plot that looks like a table
#'
#' @param v vector of pixels
#' @param coords numeric vector of length 4 with rect coordinates
#' @param border logical, draw a rectangle around the image
#' @param label character, a label printed below the image
#' @param Rcssclass character, style class
#'
add_mnist <- function(v, coords, border=TRUE, label="", Rcssclass=NULL) {
  RcssCompulsoryClass <- RcssGetCompulsoryClass(c("mnist", Rcssclass))
  if (is(v, "data.frame")) {
    v <- as.data.frame(v)
    v <- as.numeric(v[, grep("pixel", colnames(v))])
  }

  mnist_col <- RcssValue("mnist", "col", default=c("#000000", "#ffffff"))
  mnist_masked <- RcssValue("mnist", "masked", default="#888888")
  mnist_ramp <- colorRamp(mnist_col)

  v_not_na <- !is.na(v)
  vc <- rep(mnist_masked, length(v))
  vc[v_not_na] <- rgb(mnist_ramp(v[v_not_na]/255)/255)
  vr <- as.raster(matrix(vc, byrow=TRUE, nrow=sqrt(length(v))))
  rasterImage(vr, coords[1], coords[2], coords[3], coords[4], xpd=1)
  if (border) {
    rect(coords[1], coords[2], coords[3], coords[4])
  }
  text((coords[1]+coords[3])/2, coords[4], label)
}

