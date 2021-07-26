
#' create a new plot with a single text label
#'
#' @param label character, text to place in top-left corner
#' @param x numeric, horizontal position where to place the label
#' @param y numeric, vertical position where to place the label
#'
plot_label <- function(label, x=0.5, y=0.5, Rcssclass=c()) {
  RcssCompulsoryClass <- RcssGetCompulsoryClass(c("label", Rcssclass))
  parplot(0,0, xlim=c(0, 1), ylim=c(0, 1))
  text(x, y, label)
}

