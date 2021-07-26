
#' plot a minimal bar plot
#'
#' @param v numeric vector with names
#' @param xlab character, label for x axis
#' @param ylab character, label for y axis
#' @param main character, label for top of the plot
#' @param Rcssclass character, style class
#'
plot_minimal_bars <- function(v, xlab="Hit rate (%)", ylab="", main="",
                              Rcssclass=NULL) {

  RcssCompulsoryClass <- RcssGetCompulsoryClass(c("minbars", Rcssclass))

  w <- RcssValue("barplot", "width", default=0.8)
  w2 <- w/2
  ylim <- c(0, max(v, na.rm=TRUE))
  xlim <- c(0.5, length(v)+0.5)

  # draw core plot - axis and guide lines
  parplot(xlim, ylim, xlim=xlim, ylim=ylim)
  for (i in seq_along(v)) {
    rect(i-w2, 0, i+w2, v[i])
    axis(1, at=i, label=names(v)[i], Rcssclass="x")
  }
  mtext(ylab, side=2, Rcssclass="ylab")
  mtext(xlab, side=1, Rcssclass="xlab")
  mtext(main, side=3, Rcssclass="main")
}

