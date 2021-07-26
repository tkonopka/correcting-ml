
#' plot a set of lines (x-y coordinates with interconnecting lines)
#'
#' @param d data frame with data
#' @param series_col character, column in d with factors
#' @param series character vector, all series to plot
#' @param x_col character, column in d with x values
#' @param y_col character, column in d with y vaues
#' @param xlab character, label for x axis
#' @param ylab character, label for y axis
#' @param main character, label for top of the plot
#' @param xlim numeric, range for x axis
#' @param ylim numeric, range for y axis
#' @param show_points logical, toggle visibility of points
#' @param Rcssclass character, style class
#'
plot_series <- function(d, series_col="series", x_var="x", y_var="y",
                        series=NULL,
                        xlab="", ylab="", main="",
                        xlim=NULL, ylim=NULL,
                        show_points=TRUE,
                        Rcssclass=NULL) {

  RcssCompulsoryClass <- RcssGetCompulsoryClass(c("series", Rcssclass))

  if (is.null(xlim)) {
    xlim <- range(d[[x_var]])
  }
  if (is.null(ylim)) {
    ylim <- range(d[[y_var]])
  }
  if (is.null(series)) {
    series <- unique(d[[series_col]])
  }

  # draw core plot - axis and guide lines
  parplot(xlim, ylim, xlim=xlim, ylim=ylim)
  for (.series in series) {
    .d <- d[d[[series_col]]==.series, ]
    .d <- .d[order(.d[[x_var]]),]
    lines(.d[[x_var]], .d[[y_var]], Rcssclass=.series)
    if (show_points) {
      points(.d[[x_var]], .d[[y_var]], Rcssclass=.series)
    }
  }
  axis(1, at=xlim, label=NA, line=0, Rcssclass="x")
  axis(1, line=0, label=NA, Rcssclass="x")
  axis(1, lwd=0, Rcssclass="x")
  axis(2, at=ylim, label=NA, line=0, Rcssclass="y")
  axis(2, line=0, label=NA, Rcssclass="y")
  axis(2, lwd=0, Rcssclass="y")
  mtext(ylab, side=2, Rcssclass="ylab")
  mtext(xlab, side=1, Rcssclass="xlab")
  mtext(main, side=3, Rcssclass="main")
}



#' add a manual legend
#'
#' @param x, y numeric, top-left corner of the legend
#' @param labels character, vector of labels to print on the legend
#' @param main character, title for the legend
#' @param type character, choose between line and box markers
#' @param css character, vector of Rcssclass styles to match the labels
#' @param Rcssclass character, style class
add_series_legend <- function(x, y, labels, main="",
                              type=c("line", "rect"), css=labels,
                              Rcssclass=c()) {
  type <- match.arg(type)
  RcssCompulsoryClass <- RcssGetCompulsoryClass(c("series", Rcssclass))
  legend.ysp <- RcssValue("legend", "y.intersp", default=1)
  legend.xsp <- RcssValue("legend", "x.intersp", default=1)
  legend.seg <- RcssValue("legend", "seg.len", default=c(2, 1.5))
  units <- c(strwidth("M"), 2*strheight("M"))

  text(x, y+0.2*legend.ysp*units[2], main, Rcssclass=c("legend", "main"))
  y_labels <- y - (seq_along(labels) * legend.ysp * units[2])
  text(x + (legend.seg[1]+legend.xsp)*units[1], y_labels, labels,
       Rcssclass=c("legend", "label"))

  for (i in seq_along(css)) {
    .col <- RcssValue("lines", "col", default="#000000", Rcssclass=css[i])
    .lty <- RcssValue("lines", "lty", default=1, Rcssclass=css[i])
    .lwd <- RcssValue("lines", "lwd", default=1, Rcssclass=css[i])
    if (type=="line") {
      lines(c(x, x+legend.seg[1]*units[1]), rep(y_labels[i], 2),
            lty=.lty, col=.col, lwd=.lwd)
    } else {
      h2 <- legend.seg[2]*units[2]/4
      rect(x, y_labels[i] - h2,
           x + (legend.seg[1]*units[1]), y_labels[i] + h2,
           border=NA, col=.col)
    }
  }
}
