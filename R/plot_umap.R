
#' plot a UMAP scatter plot
#'
#' @param d data frame with data
#' @param xy character, two columns in d with coordinates
#' @param xlab character, label for x axis
#' @param ylab character, label for y axis
#' @param main character, label for top of the plot
#' @param xlim numeric, range for x axis
#' @param ylim numeric, range for y axis
#' @param axis_legend_size numeric of length 2, length of arrows for axis legends
#' @param Rcssclass character, style class
#'
plot_umap <- function(d, xy=c("UMAP_1", "UMAP_2"), point_class_col=NA,
                      xlim=NULL, ylim=NULL,
                      xlab=gsub("_", " ", xy[1]), ylab=gsub("_", " ", xy[2]),
                      main="",
                      axis_legend_size=c(0.2, 0.3),
                      Rcssclass=NULL) {

  RcssCompulsoryClass <- RcssGetCompulsoryClass(c("umap", Rcssclass))
  x_var <- xy[1]
  y_var <- xy[2]
  if (is.null(xlim)) {
    xlim <- range(d[[x_var]])
  }
  if (is.null(ylim)) {
    ylim <- range(d[[y_var]])
  }
  w <- xlim[2]-xlim[1]
  h <- ylim[2]-ylim[1]

  # draw core plot - axis and guide lines
  parplot(xlim, ylim, xlim=xlim, ylim=ylim)
  if (is.na(point_class_col)) {
    points(d[[x_var]], d[[y_var]])
  } else {
    parts <- split(d, d[[point_class_col]])
    for (p in sort(names(parts))) {
      points(parts[[p]][[x_var]], parts[[p]][[y_var]], Rcssclass=p)
    }
  }

  if (!identical(axis_legend_size, NA)) {

    Arrows(xlim[1], ylim[1], xlim[1]+axis_legend_size[1]*w, ylim[1], xpd=1,
           col=RcssValue("Arrow", "col", default="#000000"),
           lwd=RcssValue("Arrow", "lwd", default=1),
           arr.length=RcssValue("Arrow", "arr.length", default=0.2),
           arr.adj=RcssValue("Arrow", "arr.adj", default=1))
    text(xlim[1]+axis_legend_size[1]*w/2, ylim[1], xlab, Rcssclass="xlab")
    Arrows(xlim[1], ylim[1], xlim[1], ylim[1]+axis_legend_size[2]*h, xpd=1,
           col=RcssValue("Arrow", "col", default="#000000"),
           lwd=RcssValue("Arrow", "lwd", default=1),
           arr.length=RcssValue("Arrow", "arr.length", default=0.2),
           arr.adj=RcssValue("Arrow", "arr.adj", default=1))
    text(xlim[1], ylim[1]+axis_legend_size[2]*h/2, ylab, Rcssclass="ylab")
  }
  mtext(main, side=3, Rcssclass="main")
}

