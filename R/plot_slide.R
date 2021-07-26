
#' plot a slide graph (horizontal lines with points sliding along the lines)
#'
#' @param d data table, should have column $css used as an Rcssclass, $label
#' used as label for the line, and columns that match argument 'states'
#' @param states character, named vector defining the lines on the slide graph
#' @param state_col character, column in d
#' @param ylim numeric vector, limits for y axis
#' @param xlab character, label for x axis
#' @param ylab character, label for y axis
#' @param main character, label for top of the plot
#' @param Rcssclass character, style class
#'
plot_slide <- function(d, states, state_col="state", value_col="value",
                       xlim=c(0, 100), xlab="Hit rate (%)", ylab="",
                       main="",
                       Rcssclass=NULL) {

  RcssCompulsoryClass <- RcssGetCompulsoryClass(c("slide", Rcssclass))

  if (is.null(names(states))) {
    names(states) <- states
  }

  ylim <- c(-length(states)-0.5, -0.5)

  # draw core plot - axis and guide lines
  parplot(xlim, ylim, xlim=xlim, ylim=ylim)
  for (j in seq_along(states)) {
    lines(xlim, rep(-j, 2), Rcssclass="state")
    axis(2, at=-j, label=gsub("_", " ", states[j]), Rcssclass="y")
  }
  axis(1, label=NA, line=0, lwd=0, Rcssclass="x")
  axis(1, lwd=0, lwd.ticks=0, Rcssclass="x")
  mtext(ylab, side=2, Rcssclass="ylab")
  mtext(xlab, side=1, Rcssclass="xlab")
  mtext(main, side=3, Rcssclass="main")

  # draw one row at a time
  for (j in seq_along(states)) {
    j_d <- d[d[[state_col]]==names(states)[j], ]
    for (i in seq_len(nrow(j_d))) {
      points(j_d[[value_col]][i], -j, Rcssclass=j_d$css[i])
    }
  }

}



#' draw one point manually on a slide graph
#'
#' @param point_coords numeric of length 2, coordinates for point
#' @param label_coords numeric of length 2, coordinates for label
#' @param label character, label
#' @param Rcssclass character, style class
add_slide_legend_point <- function(point_coords, label_coords, label, Rcssclass) {
  RcssCompulsoryClass <- RcssGetCompulsoryClass(c("slide", Rcssclass, "legend"))
  points(point_coords[1], point_coords[2])
  text(label_coords[1], label_coords[2], label)
}