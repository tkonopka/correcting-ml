
#' plot a change graph (dots at various states, interconnected by lines)
#'
#' @param d data table, should have column $css used as an Rcssclass, $label
#' used as label for the line, and columns that match argument 'states'
#' @param states character, vector linking columns in d used at x ticks;
#' a named vector can be used to map column names in d to labels on the chart
#' @param ylim numeric vector, limits for y axis
#' @param ylab character, label for y axis
#' @param main character, label for top of the plot
#' @param show_y logical, toggles visibility of y axis
#' @param show_label logical, toggles visibility of labels
#' @param Rcssclass character, style class
#'
plot_performance <- function(d, states,
                             ylim=c(0, 1), ylab="Hit rate",
                             main="",
                             show_y=TRUE, show_label=TRUE,
                             Rcssclass=NULL) {

  n_states <- length(states)
  stopifnot(n_states >= 2)

  RcssCompulsoryClass <- RcssGetCompulsoryClass(c("perf", Rcssclass))
  label_q <- RcssValue("perf", "label.q", default=c(0.1, 1.0))
  label_length <- RcssValue("perf", "label.length", default=0.2)
  label_offset <- RcssValue("perf", "label.offset", default=0.1)
  state_pad <- RcssValue("perf", "state.pad", default=0.02)

  if (is.null(names(states))) {
    names(states) <- states
  }

  last_state <- tail(names(states), 1)
  label_y <- cbind(d[[last_state]], d[[last_state]])
  if ("label_y" %in% colnames(d)) {
    label_y[,2] <- label_y[,2] + d$label_y
  }

  # draw core plot - axis and guide lines
  parplot(c(0.5, n_states+0.5), ylim,
          xlim=c(0.5, n_states+0.5), ylim=ylim)
  for (j in seq_along(states)) {
    lines(rep(j, 2), ylim, Rcssclass="state")
    text(j, ylim[1]-state_pad*(ylim[2]-ylim[1]), states[j], Rcssclass="state")
  }
  if (show_y) {
    axis(2, label=NA, line=0, lwd=0, Rcssclass="y")
    axis(2, lwd=0,lwd.ticks=0, Rcssclass="y")
    mtext(ylab, side=2, Rcssclass="ylab")
  }
  mtext(main, side=3, Rcssclass="main")

  # draw one row at a time
  for (i in seq_len(nrow(d))) {
    i_css <- d$css[i]
    i_values <- rep(NA, n_states)
    for (j in seq_along(states)) {
      j_state <- names(states)[j]
      i_values[j] <- d[[j_state]][i]
    }
    # draw line segments and points
    for (j in seq(2, n_states)) {
      j_segment <- i_values[(j-1):j]
      if (all(!is.na(j_segment))) {
        lines(c(j-1, j), j_segment, Rcssclass=c("segment", i_css))
      }
    }
    # draw row labels
    if (show_label) {
      if (label_y[i,1]!= label_y[i,2]) {
        label_segment_y <- rep(label_y[i,], c(2, 1))
        lines(quantile(n_states+c(0, label_length), p=c(0, label_q)),
              label_segment_y, Rcssclass=c("label", i_css))
      }
      text(n_states + label_length + label_offset,
           label_y[i, 2], d$label[i], Rcssclass=c("label", i_css))
    }
    # draw the points
    for (j in seq_along(states)) {
      if (!is.na(i_values[j])) {
        points(j, i_values[j], Rcssclass=i_css)
      }
    }
  }

}

