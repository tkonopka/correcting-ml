
#' plot a horizontal stacked bar plot
#'
#' @param d data frame with data
#' @param series_col character, column in d with factors
#' @param value_col character vector, columns in d with values
#' @param normalize logical, set TRUE to normalize to percentages
#' @param xlab character, label for x axis
#' @param ylab character, label for y axis
#' @param main character, label for top of the plot
#' @param xlim numeric, range for x axis
#' @param ylim numeric, range for y axis
#' @param box_labels named character vector, used to create labels on top of boxes
#' @param min_box_width numeric, boxes with smaller width will not be labeled
#' @param col named vector with colors, used to color boxes
#' @param Rcssclass character, style class
#'
plot_stacks <- function(d, series_col="series", value_col=NULL, normalize=TRUE,
                        xlab="Proportion (%)", ylab="", main="",
                        box_labels=NA, min_box_width=3, box_col=NA, label_col=NA,
                        Rcssclass=NULL) {

  RcssCompulsoryClass <- RcssGetCompulsoryClass(c("stacks", Rcssclass))

  # prepare data into a matrix form
  dm <- matrix(0, ncol=length(value_col), nrow=nrow(d),
               dimnames=list(d[[series_col]], value_col))
  for (i in seq_along(value_col)) {
    dm[,i] <- d[[value_col[i]]]
  }
  if (normalize) {
    dm <- 100 * dm / apply(dm, 1, sum)
  }
  xlim <- c(0, max(apply(dm, 1, sum)))
  ylim <- c(-nrow(d), 0)
  y.mid <- -seq(1, nrow(d)) + 0.5

  bw <- RcssValue("barplot", "width", default=0.8)
  bw2 <- bw/2

  parplot(xlim, ylim, xlim=xlim, ylim=ylim)
  for (j in seq_len(nrow(dm))) {
    axis(2, at=-j+0.5, label=rownames(dm)[j], lwd=0, Rcssclass="y")
  }

  x <- rep(0, nrow(dm))
  for (i in seq_along(value_col)) {
    i.width <- dm[, i]
    i.mid <- x + (i.width/2)
    i.class <- value_col[i]
    i.col <- box_col[i.class]
    if (is.na(i.col)) {
      rect(x, y.mid-bw2, x+i.width, y.mid+bw2, Rcssclass=i.class)
    } else {
      rect(x, y.mid-bw2, x+i.width, y.mid+bw2, col=i.col, Rcssclass=i.class)
    }
    if (!identical(box_labels, NA)) {
      i.col <- label_col[i.class]
      box_data <- cbind(width=i.width, x=i.mid, y=y.mid, label=box_labels[i.class])
      box_data <- box_data[i.width > min_box_width, ]
      if (is.na(i.col)) {
        text(box_data[, "x"], box_data[, "y"], box_data[, "label"], Rcssclass="label")
      } else {
        text(box_data[, "x"], box_data[, "y"], box_data[, "label"], col=i.col,
             Rcssclass="label")
      }
    }
    x <- x+ i.width
  }

  axis(1, at=xlim, label=NA, line=0, Rcssclass="x")
  axis(1, line=0, label=NA, Rcssclass="x")
  axis(1, lwd=0, Rcssclass="x")
  mtext(ylab, side=2, Rcssclass="ylab")
  mtext(xlab, side=1, Rcssclass="xlab")
  mtext(main, side=3, Rcssclass="main")
}

