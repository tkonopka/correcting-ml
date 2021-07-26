
#' plot a set of lines (x-y coordinates with interconnecting lines)
#'
#' @param Rcssclass character, style class
#'
plot_schematic <- function(Rcssclass=NULL) {
  RcssCompulsoryClass <- RcssGetCompulsoryClass(c("schematic", Rcssclass))
  xlim <- c(0, 5)
  ylim <- c(-0.22, 1.1)

  # draw core plot - axis and guide lines
  parplot(xlim, ylim, xlim=xlim, ylim=ylim)

  sectioned_rect(0,0, 1, 0.9, rx=0.1, ry=0.1)
  text(0.5, 1.05, "original dataset", Rcssclass="main")
  text(0.5, 0.675, "train", Rcssclass="train")
  text(0.5, 0.3375, "holdout", Rcssclass="holdout")
  text(0.5, 0.1125, "test", Rcssclass="test")

  Arrows(1.2, 0.45, 1.85, 0.45, xpd=1,
         col=RcssValue("Arrow", "col", default="#000000"),
         lwd=RcssValue("Arrow", "lwd", default=1),
         arr.length=RcssValue("Arrow", "arr.length", default=0.2),
         arr.adj=RcssValue("Arrow", "arr.adj", default=1))

  x0 <- 0.9
  varwidth <- 1.2
  x_offset <- 0.25
  varh2 <- 0.58/2
  y_offset <- 0.1
  for (i in c(1,2)) {
    sectioned_rect(x0 + i*varwidth + x_offset , 0.45-varh2 + y_offset,
                   x0 + i*varwidth + x_offset + 0.6, 0.45+varh2 + y_offset,
                   rx=0.07, ry=0.07, Rcssclass="secondary")
    sectioned_rect(x0 + i*varwidth, 0.45-varh2-y_offset,
                   x0 + i*varwidth + 0.6, 0.45+varh2-y_offset,
                   rx=0.07, ry=0.07, Rcssclass="primary")
    text(x0 + i*varwidth + x_offset/2 + 0.3, 1.05, paste0("variant ", i),
         Rcssclass="main")
  }
  points(seq(4.6, 4.9, length=3), rep(0.45, 3), Rcssclass="ellipsis")

  legend_x <- c(2.1, 2.1, 2.4, 2.4)
  legend_y <- c(-0.07, -0.24, -0.24, -0.07)
  polygon(legend_x, legend_y, Rcssclass=c("outline", "primary"))
  text(max(legend_x)+0.1, mean(legend_y),
       "data for primary model", Rcssclass="label")
  polygon(legend_x, legend_y-0.25, Rcssclass=c("outline", "secondary"))
  text(max(legend_x)+0.1, mean(legend_y)-0.25,
       "data for secondary model", Rcssclass="label")
}


#' create coordinates for a rectangle with clipped corners
#'
#' @param x1 numeric, x coordinate of bottom-left corner
#' @param y1 numeric, y coordinate of botto-left corner
#' @param x2 numeric, x coordinate of top-right corner
#' @param y2 numeric, y coordinate of top-right corner
#' @param rx numeric, radius to clip horizontally
#' @param ry numeric, radius to clip vertically
#' @param corners logical, indicator to clip a corner
#' @param Rcssclass character, style class
clipped_corners <- function(x1, y1, x2, y2, rx=0.2, ry=0.2,
                            corners=rep(TRUE, 4)) {
  xmid <- (x1+x2)/2
  ymid <- (y1+y2)/2
  # bottom-left corner
  xlong <- c(x1, x1, x1+corners[1]*rx, xmid)
  ylong <- c(ymid, y1+corners[1]*ry, y1, y1)
  # bottom-right corner
  xlong <- c(xlong, xmid, x2-corners[2]*rx, x2, x2)
  ylong <- c(ylong, y1, y1, y1+corners[2]*ry, ymid)
  # top-right corner
  xlong <- c(xlong, x2, x2, x2-corners[3]*rx, xmid)
  ylong <- c(ylong, ymid, y2-corners[3]*ry, y2, y2)
  # top-left corner
  xlong <- c(xlong, xmid, x1+corners[4]*rx, x1, x1)
  ylong <- c(ylong, y2, y2, y2-corners[4]*ry, ymid)
  result <- cbind(xlong, ylong)
  unique(result)
}


#' draw a rectangle with three strips
sectioned_rect <- function(x1, y1, x2, y2, rx=0.2, ry=0.2,
                           sections=c(test=0.25, holdout=0.25, train=0.5),
                           Rcssclass=c()) {
  sections <- sections/sum(sections)
  section_top <- y1 + cumsum(sections)*(y2-y1)
  section_bottom <- y1 + c(0, head(cumsum(sections), -1))*(y2-y1)
  # draw three sections
  coords <- clipped_corners(x1, section_bottom[1], x2, section_top[1],
                            rx=rx, ry=ry,
                            corners=c(TRUE, TRUE, FALSE, FALSE))
  polygon(coords[,1], coords[,2], Rcssclass=names(sections)[1])
  coords <- clipped_corners(x1, section_bottom[2], x2, section_top[2],
                            rx=rx, ry=ry,
                            corners=rep(FALSE, 4))
  polygon(coords[,1], coords[,2], Rcssclass=names(sections)[2])
  coords <- clipped_corners(x1, section_bottom[3], x2, section_top[3],
                            rx=rx, ry=ry,
                            corners=c(FALSE, FALSE, TRUE, TRUE))
  polygon(coords[,1], coords[,2], Rcssclass=names(sections)[3])
  # draw the final output
  coords <- clipped_corners(x1, y1, x2, y2,
                            rx=rx, ry=ry,
                            corners=rep(TRUE, 4))
  polygon(coords[,1], coords[,2], Rcssclass=c("outline", Rcssclass))
}
