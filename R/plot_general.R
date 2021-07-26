# function multipanelLabel copied/modified from previous projects

#' add a text object into an existing plot (a label for a multi-panel figure)
#'
#' @param label character, text to place in top-left corner
#' @param x numeric, horizontal position where to place the label
#' (in user coordinates)
#' @param y numeric, vertical position where to place the label
#' (in user coordinates)
#'
multipanelLabel <- function(label, x=NULL, y=NULL) {
  nowpar <- graphics::par()
  pw <- nowpar$plt[2]-nowpar$plt[1]
  ph <- nowpar$plt[4]-nowpar$plt[3]
  uw <- nowpar$usr[2]-nowpar$usr[1]
  uh <- nowpar$usr[4]-nowpar$usr[3]
  if (is.null(x)) {
    x <- nowpar$usr[1] - (uw/pw)*nowpar$plt[1]
  }
  if (is.null(y)) {
    y <- nowpar$usr[3] - (uh/ph)*nowpar$plt[3] + (uh/ph)
  }
  text(x, y, label, xpd=1, Rcssclass="multipanel")
}

