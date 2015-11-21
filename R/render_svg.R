#' render svg
#' 
#' @examples 
#' \dontrun{
#' #' library(gsplot)
#' gs <- gsplot()
#' gsNew <- points(gs, y=1, x=2, xlim=c(0,3),ylim=c(0,3),
#'             col="blue", pch=18, hovertext(labels='point'))
#' svg(gsNew)
#' }
#' @export
#' @import gsplot
svg <- function(object, ...){
  UseMethod('svg')
}

#' @export
svg.gsplot <- function(object, file = "Rplot.svg", width = 6, height = 4.3, pointsize = 12, ...){
  
  browser()
  par <- par(object)
  svg <- init_svg(width, height, par, pointsize, ...) 
  # get par
  # set the page dimensions
  # translate coordinates
  # build axes
  # do.call for gsplot elements, skip those w/o `svg_` functions and warn
  # invisible return of filename
}

#' points to svg
#' 
#' @param \dots all end up at attributes to the element
svg_points <- function(svg, window, x, y, col, cex=1, ...){
  
  invisible(svg)
}