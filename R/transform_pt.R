
#' svg coordinates from x,y coordinates
#' 
#' translates coordinate reference space into svg space.
#' 
#' @param x location of x coordinate
#' @param y location of y coordinate
#' @param xlim limits of x coordinate reference 
#' @param ylim limits of y coordinate reference
#' @param view.bounds a named vector of \code{x},\code{y},\code{width}, and \code{height}
#' @param log a character of '', 'x', or 'xy' (only '' is currently supported)
#' @param as.crd boolean for passing result to \code{as.crd}
#' 
#' @seealso \code{\link{dim_coords}}
#' @export
svg_coords <- function(x, y=NULL, xlim, ylim=NULL, view.bounds, log='', as.crd=TRUE){
  coords <- list()
  if (!missing(x) && !is.null(x))
    coords$x <- dim_coords(x, xlim, c(view.bounds[['x']],view.bounds[['x']] + view.bounds[['width']]), as.crd=as.crd)
  if (!is.null(y))
    coords$y <- dim_coords(y, ylim, c(view.bounds[['y']] + view.bounds[['height']], view.bounds[['y']]), as.crd=as.crd)
  return(coords)
}

#' svg coordinates that are side agnostic
#' 
#' translates values into svg space
#' 
#' @param vals coordinates
#' @param val.lims coordinate limits
#' @param svg.lims svg view pane limits
#' @param log boolean for logged side (only \code{FALSE} is supported currently)
#' @param as.crd boolean for passing result to \code{as.crd}
#' 
#' @seealso \code{\link{svg_coords}}
#' @keywords internal
#' @export
dim_coords <- function(vals, val.lims, svg.lims, log=FALSE, as.crd=TRUE){
  stopifnot(!log)
  
  val.rng <- diff(as.numeric(val.lims))
  svg.rng <- diff(svg.lims)
  px.rat <- svg.rng/val.rng # ratio of px per plot unit val
  
  px.vals <- svg.lims[1] + (as.numeric(vals)-as.numeric(val.lims[1]))*px.rat

  if (as.crd)
    as.crd(px.vals)
  else
    px.vals
}