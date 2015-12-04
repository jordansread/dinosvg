
svg_coords <- function(x, y=NULL, xlim, ylim=NULL, view.bounds, log='', as.crd=TRUE){
  coords <- list()
  if (!missing(x) && !is.null(x))
    coords$x <- dim_coords(x, xlim, c(view.bounds[['x']],view.bounds[['x']] + view.bounds[['width']]), as.crd=as.crd)
  if (!is.null(y))
    coords$y <- dim_coords(y, ylim, c(view.bounds[['y']] + view.bounds[['height']], view.bounds[['y']]), as.crd=as.crd)
  return(coords)
}

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