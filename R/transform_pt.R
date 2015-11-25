tran_x <- function(val, axes, fig){
  
  plt_rng <- diff(axes$x_lim)
  px_rng = diff(fig$px_lim$x)
  px_rat <- px_rng/plt_rng # ratio of px per plot unit val
  
  x_px <- fig$px_lim$x[1]+(val-axes$x_lim[1])*px_rat
  
  # gets value and bounds of x, returns "pixel-space" value for x

  return(x_px)
  
}
tran_y <- function(val, axes, fig){

  plt_rng <- diff(axes$y_lim)
  px_rng = diff(rev(fig$px_lim$y))
  px_rat <- px_rng/plt_rng # ratio of px per log unit val
  
  y_px <- fig$px_lim$y[1]-(val-axes$y_lim[1])*px_rat
  
  # gets value and bounds of x, returns "pixel-space" value for x
  
  return(y_px)
  
}

svg_coords <- function(x, y=NULL, xlim, ylim=NULL, view.bounds, log=''){
  coords <- list(x=c(), y=c())
  coords$x <- dim_coords(x, xlim, c(view.bounds[['x']],view.bounds[['x']] + view.bounds[['width']]))
  if (!is.null(y) & !is.null(ylim))
    coords$y <- dim_coords(y, ylim, c(view.bounds[['y']] + view.bounds[['height']], view.bounds[['y']]))
  return(coords)
}

dim_coords <- function(vals, val.lims, svg.lims, log=FALSE){
  stopifnot(!log)
  
  val.rng <- diff(as.numeric(val.lims))
  svg.rng <- diff(svg.lims)
  px.rat <- svg.rng/val.rng # ratio of px per plot unit val
  
  px.vals <- svg.lims[1] + (as.numeric(vals)-as.numeric(val.lims[1]))*px.rat

  return(as.crd(px.vals))
}