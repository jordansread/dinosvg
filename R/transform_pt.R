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


svg_coords <- function(vals, val.lims, svg.lims, log=FALSE, return.out=TRUE){
  stopifnot(!log)
  
  val.rng <- diff(val.lims)
  svg.rng <- diff(svg.lims)
  px.rat <- svg.rng/val.rng # ratio of px per plot unit val
  
  px.vals <- svg.lims[1] + (vals-val.lims[1])*px.rat
  if (!return.out){
    px.vals <- px.vals[px.vals >= min(svg.lims) & px.vals <= max(svg.lims)]
  }
  return(px.vals)
}