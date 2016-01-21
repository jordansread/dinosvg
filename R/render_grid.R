render_grid <- function(g.view, nx = NULL, ny = nx, col = "lightgray", lty = "dotted", lwd = par("lwd"), equilogs = TRUE, xlim, ylim, ...){
  args <- filter_dot_args(...)
  view.bounds <- view_bounds(g.view)
  clip.id <- svg_id(g_mask(g.view))
  g.geom <- svg_node('g', g.view, c('stroke'=as.rgb(col), as.lty(lty), 'clip-path'=sprintf("url(#%s)",clip.id), g_args(args)))
  
  # x
  if (is.null(nx)){
    x <- tick_location(g.view, 'x')
    for (i in seq_len(length(x))){
      svg_node('path', g.geom, c(d=sprintf('M %s,%s v %s',x[i], view.bounds[['y']], view.bounds[['height']]), nd_args(args)))
    }
  } else if (!is.na(nx) && nx > 0){
    message('these arguments are not currently supported for render_grid')
  } # else do nothing (NA or 0)
  
  # y
  if (is.null(ny)){
    y <- tick_location(g.view, 'y')
    for (i in seq_len(length(y))){
      svg_node('path', g.geom, c(d=sprintf('M %s,%s h %s',view.bounds[['x']], y[i], view.bounds[['width']]), nd_args(args)))
    }
  } else if (!is.na(ny) && ny > 0){
    message('these arguments are not currently supported for render_grid')
  }
  
}