render_grid <- function(g.view, nx = NULL, ny = nx, col = "lightgray", lty = "dotted", lwd = par("lwd"), equilogs = TRUE, xlim, ylim, ...){
  args <- expand.grid(..., stringsAsFactors = FALSE)
  view.bounds <- view_bounds(g.view)
  clip.id <- svg_id(g_mask(g.view))
  g.geom <- svg_node('g', g.view, c('stroke'=as.rgb(col), as.lty(lty), 'clip-path'=sprintf("url(#%s)",clip.id), args))
  
  # x
  if (is.null(nx)){
    # find ticks in x, use them. g.view
    #g.view
  } else if (!is.na(nx) && nx > 0){
    message('these arguments are not currently supported for render_grid')
  } # else do nothing (NA or 0)
  
  # y
  if (is.null(ny)){
    tick_location(g.view, 'y')
  } else if (!is.na(ny) && ny > 0){
    message('these arguments are not currently supported for render_grid')
  }
  
}