render_abline <- function(g.view, a = NULL, b = NULL, h = NULL, v = NULL, reg = NULL, coef = NULL, untf = FALSE, 
                          lty=par("lty"), col=par("col"), lwd=par("lwd"), xlim, ylim, ...){
  
  args <- expand.grid(..., stringsAsFactors = FALSE)
  view.bounds <- view_bounds(g.view)
  clip.id <- svg_id(g_mask(g.view))
  g.geom <- svg_node('g', g.view, c('stroke'=col, 'fill'="none", 'clip-path'=sprintf("url(#%s)",clip.id), args))
  
  if (!is.null(v)){
    y1 <- view.bounds[['y']] + view.bounds[['height']]
    y2 <- view.bounds[['y']]
    
    x <- dim_coords(v, xlim, c(view.bounds[['x']],view.bounds[['x']] + view.bounds[['width']]))
    for (i in seq_len(length(x))){
      svg_node('line', g.geom, c(x1=x[i], y1=y1, x2=x[i], y2=y2))
    }
  } else if (!is.null(v)){
    x1 <- view.bounds[['x']]
    x2 <- view.bounds[['x']] + view.bounds[['width']]
    
    y <- dim_coords(h, ylim, c(view.bounds[['y']] + view.bounds[['height']], view.bounds[['y']]))
    for (i in seq_len(length(y))){
      svg_node('line', g.geom, c(x1=x1, y1=y[i], x2=x2, y2=y[i]))
    }
  } else {
    message('these arguments are not currently supported for render_abline')
  }
  
  
}