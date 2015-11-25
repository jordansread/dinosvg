render_abline <- function(g.view, a = NULL, b = NULL, h = NULL, v = NULL, reg = NULL, coef = NULL, untf = FALSE, 
                          lty=par("lty"), col=par("col"), lwd=par("lwd"), xlim, ylim, ...){
  
  args <- expand.grid(..., stringsAsFactors = FALSE)
  view.bounds <- view_bounds(g.view)
  clip.id <- svg_id(g_mask(g.view))
  g.geom <- svg_node('g', g.view, c('stroke'=as.rgb(col), as.lty(lty), 'clip-path'=sprintf("url(#%s)",clip.id), args))
  
  if (!is.null(v)){
    y1 <- view.bounds[['y']] + view.bounds[['height']]
    y2 <- view.bounds[['y']]
    
    x <- svg_coords(x=v,xlim=xlim, view.bounds = view.bounds)$x
    for (i in seq_len(length(x))){
      svg_node('line', g.geom, c(x1=x[i], y1=y1, x2=x[i], y2=y2))
    }
  } else if (!is.null(v)){
    x1 <- view.bounds[['x']]
    x2 <- view.bounds[['x']] + view.bounds[['width']]
    
    y <- svg_coords(y=h,ylim=ylim, view.bounds = view.bounds)$y
    for (i in seq_len(length(y))){
      svg_node('line', g.geom, c(x1=x1, y1=y[i], x2=x2, y2=y[i]))
    }
  } else {
    message('these arguments are not currently supported for render_abline')
  }
  
  
}
