render_abline <- function(g.view, a = NULL, b = NULL, h = NULL, v = NULL, reg = NULL, coef = NULL, untf = FALSE, 
                          lty=par("lty"), col=par("col"), lwd=par("lwd"), xlim, ylim, ...){
  
  args <- expand.grid(..., stringsAsFactors = FALSE)
  view.bounds <- view_bounds(g.view)
  clip.id <- svg_id(g_mask(g.view))
  g.geom <- svg_node('g', g.view, c('stroke'=as.rgb(col), as.lty(lty), 'clip-path'=sprintf("url(#%s)",clip.id), args))
  
  if (!is.null(v)){
    
    x <- svg_coords(x=v,xlim=xlim, view.bounds = view.bounds)$x
    for (i in seq_len(length(x))){
      svg_node('path', g.geom, c(d=sprintf('M %s,%s v %s',x[i], view.bounds[['y']], view.bounds[['height']])))
    }
  } else if (!is.null(v)){
    
    y <- svg_coords(y=h,ylim=ylim, view.bounds = view.bounds)$y
    for (i in seq_len(length(y))){
      svg_node('path', g.geom, c(d=sprintf('M %s,%s h %s',view.bounds[['x']], y[i], view.bounds[['width']])))
    }
  } else {
    message('these arguments are not currently supported for render_abline')
  }
  
  
}
