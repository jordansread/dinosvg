render_arrows <- function(g.view, x0, y0, x1=x0, y1=y0, length=0.25, angle=90, code=2, col=par("col"), lty=par("lty"), lwd=par("lwd"), xlim, ylim, ...){
  
  px.width <- 30
  
  if (angle != 90)
    message('angle != 90 not yet supported in render_arrows')
  args <- expand.grid(..., stringsAsFactors = FALSE)
  view.bounds <- view_bounds(g.view)
  coords.from <- svg_coords(x0, y0, xlim, ylim, view.bounds)
  coords.to <- svg_coords(x1, y1, xlim, ylim, view.bounds)
  
  clip.id <- svg_id(g_mask(g.view))
  
  g.geom <- svg_node('g', g.view, c('stroke'=as.rgb(col), as.lty(lty), 'clip-path'=sprintf("url(#%s)",clip.id), args))
  
  for (i in seq_len(length(coords.from$x))){
    path <- paste0('M',paste(paste(coords.from$x[i], coords.from$y[i],sep=',', collapse=' '), 
                             'L', paste(coords.to$x[i], coords.to$y[i],sep=',', collapse=' ')),
                   'M', paste(paste(as.crd(as.numeric(coords.to$x[i])-length*px.width), coords.to$y[i],sep=',', collapse=' '),
                              'L', paste(as.crd(as.numeric(coords.to$x[i])+length*px.width), coords.to$y[i],sep=',', collapse=' ')))
    svg_node('path', g.geom, c(d=path))
  }
  
}
