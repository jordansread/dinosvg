render_lines <- function(g.view, x, y, pch=par("pch"), lty=par("lty"), col=par("col"), lwd=par("lwd"), xlim, ylim, ...){
  
  args <- expand.grid(..., stringsAsFactors = FALSE)
  view.bounds <- view_bounds(g.view)
  coords <- svg_coords(x, y, xlim, ylim, view.bounds)
  clip.id <- svg_id(g_mask(g.view))
  
  g.geom <- svg_node('g', g.view, c('stroke'=as.rgb(col), 'fill'="none", 'clip-path'=sprintf("url(#%s)",clip.id), args))

  path <- paste0('M',paste(coords$x, coords$y,sep=',', collapse=' '))
  svg_node('path', g.geom, c(d=path))
}
