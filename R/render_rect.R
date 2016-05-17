render_rect <- function(g.view, xleft, ybottom, xright, ytop, density=NULL, angle=45, col=NA, border=NULL, lty=par('lty'), lwd=par('lty'), xlim, ylim, hovertext=NULL,...){
  args <- filter_dot_args(...)
  
  view.bounds <- view_bounds(g.view)
  
  coords <- svg_coords(xleft, ytop, xlim, ylim, view.bounds)
  y.loc <- svg_coords(xleft, (ytop+ybottom)/2, xlim, ylim, view.bounds)$y
  width <- svg_coords(xright-xleft, xlim=xlim, view.bounds=view.bounds, as.crd=FALSE)$x - svg_coords(0, xlim=xlim, view.bounds=view.bounds, as.crd=FALSE)$x
  
  height <- svg_coords(xleft, 0, xlim=xlim, ylim=ylim, view.bounds=view.bounds, as.crd=FALSE)$y - svg_coords(xleft, ytop-ybottom, xlim=xlim, ylim=ylim, view.bounds=view.bounds, as.crd=FALSE)$y
  
  clip.id <- svg_id(g_mask(g.view))
  
  g.geom <- svg_node('g', g.view, c('clip-path'=sprintf("url(#%s)",clip.id), g_args(args)))
  
  for (i in seq_along(xleft)){
    if (!is.null(hovertext))
      hover.args <- c(onmouseover=sprintf("hovertext('%s',evt.target.getAttribute('x'),%s)",hovertext[i],y.loc[i]), onmouseout="hovertext(' ')") 
    else 
      hover.args <- NULL
    svg_node('rect', g.geom, c(x=coords$x[i], y=coords$y[i], height=height[i],width=width[i], 'fill'=as.rgb(col[i]), nd_args(args,i), hover.args))
  }
}
