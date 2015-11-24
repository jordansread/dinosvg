render_points <- function(g.view, x, y, pch=par("pch"), col=par("col"), bg="#FFFFFF00", cex=1, lwd=par("lwd"), xlim, ylim, hovertext=NULL, ...){
  
  args <- expand.grid(..., stringsAsFactors = FALSE)
  view.bounds <- view_bounds(g.view)
  radius <- as.crd(cex*2.7) # need to use ppi?
  coords <- svg_coords(x, y, xlim, ylim, view.bounds)
  
  clip.id <- svg_id(g_mask(g.view))
  
  if (!is.null(hovertext) & length(hovertext) == 1){
    hovertext <- rep(hovertext, length(coords$x))
  }
  
  g.geom <- svg_node('g', g.view, c('fill'=col, 'clip-path'=sprintf("url(#%s)",clip.id), args))
  
  for (i in seq_len(length(coords$x))){
    if (!is.null(hovertext)){
      hover.args <- c(onmouseover=sprintf("hovertext('%s',%s,%s)",hovertext[i],coords$x[i],coords$y[i]), onmouseout="hovertext(' ')") 
    } else 
      hover.args <- NULL
    svg_node('circle', g.geom, c(cx=coords$x[i], cy=coords$y[i], r=radius, hover.args))
  }
}

points_node <- function(g, x, y, pch, col, bg, cex, lwd){
  # here, support multiple pch values w/ a switch
}
