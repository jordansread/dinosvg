render_text <- function(g.view, x, y = NULL, labels = seq_along(x), adj = NULL,
                        pos = 2, offset = 0.5, vfont = NULL,
                        cex = 1, col = par("col"), font = NULL, xlim, ylim, ...){
  stopifnot(is.null(adj), is.null(font), is.null(vfont), offset==0.5)
  pos <- as.character(pos)
  args <- filter_dot_args(...)
  view.bounds <- view_bounds(g.view)
  g.text <- svg_node('g', g.view, c('fill'=as.rgb(col), g_args(args)))
  
  coords <- svg_coords(x, y, xlim, ylim, view.bounds = view.bounds)
  for (i in seq_len(length(coords$x))){
    svg_node("text", g.text, c(x=coords$x[i], y=coords$y[i], as.pos(pos), nd_args(args)), newXMLTextNode(labels[i]))  
  }
  
}

as.pos <- function(pos){
  #// to do: dx and dy should be aligned w/ offset
  switch(pos,
         '1'=c('text-anchor'='middle',dy='1.0em'), #below
         '2'=c('text-anchor'='end',dy='0.33em', dx='0.5em'), #to left of
         '3'=c('text-anchor'='middle',dy='-0.5em'), #above
         '4'=c('text-anchor'='start',dy='0.33em', dx='0.5em')) #to right of
  
}