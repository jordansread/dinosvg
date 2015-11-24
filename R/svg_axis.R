render_axis <- function(g.axis, side, at=NULL, lim, view.bounds, tick.len, ...){
  if (is.null(at))
    at <- pretty(lim)
  
  at <- at[at >= min(lim) & at <= max(lim)]
  
  do.call(sprintf('render_axis_%s', side), list(g.axis, at=at, lim=lim, view.bounds = view.bounds, tick.len = tick.len, ...))
}

render_axis_1 <- function(g.axis, at=NULL, lim, view.bounds, tick.len, ...){
  
  coords <- dim_coords(at, lim, c(view.bounds[['x']], view.bounds[['x']] + view.bounds[['width']]))
  y <- c(view.bounds[['y']] + view.bounds[['height']], view.bounds[['y']] + view.bounds[['height']] - tick.len)
  x <- as.vector(sapply(coords, rep, 2))
  render_ticks(g.axis, x, y)
  
  tick.labels <- svg_node('g', g.axis, c(id='tick-labels', stroke='none',fill='#000000', 'text-anchor'="middle"))
  for (i in seq_len(length(at))){
    svg_node("text", tick.labels, c(x=coords[i], y=y[1], dy='1.0em'), newXMLTextNode(at[i]))
  }
}

render_axis_3 <- function(g.axis, at=NULL, lim, view.bounds, tick.len, ...){
  
  coords <- dim_coords(at, lim, c(view.bounds[['x']], view.bounds[['x']] + view.bounds[['width']]))
  y <- c(view.bounds[['y']], view.bounds[['y']] + tick.len)
  x <- as.vector(sapply(coords, rep, 2))
  render_ticks(g.axis, x, y)
  
  tick.labels <- svg_node('g', g.axis, c(id='tick-labels', stroke='none',fill='#000000', 'text-anchor'="middle"))
  for (i in seq_len(length(at))){
    svg_node("text", tick.labels, c(x=coords[i], y=y[1], dy='-1.0em'), newXMLTextNode(at[i]))
  }
}

render_axis_2 <- function(g.axis, at=NULL, lim, view.bounds, tick.len, ...){
  
  coords <- dim_coords(at, lim, c(c(view.bounds[['y']] + view.bounds[['height']], view.bounds[['y']])))
  y <- as.vector(sapply(coords, rep, 2))
  x <- c(view.bounds[['x']] , view.bounds[['x']] + tick.len)
  render_ticks(g.axis, x, y)
  
  tick.labels <- svg_node('g', g.axis, c(id='tick-labels', stroke='none',fill='#000000', 'text-anchor'="end"))
  for (i in seq_len(length(at))){
    svg_node("text", tick.labels, c(x=x[1], y=coords[i], dx='-0.33em', dy='0.33em'), newXMLTextNode(at[i]))
  }
}

render_axis_4 <- function(g.axis, at=NULL, lim, view.bounds, tick.len, ...){
  
  coords <- dim_coords(at, lim, c(c(view.bounds[['y']] + view.bounds[['height']], view.bounds[['y']])))
  y <- as.vector(sapply(coords, rep, 2))
  x <- c(view.bounds[['x']] + view.bounds[['width']], view.bounds[['x']] + view.bounds[['width']] - tick.len)
  render_ticks(g.axis, x, y)
  
  tick.labels <- svg_node('g', g.axis, c(id='tick-labels', stroke='none',fill='#000000', 'text-anchor'="begin"))
  for (i in seq_len(length(at))){
    svg_node("text", tick.labels, c(x=x[1], y=coords[i], dx='0.33em', dy='0.33em'), newXMLTextNode(at[i]))
  }
}

render_ticks <- function(g.axis, x, y){
  paths <- paste0('M', paste(paste(x, y, sep=','),c('L','M'), collapse=''))
  svg_node('path', g.axis, c(d=paths, id='ticks'))
}