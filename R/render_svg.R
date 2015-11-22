#' render svg
#' 
#' @examples 
#' \dontrun{
#' library(gsplot)
#' gs <- gsplot()
#' gsNew <- points(gs, y=1:10, x=2:11, 
#'             col="blue", pch=18, hovertext='point')
#' svg(gsNew)
#' }
#' @export
#' @import gsplot
svg <- function(object, ...){
  UseMethod('svg')
}

#' @export
svg.gsplot <- function(object, file = "Rplot.svg", width = 6, height = 4.3, pointsize = 12, ...){

  par <- par(object)
  svg <- init_svg(width, height, ...)
  svg_window(svg, window=object$view$window)
  svg_points(svg, window=object$view$window, points=object$view$points)
  # get par
  # set the page dimensions
  # translate coordinates
  # build axes
  # do.call for gsplot elements, skip those w/o `svg_` functions and warn
  # invisible return of filename
  return(write_svg(svg, file))
}

#' @importFrom XML xmlAttrs
svg_view_bounds <- function(svg, mai=par()$mai){
  
  ppi <- 72 # points per inch, this SHOULD BE A PKG VAR...
  mar <- mai*ppi
  view.box <- strsplit(xmlAttrs(svg)[['viewBox']],'[ ]')[[1]] %>% 
    as.numeric()
  
  c(x = view.box[1]+mar[2], y = view.box[2]+mar[2], 
         width = view.box[3]-mar[2]-mar[4], 
         height = view.box[4]-mar[1]-mar[3])
}


#' @importFrom XML newXMLTextNode
svg_window <- function(svg, window){

  g.view <- svg_node('g', svg, c('id'=sprintf('view-%s-%s', window$side[1], window$side[2])))
  g.axes <- svg_node('g', g.view, c('id'="axes", 'fill'="none", 'stroke'="#000000", 'stroke-width'="1"))
  
  ax <- svg_view_bounds(svg)
  svg_node('rect', g.axes, c(x=ax[['x']], y=ax[['y']], height=ax[['height']], width=ax[['width']], id='axes-box'))
  
  
  tick.len <- 5
  
  x.p <- pretty(window$xlim)
  y.p <- pretty(window$ylim)
  coord.tick <- list(x = x.p[x.p >= min(window$xlim) & x.p <= max(window$xlim)],
                     y = y.p[y.p >= min(window$ylim) & y.p <= max(window$ylim)])
  # x-ticks
  tick.loc <- list(
    x = svg_coords(coord.tick$x, window$xlim, c(ax[['x']], ax[['x']] + ax[['width']]), return.out=T),
    y = svg_coords(coord.tick$x, window$ylim, c(ax[['y']] + ax[['height']], ax[['y']]), return.out=T))
  
  x.paths <- paste0('M', paste(paste(as.vector(sapply(tick.loc$x, rep, 2)),
                                     c(ax[['y']] + ax[['height']],ax[['y']] + ax[['height']] - tick.len), sep=','),c('L','M'), collapse=''))
  y.paths <- paste0('M', paste(paste(c(ax[['x']] , ax[['x']] + tick.len), as.vector(sapply(tick.loc$y, rep, 2)),
                                     sep=','),c('L','M'), collapse=''))
  
  
  x.axis <- newXMLNode('g', parent = g.axes, attrs=c(id='x-axis'))
  y.axis <- newXMLNode('g', parent = g.axes, attrs=c(id='y-axis'))
  
  newXMLNode('path', parent = x.axis,
             attrs=c(d=x.paths, id='x-axis-ticks'))
  x.axis.text <- newXMLNode('g', parent = x.axis, attrs=c(id='x-axis-text', stroke='none',fill='#000000', 'text-anchor'="middle"))
  for (i in seq_len(length(coord.tick$x))){
    newXMLNode("text", newXMLTextNode(coord.tick$x[i]), 'parent' = x.axis.text,
               attrs=c(x=tick.loc$x[i], y=ax[['y']] + ax[['height']], dy='1.0em'))
  }
  
  newXMLNode('path', parent = y.axis,
             attrs=c(d=y.paths, id='y-axis-ticks'))
  y.axis.text <- newXMLNode('g', parent = y.axis, attrs=c(id='y-axis-text', stroke='none',fill='#000000', 'text-anchor'="end"))
  for (i in seq_len(length(coord.tick$y))){
    newXMLNode("text", newXMLTextNode(coord.tick$y[i]), 'parent' = y.axis.text,
               attrs=c(y=tick.loc$y[i], x=ax[['x']], dx='-0.33em', dy='0.33em'))
  }
  invisible(svg)
}

svg_node <- function(name, parent, attrs, ...){
  invisible(newXMLNode(name = name, parent = parent,
             attrs=attrs, ...))
}

#' points to svg
#' 
#' @param \dots all end up at attributes to the element
svg_points <- function(svg, window, points, ...){
  
  ax <- svg_view_bounds(svg)
  
  x <- svg_coords(points$x, window$xlim, c(ax[['x']], ax[['x']] + ax[['width']]))
  y <- svg_coords(points$y, window$ylim, c(ax[['y']] + ax[['height']], ax[['y']]))
  
  g.id <- newXMLNode('g', parent = svg, attrs=c(fill='red'))
  
  for (i in seq_len(length(y))){
    newXMLNode('circle', parent = g.id,
               attrs=c(cx=x[i], cy=y[i], r='5'))
  }
  invisible(svg)
}