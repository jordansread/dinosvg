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
svg_view_bounds <- function(svg, mai){
  
  ppi <- 72 # points per inch, this SHOULD BE A PKG VAR...
  mar <- mai*ppi
  view.box <- strsplit(xmlAttrs(svg)[['viewBox']],'[ ]')[[1]] %>% 
    as.numeric()
  
  c(x = view.box[1]+mar[2], y = view.box[2]+mar[2], 
         width = view.box[3]-mar[2]-mar[4], 
         height = view.box[4]-mar[1]-mar[3])
}
#' @importFrom XML xpathApply
view_bounds <- function(svg, side){
  box_node <- xpathApply(svg, sprintf("//*[local-name()='g'][@id='view-%s-%s']//*[local-name()='rect'][@id='axes-box']", side[1], side[2]))
  sapply(xmlAttrs(box_node[[1]])[c('x','y','height','width')], as.numeric)
}

#' @importFrom XML newXMLTextNode
svg_window <- function(svg, window){

  g.view <- svg_node('g', svg, c('id'=sprintf('view-%s-%s', window$side[1], window$side[2])))
  g.axes <- svg_node('g', g.view, c('id'="axes", 'fill'="none", 'stroke'="#000000", 'stroke-width'="1"))
  
  ax <- svg_view_bounds(svg, mai=par()$mai)
  svg_node('rect', g.axes, c(x=ax[['x']], y=ax[['y']], height=ax[['height']], width=ax[['width']], id='axes-box'))
  
  view.bounds <- view_bounds(svg, window$side)
  
  x.axis <- svg_node('g', g.axes, c(id='x-axis'))
  y.axis <- svg_node('g', g.axes, c(id='y-axis'))
  
  tick.len <- 5
  
  axis_side_1(x.axis, lim=window$xlim, view.bounds = view.bounds, tick.len = tick.len)
  axis_side_2(y.axis, lim=window$ylim, view.bounds = view.bounds, tick.len = tick.len)
}

axis_side_1 <- function(g.axis, at=NULL, lim, view.bounds, tick.len, ...){
  
  if (is.null(at))
    at <- pretty(lim)
  
  at <- at[at >= min(lim) & at <= max(lim)]
  
  coords <- svg_coords(at, lim, c(view.bounds[['x']], view.bounds[['x']] + view.bounds[['width']]))
  paths <- paste0('M', paste(paste(as.vector(sapply(coords, rep, 2)),
                                   c(view.bounds[['y']] + view.bounds[['height']], view.bounds[['y']] + view.bounds[['height']] - tick.len), sep=','),
                             c('L','M'), collapse=''))
  svg_node('path', g.axis, c(d=paths, id='ticks'))
  
  x.axis.text <- svg_node('g', g.axis, c(id='tick-labels', stroke='none',fill='#000000', 'text-anchor'="middle"))
  for (i in seq_len(length(at))){
    newXMLNode("text", newXMLTextNode(at[i]), 'parent' = x.axis.text,
               attrs=c(x=coords[i], y=view.bounds[['y']] + view.bounds[['height']], dy='1.0em'))
  }
}

axis_side_2 <- function(g.axis, at=NULL, lim, view.bounds, tick.len, ...){
  if (is.null(at))
    at <- pretty(lim)
  
  at <- at[at >= min(lim) & at <= max(lim)]
  
  coords <- svg_coords(at, lim, c(c(view.bounds[['y']] + view.bounds[['height']], view.bounds[['y']])))
  paths <- paste0('M', paste(paste(c(view.bounds[['x']] , view.bounds[['x']] + tick.len), as.vector(sapply(coords, rep, 2)),
                                     sep=','),c('L','M'), collapse=''))
  svg_node('path', g.axis, c(d=paths, id='ticks'))
  
  y.axis.text <- svg_node('g', g.axis, c(id='tick-labels', stroke='none',fill='#000000', 'text-anchor'="end"))
  for (i in seq_len(length(at))){
    svg_node("text", y.axis.text, c(y=coords[i], x=view.bounds[['x']], dx='-0.33em', dy='0.33em'), newXMLTextNode(at[i]))
  }
}
svg_node <- function(name, parent, attrs, ...){
  invisible(newXMLNode(name = name, parent = parent,
             attrs=attrs, ...))
}

as.crd <- function(x){
  sprintf('%s',round(as.vector(x), digits = 3))
}

#' points to svg
#' 
#' @param \dots all end up at attributes to the element
svg_points <- function(svg, window, points, ...){
  
  ax <- view_bounds(svg, side=window$side)
  
  x <- svg_coords(points$x, window$xlim, c(ax[['x']], ax[['x']] + ax[['width']]))
  y <- svg_coords(points$y, window$ylim, c(ax[['y']] + ax[['height']], ax[['y']]))
  
  view_node <- xpathApply(svg, sprintf("//*[local-name()='g'][@id='view-%s-%s']", window$side[1], window$side[2]))
  g.id <- newXMLNode('g', parent = view_node, attrs=c(fill='red'))
  
  for (i in seq_len(length(y))){
    newXMLNode('circle', parent = g.id,
               attrs=c(cx=x[i], cy=y[i], r='5'))
  }
  invisible(svg)
}