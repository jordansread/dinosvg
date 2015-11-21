#' render svg
#' 
#' @examples 
#' \dontrun{
#' library(gsplot)
#' gs <- gsplot()
#' gsNew <- points(gs, y=1, x=2, xlim=c(0,3),ylim=c(0,3),
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
  svg <- init_svg(width, height, ...) %>% 
    svg_window(window=object$view$window)
  # get par
  # set the page dimensions
  # translate coordinates
  # build axes
  # do.call for gsplot elements, skip those w/o `svg_` functions and warn
  # invisible return of filename
  return(write_svg(svg, file))
}

#' @importFrom XML xmlAttrs newXMLTextNode
svg_window <- function(svg, window){
  
  ppi <- 72 # points per inch, this SHOULD BE A PKG VAR...
  
  mar <- par()$mai*72 # c(bottom, left, top, right)
  tick.len <- 5
  g.axes <- newXMLNode('g', parent = svg,
                    attrs=c(id='axes', fill="none", stroke="#000000", 'stroke-width'="1"))
  
  view.box <- strsplit(xmlAttrs(svg)[['viewBox']],'[ ]')[[1]] %>% 
    as.numeric()
  
  ax = c(x = view.box[1]+mar[2], y = view.box[2]+mar[2], 
         width = view.box[3]-mar[2]-mar[4], 
         height = view.box[4]-mar[1]-mar[3])
  
  newXMLNode('rect', parent = g.axes,
             attrs=c(x=ax[['x']], y=ax[['y']], height=ax[['height']], width=ax[['width']], id='axes-box'))
  
  # x-ticks
  tick.loc <- list(
    x = svg_coords(pretty(window$xlim), window$xlim, c(ax[['x']], ax[['x']] + ax[['width']])),
    y = svg_coords(pretty(window$ylim), window$ylim, c(ax[['y']] + ax[['height']], ax[['y']])))
  
  x.paths <- paste0('M', paste(paste(as.vector(sapply(tick.loc$x, rep, 2)),
                                     c(ax[['y']] + ax[['height']],ax[['y']] + ax[['height']] - tick.len), sep=','),c('L','M'), collapse=''))
  y.paths <- paste0('M', paste(paste(c(ax[['x']] , ax[['x']] + tick.len), as.vector(sapply(tick.loc$y, rep, 2)),
                                     sep=','),c('L','M'), collapse=''))
  
  
  x.axis <- newXMLNode('g', parent = g.axes, attrs=c(id='x-axis'))
  y.axis <- newXMLNode('g', parent = g.axes, attrs=c(id='y-axis'))
  
  newXMLNode('path', parent = x.axis,
             attrs=c(d=x.paths, id='x-axis-ticks'))
  for (i in seq_len(length(tick.loc$x))){
    newXMLNode("text", newXMLTextNode(pretty(window$xlim)[i]), 'parent' = x.axis,
               attrs=c(x=tick.loc$x[i], y=ax[['y']] + ax[['height']], dy='1.0em', stroke='none',fill='#000000', 'text-anchor'="middle"))
  }
  
  newXMLNode('path', parent = y.axis,
             attrs=c(d=y.paths, id='y-axis-ticks'))
  invisible(svg)
}



#' points to svg
#' 
#' @param \dots all end up at attributes to the element
svg_points <- function(svg, window, x, y, col, cex=1, ...){
  
  invisible(svg)
}