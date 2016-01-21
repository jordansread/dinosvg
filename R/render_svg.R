#' render svg
#' 
#' create svg document from gsplot object 
#' 
#' @param object a gsplot object
#' @param \dots additional args passed to \code{svg.gsplot}
#' 
#' @examples 
#' \dontrun{
#' library(gsplot)
#' gs <- gsplot() %>% 
#'    points(y=1:11, x=1:11, 
#'             col="blue", pch=18, hovertext=paste0('text:',1:11), xlab='pizza', ylab='dogs', 
#'             id=paste0('point',1:11), 'fill-opacity'='0.3', 'stroke-opacity'=seq(0,1, length.out = 11)) %>% 
#'    points(x=4:11, y=11:4, 
#'             col="red", pch=1, hovertext=paste0('text:',11:4)) %>% 
#'    points(3:5,4:6,side=c(1,4), col='green', hovertext='green', pch=14, ylab='cats') %>% 
#'    lines(2:5, c(2,2.6,2.3), col='blue')
#' svg(gs)
#' }
#' @export
#' @import gsplot
svg <- function(object, ...){
  UseMethod('svg')
}

#' @export
svg.gsplot <- function(object, file = "Rplot.svg", width = 6, height = 4.3, pointsize = 12, ...){

  svg <- init_svg(width, height, ...)
  add_css(svg)
  
  for (view in gsplot:::views(object)){
    par(par(object)) # set global par to object par
    render_view(svg, view)
  }
  add_tooltip(svg)
  # get par
  # set the page dimensions
  # translate coordinates
  # build axes
  # do.call for gsplot elements, skip those w/o `svg_` functions and warn
  # invisible return of filename
  return(write_svg(svg, file))
}



#' @importFrom XML xmlAttrs
svg_id <- function(ele){
  xmlAttrs(ele)[[1]]
}


xpath_one <- function(svg, xpath){
  nodes <- xpathApply(svg, xpath)
  
  if (length(nodes) > 1)
    stop('more than one element found for ', xpath)

  return(nodes[[1]])
}


#' @importFrom XML newXMLNode
svg_node <- function(name, parent, attrs=NULL, ...){
  invisible(newXMLNode(name = name, parent = parent,
             attrs=attrs, ...))
}

