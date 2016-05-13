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
#'    lines(2:4, c(2,2.6,2.3), col='blue') %>% 
#'    axis(side=1, at = c(3,6.5,8.6))
#' svg(gs)
#' }
#' @export
#' @import gsplot
svg <- function(object, ...){
  UseMethod('svg')
}

#' @export
#' @importFrom XML toString.XMLNode
svg.gsplot <- function(object, file = "Rplot.svg", width = 6, height = 4.3, pointsize = 12, as.string=FALSE, ...){

  svg <- init_svg(width, height, ...)
  add_css(svg, css.text=object$css)
  
  # can add gsplot dinosvg section to object before all this, would would contain shared components
  
  for (view.name in gsplot:::view_names(object)){
    par(par(object)) # set global par to object par
    render_view(svg, object, view.name)
  }
  
  # for (side.name in gsplot:::side_names(object)){
  #   render_side(svg, side)
  # }
  # tick.len <- 5
  # window <- object$view.1.2$window
  # g.view <- g_view(svg,window[['side']])
  # g.axes <- g_axes(g.view)
  # view.bounds <- view_bounds(g.view)
  
  
  #add_tooltip(svg)
  add_ecmascript(svg, ecmascript.text=object$ecmascript)
  # get par
  # set the page dimensions
  # translate coordinates
  # build axes
  # do.call for gsplot elements, skip those w/o `svg_` functions and warn
  # invisible return of filename
  
  if (as.string){
    return(toString.XMLNode(svg))
  }
  write_svg(svg, file)
  base::rm(svg)
  return(file)
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

#' @importFrom XML saveXML 
write_svg <- function(svg, file){
  
  saveXML(svg, file = file)
  invisible(file)
}

