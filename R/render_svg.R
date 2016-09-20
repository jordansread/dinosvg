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
#'             col="blue", pch=18, xlab='pizza', ylab='dogs', legend.name='thor') %>% 
#'    points(x=4:11, y=11:4, 
#'             col="red", pch=1, legend.name='rabbit') %>% 
#'    points(3:5,4:6,side=c(1,4), col='green', pch=14, ylab='cats') %>% 
#'    lines(2:4, c(2,2.6,2.3), col='blue') %>% 
#'    axis(side=1, at = c(3,6.5,8.6)) %>% 
#'    legend()
#' svg(gs)
#' }
#' @export
#' @import gsplot
svg <- function(object, ...){
  UseMethod('svg')
}

#' @export
#' @importFrom XML toString.XMLNode
svg.gsplot <- function(object, file = "Rplot.svg", width = 6, height = 4.3, pointsize = 12, version="1.1", preserveAspectRatio="xMinYMin meet", ...){

  svg <- init_svg(width, height, version = version, preserveAspectRatio = preserveAspectRatio,...)
  
  # can add gsplot dinosvg section to object before all this, would would contain shared components
  
  for (view.name in gsplot:::view_names(object)){
    old.par <- par(par(object)) # set global par to object par
    render_view(svg, object, view.name, width=width, height=height, pointsize=pointsize)
  }
  
  for (side.name in gsplot:::side_names(object)){
    render_side(svg, object, side.name, width=width, height=height, pointsize=pointsize)
  }
  
  render_legend(svg, object, width=width, height=height, pointsize=pointsize)
  # draw legend
  # draw box
  
  par(old.par)
  write_svg(svg, file)
  return(file)
}



#' @importFrom xml2 xml_attr
svg_id <- function(ele){
  xml_attr(ele,'id')
}


#' @importFrom xml2 write_xml
write_svg <- function(svg, file){
  
  write_xml(svg, file = file)
  invisible(file)
}

