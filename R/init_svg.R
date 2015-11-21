#' @export
#' @importFrom XML newXMLNode
init_svg <- function(width, height, par, pointsize, ...){
  
  attrs <- expand.grid(..., stringsAsFactors = FALSE)
  svg <- newXMLNode('svg',
                    namespace=c("http://www.w3.org/2000/svg", xlink="http://www.w3.org/1999/xlink"),
                    attrs=c('version'="1.1", 'preserveAspectRatio'="xMinYMin meet", 
                            viewBox=sprintf("0 0 %1.0f %1.0f", width*pointsize, height*pointsize), attrs))
  invisible(svg)
}