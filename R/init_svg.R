#' @export
#' @importFrom XML newXMLNode
init_svg <- function(width, height, ...){
  
  ppi <- 72 # points per inch
  
  attrs <- expand.grid(..., stringsAsFactors = FALSE)
  svg <- newXMLNode('svg',
                    namespace=c("http://www.w3.org/2000/svg", xlink="http://www.w3.org/1999/xlink"),
                    attrs=c('version'="1.1", 'preserveAspectRatio'="xMinYMin meet", 
                            viewBox=sprintf("0 0 %1.0f %1.0f", width*ppi, height*ppi), attrs))
  invisible(svg)
}

#' @importFrom XML saveXML
write_svg <- function(svg, file){
  
  saveXML(svg, file = file)
  invisible(file)
}