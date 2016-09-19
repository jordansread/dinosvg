#' @export
#' @importFrom xml2 xml_new_document xml_attrs
init_svg <- function(width, height, ...){
  
  
  ppi <- 72 # points per inch
  attrs <- expand.grid(..., stringsAsFactors = FALSE)
  svg <- xml_new_document()
  svg <- xml_add_child(svg, 'svg')
  xml_attrs(svg) <- c("xmlns"="http://www.w3.org/2000/svg",'xmlns:xlink'="http://www.w3.org/1999/xlink",
                      'version'="1.1", 'preserveAspectRatio'="xMinYMin meet", 
                      viewBox=sprintf("0 0 %1.0f %1.0f", width*ppi, height*ppi), attrs)
  invisible(svg)
}

