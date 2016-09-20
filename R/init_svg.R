#' @export
#' @importFrom xml2 xml_new_document xml_attrs<- xml_add_child
init_svg <- function(width, height, ..., version="1.1"){
  
  
  ppi <- 72 # points per inch
  svg <- xml_new_document()
  svg <- xml_add_child(svg, 'svg')
  xml_attrs(svg) <- c("xmlns"="http://www.w3.org/2000/svg",'xmlns:xlink'="http://www.w3.org/1999/xlink",
                      viewBox=sprintf("0 0 %1.0f %1.0f", width*ppi, height*ppi), ...)
  defs <- xml_add_child(svg, 'defs')
  cdata <- "<![CDATA[
    line, polyline, path, rect, circle {
      fill: none;
      stroke: #000000;
      stroke-linecap: round;
      stroke-linejoin: round;
      stroke-miterlimit: 10.00;
    }
  ]]>"
  xml_add_child(defs, 'style', type='text/css', cdata)
  invisible(svg)
}

