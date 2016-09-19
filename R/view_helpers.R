#' @importFrom xml2 xml_attr
svg_view_bounds <- function(svg, mai, as.numeric=TRUE){
  
  ppi <- 72 # points per inch, this SHOULD BE A PKG VAR...
  mar <- mai*ppi
  view.box <- as.numeric(strsplit(xml_attr(svg, 'viewBox'),'[ ]')[[1]])
  
  view.bounds <- c(x = view.box[1]+mar[2], y = view.box[2]+mar[3], 
    width = view.box[3]-mar[2]-mar[4], 
    height = view.box[4]-mar[1]-mar[3])
  
  if (as.numeric){
    return(view.bounds)
  } else {
    return(sapply(view.bounds, as.character))
  }
  
  
}
#' @importFrom xml2 xml_attrs
view_bounds <- function(svg, side=NULL, as.numeric=TRUE){
  if (is.null(side)){
    box.node <- xpath_one(svg, "//*[local-name()='rect'][@id='axes-box']")
  } else {
    box.node <- xpath_one(svg, sprintf("//*[local-name()='g'][@id='view-%s-%s']//*[local-name()='rect'][@id='axes-box']", side[1], side[2]))
  }
  view.bounds <- xml_attrs(box.node)[c('x','y','height','width')]
  if (as.numeric){
    return(sapply(view.bounds, as.numeric))
  } else {
    return(view.bounds)
  }
}

g_view <- function(svg, side=NULL){
  if (is.null(side))
    xpath_one(svg, "//*[local-name()='g'][contains(@id,'view-']")
  else
    xpath_one(svg, sprintf("//*[local-name()='g'][@id='view-%s-%s']", side[1], side[2]))
}

g_axes <- function(svg, side=NULL){
  if (is.null(side))
    xpath_one(svg, "//*[local-name()='g'][@id='axes']")
  else
    xpath_one(svg, sprintf("//*[local-name()='g'][@id='view-%s-%s']//*[local-name()='g'][@id='axes']", side[1], side[2]))
}

g_mask <- function(svg, side=NULL){
  if (is.null(side)){
    xpath_one(svg, "//*[local-name()='clipPath'][contains(@id,'mask')]")
  } else {
    xpath_one(svg, sprintf("//*[local-name()='g'][@id='view-%s-%s']//*[local-name()='clipPath'][contains(@id,'mask')]", side[1], side[2]))
  }
}
