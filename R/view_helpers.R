#' @importFrom XML xmlAttrs
svg_view_bounds <- function(svg, mai){
  
  ppi <- 72 # points per inch, this SHOULD BE A PKG VAR...
  mar <- mai*ppi
  view.box <- strsplit(xmlAttrs(svg)[['viewBox']],'[ ]')[[1]] %>% 
    as.numeric()
  
  c(x = view.box[1]+mar[2], y = view.box[2]+mar[3], 
    width = view.box[3]-mar[2]-mar[4], 
    height = view.box[4]-mar[1]-mar[3])
}
#' @importFrom XML xpathApply
view_bounds <- function(svg, side=NULL){
  if (is.null(side)){
    box.node <- xpath_one(svg, "//*[local-name()='rect'][@id='axes-box']")
  } else {
    box.node <- xpath_one(svg, sprintf("//*[local-name()='g'][@id='view-%s-%s']//*[local-name()='rect'][@id='axes-box']", side[1], side[2]))
  }
  
  sapply(xmlAttrs(box.node)[c('x','y','height','width')], as.numeric)
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
