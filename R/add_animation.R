# currently, only supporting key frames and key times
animate_attribute <- function(parent, attr_name, begin, ...){
  
  
  args <- expand.grid(..., stringsAsFactors = FALSE)
    
  animate_nd <- newXMLNode("animate", 'parent' = parent,
                           attrs = c('attributeName' = attr_name,
                                     'begin' = begin,
                                     args))
  invisible(animate_nd)  
}
