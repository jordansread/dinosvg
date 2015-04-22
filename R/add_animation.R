# currently, only supporting key frames and key times
animate_attribute <- function(parent, attr_name, begin, dur, fill = 'freeze', values, keyTimes, id = 'animate_node'){
  
  
  if (length(values) != length(keyTimes)){
    stop('values and keyTimes must have the same length')
  }
  if (any(keyTimes > 1 | keyTimes < 0)){
    stop('keyTimes must be between 0 and 1, inclusive')
  }
  
    
  animate_nd <- newXMLNode("animate", 'parent' = parent,
                           attrs = c('attributeName' = attr_name,
                                     'id' = id,
                                     'begin' = begin,
                                     'dur' = dur,
                                     'fill' = fill,
                                     'values' = values))
  invisible(animate_nd)  
}
