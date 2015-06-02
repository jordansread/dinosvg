#'@importFrom XML newXMLNode newXMLTextNode
line <- function(parent, x,y,style){
  x = sprintf('%1.1f',x)
  y = sprintf('%1.1f',y)
  newXMLNode("line", 'parent' = parent,
             attrs = c(x1 = x[1], y1 = y[1], x2 = x[2], y2 = y[2],
                       'style' = style))
}
circle <- function(parent, x, y, ...){
  x = sprintf('%1.1f',x)
  y = sprintf('%1.1f',y)
  
  args <- expand.grid(..., stringsAsFactors = F)
  node <- newXMLNode("circle", 'parent' = parent,
             attrs = c('cx' = x, 'cy' = y, args))
  invisible(node)
}
rect <- function(parent, x, y, h, w, style, id){
  x = sprintf('%1.1f',x)
  y = sprintf('%1.1f',y)
  h = sprintf('%1.1f',h)
  w = sprintf('%1.1f',w)
  newXMLNode("rect", 'parent' = parent,
             attrs = c('id' = id, 
                       x = x, y = y, width = w, height = h,
                       'style' = style))
}

txt <- function(parent, text, x, y, rotate = 0, anchor){
  x = sprintf('%1.1f',x)
  y = sprintf('%1.1f',y)
  newXMLNode("text", newXMLTextNode(text), 'parent' = parent,
             attrs = c('transform'=sprintf("translate(%s,%s)rotate(%s)",x, y, rotate),
                       'text-anchor'=anchor))
}

linepath <- function(parent, x,y, ...){
  args <- expand.grid(..., stringsAsFactors = F)
  
  x = sprintf('%1.1f',x)
  y = sprintf('%1.1f',y)
  path = ''
  # make this better later...
  for (i in 1:length(x)){
    if (!is.na(y[i]) & !is.na(x[i])){
      path <- paste0(path, ifelse(i==1,"M","L"), x[i],',',y[i], " ")
    }
    
  }
  
  node <- newXMLNode("path", 'parent' = parent,
             attrs = c(d = path, args))
  invisible(node)
}