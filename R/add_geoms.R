#'@importFrom XML newXMLNode newXMLTextNode
line <- function(parent, x,y,style){
  x = sprintf('%1.1f',x)
  y = sprintf('%1.1f',y)
  newXMLNode("line", 'parent' = parent,
             attrs = c(x1 = x[1], y1 = y[1], x2 = x[2], y2 = y[2],
                       'style' = style))
}
circle <- function(parent, x, y, style, id){
  x = sprintf('%1.1f',x)
  y = sprintf('%1.1f',y)
  mouse_move_txt <- sprintf("ShowTooltip(evt, '%s')",'_fake_name_')
  newXMLNode("circle", 'parent' = parent,
             attrs = c('id' = id,
                       'cx' = x, 'cy' = y, 'r' = "4",
                       'style' = style,
                       'fill-opacity' = 0.3,
                       onmouseover = "MakeOpaque(evt)",
                       onmousemove = mouse_move_txt,
                       onmouseout = "HideTooltip(evt);evt.target.setAttribute('fill-opacity', '0.3')"))
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