#'@title add axes and ticks to an svg document. 
#'@details transformation from plot space to pixel space is handled here. 
#'@importFrom XML newXMLNode addChildren
#'@export
add_axes <- function(g_id, axes, fig){
  
  y_bump <- c(x=-5, y = 5)
  x_bump <- c(x=0, y = 5) #tick label bumps
  x_ax_bump <- (max(nchar(axes$y_ticks))+1)*10 #axis label bump (should be based on nchar of axis labels?)
  
  newXMLNode("rect", parent=g_id, 
             attrs = c(id="box1", x=fig$margins[2], y=fig$margins[3], 
                       width=diff(fig$px_lim$x), height=diff(-fig$px_lim$y),
                       style="fill: #FFFFFF;stroke: black; "))
  
  
  y_ax_cent <- mean(c(fig$margins[3], fig$h-fig$margins[3]-fig$margins[1]))
  newXMLNode("text", newXMLTextNode(axes$y_label), parent = g_id,
             attrs = c(transform=paste0("translate(",fig$margins[2]-x_ax_bump,",",y_ax_cent,")rotate(270)"),
                       'text-anchor'="middle"))
  
  y1 = fig$h-fig$margins[1]-fig$margins[3]
  for (i in 1:length(axes$x_ticks)){
    x = tran_x(axes$x_ticks[i], axes, fig)
    tck <- newXMLNode("line", attrs = c(x1 = x, y1 = y1, 
                                        x2 = x, y2 = y1 - axes$tick_len,
                                        style = "stroke:black;stroke-width:1.5"))
    label <- newXMLNode("text", newXMLTextNode(axes$x_tk_label[i]), parent = g_id, 
                        attrs = c('text-anchor'="end",
                                  transform=sprintf("translate(%s,%s)rotate(%s)",x+x_bump[['y']],
                                                    fig$h-fig$margins[1]-fig$margins[3]+x_bump[['y']],
                                                    270)))
    addChildren(g_id, c(tck,label))
  }
  x1 = fig$margins[2]
  for (i in 1:length(axes$y_ticks)){
    y <- tran_y(axes$y_ticks[i], axes, fig)
    
    tck <- newXMLNode("line", attrs = c(x1 = x1, y1 = y, x2 = x1+axes$tick_len, y2 = y,
                                        style = "stroke:black;stroke-width:1.5"))
    label <- newXMLNode("text", newXMLTextNode(axes$y_tk_label[i]), parent = g_id, 
                        attrs = c('x' = fig$margins[2]+y_bump[['x']],
                                  'y' = y+y_bump[['y']],
                                  'text-anchor' = "end"))
    
    addChildren(g_id, c(tck,label))
  }
  

}