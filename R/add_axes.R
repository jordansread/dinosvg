#'@title add axes and ticks to an svg document. 
#'@details transformation from plot space to pixel space is handled here. 
#'@importFrom XML newXMLNode addChildren newXMLTextNode
#'@export
add_axes <- function(g_id, axes, fig){
  
  y_bump <- c(x=-5, y = 5)
  x_bump <- c(x=0, y = 5) #tick label bumps
  x_ax_bump <- (max(nchar(axes$y_ticks))+1)*10 #axis label bump (should be based on nchar of axis labels?)
  axis_style <- "fill:#FFFFFF;stroke:black"
  rect(g_id, fig$margins[2], fig$margins[3], diff(-fig$px_lim$y), diff(fig$px_lim$x), axis_style, id = "axis.box")
  
  
  y_ax_cent <- mean(c(fig$margins[3], fig$h-fig$margins[3]-fig$margins[1]))
  newXMLNode("text", newXMLTextNode(axes$y_label), parent = g_id,
             attrs = c(transform=paste0("translate(",fig$margins[2]-x_ax_bump,",",y_ax_cent,")rotate(270)"),
                       'text-anchor'="middle"))
  
  y1 = fig$h-fig$margins[1]-fig$margins[3]
  for (i in 1:length(axes$x_ticks)){
    x = tran_x(axes$x_ticks[i], axes, fig)
    
    line(g_id, c(x,x),c(y1,y1-axes$tick_len),style = "stroke:black;stroke-width:1.5")
    newXMLNode("text", newXMLTextNode(axes$x_tk_label[i]), parent = g_id, 
                        attrs = c('text-anchor' = "end",
                                  'transform' = sprintf("translate(%s,%s)rotate(%s)",x+x_bump[['y']],
                                                    fig$h-fig$margins[1]-fig$margins[3]+x_bump[['y']],
                                                    270)))
  }
  x1 = fig$margins[2]
  for (i in 1:length(axes$y_ticks)){
    y <- tran_y(axes$y_ticks[i], axes, fig)
    
    line(g_id, c(x1,x1+axes$tick_len),c(y,y),style = "stroke:black;stroke-width:1.5")

    label <- newXMLNode("text", newXMLTextNode(axes$y_tk_label[i]), parent = g_id, 
                        attrs = c('x' = fig$margins[2]+y_bump[['x']],
                                  'y' = y+y_bump[['y']],
                                  'text-anchor' = "end"))
  }
  

}