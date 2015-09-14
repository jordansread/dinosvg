#'@title add axes and ticks to an svg document. 
#'@details transformation from plot space to pixel space is handled here. 
#'@export
add_axes <- function(g_id, axes, fig, y_rotate = 270, x_tick_rotate=270){
  
  y_bump <- c(x=-5, y = 5)
  x_ax_bump <- (max(nchar(axes$y_ticks))+1)*10 #axis label bump (should be based on nchar of axis labels?)
  y_ax_bump <- (max(nchar(axes$x_ticks))+2)*10 #axis label bump (should be based on nchar of axis labels?)
  axis_style <- "fill:#FFFFFF;stroke:black"
  
  
  rect(g_id, fig$margins[2], fig$margins[3], diff(-fig$px_lim$y), diff(fig$px_lim$x), axis_style, id = "axis.box")
  y_ax_cent <- mean(c(fig$margins[3], fig$h-fig$margins[3]-fig$margins[1]))
  x_ax_cent <- mean(c(fig$margins[2], fig$w-fig$margins[4]-fig$margins[2]))
  txt(g_id, text=axes$y_label, fig$margins[2]-x_ax_bump, y_ax_cent, rotate = y_rotate, "middle", id='y-label')
  if (!is.null(axes$x_label)){
    txt(g_id, text=axes$x_label, x_ax_cent, fig$h-fig$margins[1]-fig$margins[3]+y_ax_bump, rotate = 0, "middle", id='x-label')
  }

  
  y1 = fig$h-fig$margins[1]-fig$margins[3]
  for (i in 1:length(axes$x_ticks)){
    x = tran_x(axes$x_ticks[i], axes, fig)
    
    line(g_id, c(x,x),c(y1,y1-axes$tick_len),style = "stroke:black;stroke-width:1.5")
    txt(g_id, text=axes$x_tk_label[i], x = x,
        y = fig$h-fig$margins[1]-fig$margins[3], rotate = x_tick_rotate, "middle", dy="1.0em")
  }
  x1 = fig$margins[2]
  for (i in 1:length(axes$y_ticks)){
    y <- tran_y(axes$y_ticks[i], axes, fig)
    
    line(g_id, c(x1,x1+axes$tick_len),c(y,y),style = "stroke:black;stroke-width:1.5")
    txt(g_id, text=axes$y_tk_label[i], x = fig$margins[2]+y_bump[['x']],
        y = y+y_bump[['y']], anchor = "end")
  }
  

}