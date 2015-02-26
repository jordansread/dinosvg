#'@export
add_boxwhisk <- function(g_id, y_stats, y_data, x_val, box_id, axes, fig, boxes ){
  style_box <- sprintf('fill:%s;stroke:%s;fill-opacity:%s;stroke-width:%s',"#40DFD0", "black",  0.3, 1.5)
  style_line <- sprintf('stroke:%s;stroke-width:%s', "black", 1.5)
  
  px_width = tran_x(axes$x_lim[1]+boxes$width, axes, fig) - tran_x(axes$x_lim[1], axes, fig) # make sure it is within the box, otherwise it gets a inf
  wk_width = px_width*boxes$rat_whisker
  
  x_px = rep(NA,5)
  x_px[3] = tran_x(x_val, axes, fig)
  x_px[1] = x_px[3] - px_width/2  # for left edge
  x_px[2] = x_px[3] - (boxes$rat_whisker*px_width)/2
  x_px[4] = x_px[2] + boxes$rat_whisker*px_width
  y_px <- tran_y(y_stats, axes, fig)
  
  px_height = y_px[2] - y_px[4]
  
  newXMLNode("rect", parent = g_id,
                    attrs = c(id = box_id, 
                              x = x_px[1], y = y_px[4], width = px_width, height = px_height,
                              'style' = style_box))
  line(g_id, c(x_px[3],x_px[3]), c(y_px[4],y_px[5]), style_line) 
  line(g_id, c(x_px[2],x_px[4]), c(y_px[5],y_px[5]), style_line) 
  line(g_id, c(x_px[2],x_px[4]), c(y_px[1],y_px[1]), style_line)
  line(g_id, c(x_px[3],x_px[3]), c(y_px[2],y_px[1]), style_line) 
  
  for (i in 1:length(y_data)){
    value <- y_data[i]
    cy = tran_y(val = value, axes, fig)
    
    if (value < y_stats[1] | value > y_stats[5]){
      stroke = 'black'
    } else {
      stroke = 'none'
    }
    style_circle <-  sprintf('fill:%s;stroke:%s;fill-opacity:%s;stroke-width:%s',"grey", stroke,  0.2, 1)
    circle(g_id,x_px[3],cy, style_circle, id = 'fake_id')
          
  }

}
line <- function(parent, x,y,style){
  x = sprintf('%1.1f',x)
  y = sprintf('%1.1f',y)
  newXMLNode("line", 'parent' = parent,
             attrs = c(x1 = x[1], y1 = y[1], x2 = x[2], y2 = y[2],
                       'style' = style))
}
circle <- function(parent, x, y, style, id){
  mouse_move_txt <- sprintf("ShowTooltip(evt, '%s')",'_fake_name_')
  newXMLNode("circle", 'parent' = parent,
             attrs = c('id' = id,
                       'cx' = x, 'cy' = y, 'r' = "4",
                       'style' = style,
                       onmouseover = "MakeOpaque(evt)",
                       onmousemove = mouse_move_txt,
                       onmouseout = "HideTooltip(evt);evt.target.setAttribute('fill-opacity', '0.1')"))
}