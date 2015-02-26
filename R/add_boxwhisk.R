#'@export
add_boxwhisk <- function(g_id, y_stats, y_data, x_val, box_id, axes, fig, boxes ){
  
  box_fill = "#40DFD0"
  stroke = "black"
  box_opc = 0.3
  cir_opc = 0.3
  box_lw = 1.5
  cir_lw = 1
  cir_fill = "grey"
  
  style_box <- sprintf('fill:%s;stroke:%s;fill-opacity:%s;stroke-width:%s',
                       box_fill, stroke, box_opc, box_lw)
  style_line <- sprintf('stroke:%s;stroke-width:%s',
                        stroke, box_lw)
  
  px_width = tran_x(axes$x_lim[1]+boxes$width, axes, fig) - tran_x(axes$x_lim[1], axes, fig) # make sure it is within the box, otherwise it gets a inf
  wk_width = px_width*boxes$rat_whisker
  
  x_px = rep(NA,5)
  x_px[3] = tran_x(x_val, axes, fig)
  x_px[1] = x_px[3] - px_width/2  # for left edge
  x_px[2] = x_px[3] - (boxes$rat_whisker*px_width)/2
  x_px[4] = x_px[2] + boxes$rat_whisker*px_width
  y_px <- tran_y(y_stats, axes, fig)
  
  px_height = y_px[2] - y_px[4]
  
  rect(parent = g_id, x_px[1], y_px[4], px_height, px_width, style_box, box_id)
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
    style_circle <-  sprintf('fill:%s;stroke:%s;stroke-width:%s',
                             cir_fill, stroke, cir_lw)
    
    circle(g_id,x_px[3],cy, style_circle, id = 'fake_id')
          
  }

}
