render_axis <- function(g.axes, side, at=NULL, lim, view.bounds, tick.len, ...){
  
  at <- at[at >= min(lim) & at <= max(lim)]
  g.axis <- svg_node('g', g.axes, id=sprintf('axis-side-%s', side))
  do.call(sprintf('render_axis_%s', side), list(g.axis, at=at, lim=lim, view.bounds = view.bounds, tick.len = tick.len, ...))
}

render_x_axis <- function(g.axis, side, at=NULL, labels=at, lim, view.bounds, tick.len, axis.label, las=par('las'), ...){

  args <- filter_dot_args(...)
 x <- svg_coords(x=at, xlim=lim, view.bounds=view.bounds)$x
 y <- as.character(c(view.bounds[['y']] + view.bounds[['height']], NA, view.bounds[['y']]))
 tick.len <- c(-tick.len, NA, tick.len)
 tck.dy <- c('1.0em', NA, '-0.33em')
 lab.dy <- c('2.0em', NA, '-1.33em')
 
 get_anchor <- function(las, side){
   if (is.null(las)){
     las = par('las',side)
   }
   if (las == 0 || las == 1){
     return('middle')
   } else{
     if (side == 1)
       return("end")
     else 
       return('begin')
   }
 }
 
 get_position <- function(x,y, las, side){
   if (is.null(las)){
     las = par('las',side)
   }
   if (las == 0 || las == 1){
     return(c(x=x, y=y, dy=tck.dy[side]))
   } else{
     if (side == 1)
       return(c(dy="0.33em", dx="-0.33em", transform=sprintf("translate(%s,%s) rotate(-90)", x,y)))
     else 
       return(c(dy="-0.33em", dx="0.33em", transform=sprintf("translate(%s,%s) rotate(90)", x,y)))
   }
   
   
 }
 
 if (!is.null(at)){
   tick.labels <- svg_node('g', g.axis, id='tick-labels', stroke='none',fill='#000000', 'text-anchor'=get_anchor(las, side))
   ticks <- svg_node('g', g.axis, id='ticks', fill="none", stroke="#000000", `stroke-width`="1")
   for (i in seq_len(length(at))){
     svg_node("path", ticks, d=sprintf('M %s,%s v %s',x[i], y[side], tick.len[side]))
     if (length(labels) == 1 && !labels){
       
     } else {
       svg_node("text", tick.labels, c(get_position(x=x[i], y=y[side], las, side), nd_args(args,i)), labels[i])
     }
     
   }
 }
 
 
 if (!missing(axis.label) & axis.label != ''){
   a.axis.label <- svg_node('g', g.axis, id='axis-label', stroke='none',fill='#000000', 'text-anchor'="middle")
   svg_node("text", a.axis.label, x=as.crd(view.bounds[['x']]+view.bounds[['width']]/2), y=y[side], dy=lab.dy[side], axis.label)
 }
}

render_axis_1 <- function(g.axis, at=NULL, lim, view.bounds, tick.len, axis.label, ...){
  
  render_x_axis(g.axis, side=1, at, lim, view.bounds, tick.len, axis.label, ...)
}

render_axis_3 <- function(g.axis, at=NULL, lim, view.bounds, tick.len, axis.label,...){
  
  render_x_axis(g.axis, side=3, at, lim, view.bounds, tick.len, axis.label, ...)
}

render_y_axis <- function(g.axis, side, at=NULL, lim, view.bounds, tick.len, axis.label, ...){
  
  y <- svg_coords(y=at, ylim=lim, view.bounds=view.bounds)$y
  x <- c(NA, view.bounds[['x']], NA, view.bounds[['x']] + view.bounds[['width']])
  tick.len <- c(NA, tick.len, NA, -tick.len)
  txt.anc <- c(NA, 'end',NA,'begin')
  tck.dy <- c(NA, '0.33em', NA, '0.33em')
  tck.dx <- c(NA, '-0.33em', NA, '0.33em')
  lab.dy <- c(NA, '-2.0em', NA, '2.0em')
  
  tick.labels <- svg_node('g', g.axis, c(id='tick-labels', stroke='none',fill='#000000', 'text-anchor'=txt.anc[side]))
  ticks <- svg_node('g', g.axis, c(id='ticks'))
  for (i in seq_len(length(at))){
    svg_node("path", ticks, c(d=sprintf('M %s,%s h %s',x[side], y[i], tick.len[side])))
    svg_node("text", tick.labels, c(x=x[side], y=y[i], dx=tck.dx[side], dy=tck.dy[side]), newXMLTextNode(at[i]))
  }
  
  if (!missing(axis.label) & axis.label != ''){
    a.axis.label <- svg_node('g', g.axis, c(id='axis-label', stroke='none',fill='#000000', 'text-anchor'="middle"))
    y.pos <-as.crd(view.bounds[['y']] + view.bounds[['height']]/2)
    svg_node("text", a.axis.label, c(x=x[side], y=y.pos, dy=lab.dy[side],transform=sprintf("rotate(-90 %s,%s)",x[side],y.pos)), newXMLTextNode(axis.label))  
  }
}

render_axis_2 <- function(g.axis, at=NULL, lim, view.bounds, tick.len, axis.label,...){
  
  render_y_axis(g.axis, side=2, at, lim, view.bounds, tick.len, axis.label, ...)
}

render_axis_4 <- function(g.axis, at=NULL, lim, view.bounds, tick.len, axis.label,...){
  
  render_y_axis(g.axis, side=4, at, lim, view.bounds, tick.len, axis.label, ...)
}
