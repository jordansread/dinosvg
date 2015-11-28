render_points <- function(g.view, x, y, pch=par("pch"), col=par("col"), bg="#FFFFFF00", cex=1, lwd=par("lwd"), xlim, ylim, hovertext=NULL, ...){
  
  args <- expand.grid(..., stringsAsFactors = FALSE)
  view.bounds <- view_bounds(g.view)
  
  coords <- svg_coords(x, y, xlim, ylim, view.bounds)
  
  clip.id <- svg_id(g_mask(g.view))
  
  if (!is.null(hovertext) & length(hovertext) == 1){
    hovertext <- rep(hovertext, length(coords$x))
  }
  
  pch <- as.character(pch)
  g.geom <- svg_node('g', g.view, c('fill'=as.rgb(col), 'clip-path'=sprintf("url(#%s)",clip.id), args))
  
  for (i in seq_len(length(coords$x))){
    if (!is.null(hovertext)){
      hover.args <- c(onmouseover=sprintf("hovertext('%s',%s,%s)",hovertext[i],coords$x[i],coords$y[i]), onmouseout="hovertext(' ')") 
    } else 
      hover.args <- NULL
    points_node(g.geom, coords$x[i], coords$y[i], pch, as.rgb(col), as.rgb(col), cex, lwd, hover.args)
  }
}

as.path <- function(x,y){
  paste0('M ',paste(x,y,sep=',',collapse=' L'), 'Z')
}
points_node <- function(g, x, y, pch, col, bg, cex, lwd, ...){
  
  num <- as.numeric(c(x,y)) # hack! should still be numeric. Need to make it as.crd as the *last* step when entering node. This is slow I bet
  std.rad <- 2.7 #px
  geoms <- list(circle = c(cx=x, cy=y, r=as.crd(cex*std.rad)),
                sm.circle = c(cx=x, cy=y, r=as.crd(cex*std.rad*0.7)),
                square = c(d=as.path(x=c(num[1]-std.rad,num[1]+std.rad,num[1]+std.rad,num[1]-std.rad), 
                                     y=c(num[2]-std.rad,num[2]-std.rad,num[2]+std.rad,num[2]+std.rad))),
                up.tri = c(d=as.path(x=c(num[1],num[1]+std.rad*1.35,num[1]-std.rad*1.35), 
                                       y=c(num[2]-std.rad*1.5,num[2]+std.rad*0.75,num[2]+std.rad*0.75))),
                diamond = c(d=as.path(x=c(num[1],num[1]+std.rad*1.41,num[1],num[1]-std.rad*1.41), 
                                      y=c(num[2]-std.rad*1.41,num[2],num[2]+std.rad*1.41,num[2]))),
                cross = c(d=paste(as.path(x=c(num[1],num[1]),y=c(num[2]-std.rad*1.41,num[2]+std.rad*1.41)),
                          as.path(x=c(num[1]+std.rad*1.41, num[1]-std.rad*1.41),y=c(num[2], num[2])), sep=' ')),
                sm.cross = c(d=paste(as.path(x=c(num[1],num[1]),y=c(num[2]-std.rad,num[2]+std.rad)),
                                  as.path(x=c(num[1]+std.rad, num[1]-std.rad),y=c(num[2], num[2])), sep=' ')),
                x = c(d=paste(as.path(x=c(num[1]-std.rad,num[1]+std.rad), y=c(num[2]-std.rad,num[2]+std.rad)), 
                              as.path(x=c(num[1]+std.rad,num[1]-std.rad), y=c(num[2]-std.rad,num[2]+std.rad)), sep=' ')),
                dwn.tri = c(d=as.path(x=c(num[1],num[1]+std.rad*1.35,num[1]-std.rad*1.35), 
                                      y=c(num[2]+std.rad*1.5,num[2]-std.rad*0.75,num[2]-std.rad*0.75))),
                tri.square = c(d=sprintf('M %s,%s h %s v %s h %s Z M %s,%s L %s,%s L %s,%s', 
                                         num[1]-std.rad,num[2]-std.rad, 2*std.rad, 2*std.rad, -2*std.rad, 
                                         num[1]-std.rad,num[2]-std.rad, num[1], num[2]+std.rad, num[1]+std.rad, num[2]-std.rad)))
                
  
  transparent <- "#FFFFFF"
  # here, support multiple pch values w/ a switch
  node <- switch(pch,
          '0' = svg_node('path', g, c(geoms[['square']], fill=transparent, 'fill-opacity'="0", stroke=col, ...)),
          '1' = svg_node('circle', g, c(geoms[['circle']], fill=transparent, 'fill-opacity'="0", stroke=col, ...)),
          '2' = svg_node('path', g, c(geoms[['up.tri']], fill=transparent, 'fill-opacity'="0", stroke=col, ...)),
          '3' = svg_node('path', g, c(geoms[['cross']], fill=transparent, 'fill-opacity'="0", stroke=col, ...)),
          '4' = svg_node('path', g, c(geoms[['x']], fill=transparent, 'fill-opacity'="0", stroke=col, ...)),
          '5' = svg_node('path', g, c(geoms[['diamond']], fill=transparent, 'fill-opacity'="0", stroke=col, ...)),
          '6' = svg_node('path', g, c(geoms[['dwn.tri']], fill=transparent, 'fill-opacity'="0", stroke=col, ...)),
          '7' = svg_node('path', g, c(d=paste0(geoms[['square']][['d']],' ',geoms[['x']][['d']]), fill=transparent, 'fill-opacity'="0", stroke=col, ...)),
          '8' = svg_node('path', g, c(d=paste0(geoms[['cross']][['d']],' ',geoms[['x']][['d']]), fill=transparent, 'fill-opacity'="0", stroke=col, ...)),
          '9' = svg_node('path', g, c(d=paste0(geoms[['cross']][['d']],' ',geoms[['diamond']][['d']]), fill=transparent, 'fill-opacity'="0", stroke=col, ...)),
          '10' = svg_node('path', g, c(d=paste0(geoms[['sm.cross']][['d']],' ',geoms[['diamond']][['d']]), fill=transparent, 'fill-opacity'="0", stroke=col, ...)),
          '11' = svg_node('path', g, c(d=paste0(geoms[['up.tri']][['d']],' ',geoms[['dwn.tri']][['d']]), fill=transparent, 'fill-opacity'="0", stroke=col, ...)),
          '12' = svg_node('path', g, c(d=paste0(geoms[['sm.cross']][['d']],' ',geoms[['square']][['d']]), fill=transparent, 'fill-opacity'="0", stroke=col, ...)),
          '13' = svg_node('path', g, c(d=paste0(geoms[['x']][['d']],' ',geoms[['square']][['d']]), fill=transparent, 'fill-opacity'="0", stroke=col, ...)), #needs path circle
          '14' = svg_node('path', g, c(geoms[['tri.square']], fill=transparent, 'fill-opacity'="0", stroke=col, 'stroke-linejoin'="bevel", ...)),
          '15' = svg_node('path', g, c(geoms[['square']], fill=col, stroke=col, ...)),
          '16' = svg_node('circle', g, c(cx=x, cy=y, r=as.crd(cex*2.7), fill=col, stroke=col, ...)),
          '17' = svg_node('path', g, c(geoms[['up.tri']], fill=col, stroke=col, ...)),
          '18' = svg_node('path', g, c(geoms[['diamond']], fill=col, stroke=col, ...)),
          '19' = svg_node('circle', g, c(geoms[['circle']], fill=col, stroke=col, ...)),
          '20' = svg_node('circle', g, c(geoms[['sm.circle']], fill=col, stroke=col, ...)),
          '21' = svg_node('circle', g, c(geoms[['circle']], fill=bg, stroke=col, ...)),
          '22' = svg_node('path', g, c(geoms[['square']], fill=bg, stroke=col, ...)),
          '23' = svg_node('path', g, c(geoms[['diamond']], fill=bg, stroke=col, ...)),
          '24' = svg_node('path', g, c(geoms[['up.tri']], fill=bg, stroke=col, ...)),
          '25' = svg_node('path', g, c(geoms[['dwn.tri']], fill=bg, stroke=col, ...)),
          '.' = svg_node('circle', g, c(cx=x, cy=y, r=as.crd(cex*2.7), fill=transparent, 'fill-opacity'="0", stroke=col, ...)),
          'o' = svg_node('circle', g, c(cx=x, cy=y, r=as.crd(cex*2.7), fill=transparent, 'fill-opacity'="0", stroke=col, ...)),
          '0' = svg_node('circle', g, c(cx=x, cy=y, r=as.crd(cex*2.7), fill=transparent, 'fill-opacity'="0", stroke=col, ...)),
          'O' = svg_node('circle', g, c(cx=x, cy=y, r=as.crd(cex*2.7), fill=transparent, 'fill-opacity'="0", stroke=col, ...)),
          'a' = svg_node('circle', g, c(cx=x, cy=y, r=as.crd(cex*2.7), fill=transparent, 'fill-opacity'="0", stroke=col, ...)),
          'A' = svg_node('circle', g, c(cx=x, cy=y, r=as.crd(cex*2.7), fill=transparent, 'fill-opacity'="0", stroke=col, ...)),
          '*' = svg_node('circle', g, c(cx=x, cy=y, r=as.crd(cex*2.7), fill=transparent, 'fill-opacity'="0", stroke=col, ...)),
          '+' = svg_node('circle', g, c(cx=x, cy=y, r=as.crd(cex*2.7), fill=transparent, 'fill-opacity'="0", stroke=col, ...)),
          '-' = svg_node('circle', g, c(cx=x, cy=y, r=as.crd(cex*2.7), fill=transparent, 'fill-opacity'="0", stroke=col, ...)),
          '|' = svg_node('circle', g, c(cx=x, cy=y, r=as.crd(cex*2.7), fill=transparent, 'fill-opacity'="0", stroke=col, ...)))
  invisible(node)
}
