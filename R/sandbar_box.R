#'@importFrom XML saveXML newXMLNode
#'@export
sandbar_box <- function(){
  trips = list("1980-01-03" = rnorm(12,mean=400,sd=200),
               "1989-05-01" = rnorm(12,mean=452,sd=122),
               "1994-06-15" = rnorm(25,mean=700,sd=162),
               "1999-05-01" = rnorm(12,mean=452,sd=122),
               "2001-06-15" = rnorm(12,mean=452,sd=222),
               "2002-05-01" = rnorm(12,mean=452,sd=122),
               "2004-06-15" = rnorm(25,mean=700,sd=162),
               "2005-05-01" = rnorm(12,mean=452,sd=122),
               "2009-06-15" = rnorm(12,mean=452,sd=222),
               "2012-06-14" = rnorm(11,mean=952,sd=256))
  
  
  bp <- boxplot(trips, las = 2, ylab = 'Variable (?/L)', plot = T)
  y_ticks <- axTicks(2)
  y_lim <- par()$usr[c(3, 4)]
  x_lim <- par()$usr[c(1, 2)] # note first marker is on 1, incrementing as int
  dev.off()
  # --- pixel dims ---
  axes <- list('tick_len' = 5,
               'y_label' = "Eddy volume (cubic meters)",
               'y_ticks' = y_ticks,
               'y_tk_label' = y_ticks,
               'x_ticks' = seq_len(length(trips)),
               'x_tk_label' = names(trips),
               'y_lim' = y_lim,
               'x_lim' = x_lim)
  
  fig <- list('w' = 650,
              'h' = 450,
              'margins' = c(100,80,10, 10)) #bot, left, top, right
  
  fig$px_lim <- list("x" = c(fig$margins[2], fig$w-fig$margins[2]-fig$margins[4]),
                     "y" = c(fig$h-fig$margins[3]-fig$margins[1], fig$margins[3]))
  
  boxes <- list('width' = 0.65,
                'rat_whisker' = 0.6, # ratio of whisker to box width
                'def_opacity' = 0.5)
  
  
  
  g_id <- svg_init(fig, boxes$def_opacity)
  add_axes(g_id, axes, fig)
  
  
  # for one trip:
  for (i in 1:length(trips)){
    
    add_boxwhisk(g_id, y_stats = bp$stats[, i], y_data = trips[[i]], x_val = i, box_id = i, axes, fig, boxes)
    
  }
  
  
  add_usgs(g_id,
           base_x = fig$w-fig$margins[2]-fig$margins[4],
           base_y = fig$h-fig$margins[1]-fig$margins[3])
  
  newXMLNode("text", parent = g_id, newXMLTextNode('Tooltip'),
                   attrs = c(class="label", id="tooltip", x="0", y="0", 
                             visibility="hidden"))
  
  root_nd <- xmlRoot(g_id)

  saveXML(root_nd, file = './sandbar_boxplot.svg')
}