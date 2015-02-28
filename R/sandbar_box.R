#'@importFrom XML saveXML newXMLNode
#'@export
sandbar_box <- function(){
  trips = list("Feb-1980" = rnorm(12,mean=400,sd=200),
               "Apr-1989" = rnorm(12,mean=452,sd=122),
               "Jun-1994" = rnorm(25,mean=700,sd=162),
               "May-1999" = rnorm(12,mean=452,sd=122),
               "Jun-2001" = rnorm(12,mean=452,sd=222),
               "Apr-2002" = rnorm(12,mean=452,sd=122),
               "Jun-2004" = rnorm(25,mean=700,sd=162),
               "May-2005" = rnorm(12,mean=452,sd=122),
               "Jun-2009" = rnorm(12,mean=452,sd=222),
               "Jul-2012" = rnorm(11,mean=952,sd=256))
  
  
  bp <- boxplot(trips, las = 2, ylab = 'Variable (?/L)', plot = T)
  y_ticks <- axTicks(2)
  y_lim <- par()$usr[c(3, 4)]
  x_lim <- par()$usr[c(1, 2)] # note first marker is on 1, incrementing as int
  dev.off()
  # --- pixel dims ---
  axes <- list('tick_len' = 5,
               'y_label' = "Sand Volume in Eddy (m^3)",
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
  
  newXMLNode("rect", parent = g_id, newXMLTextNode('Tooltip'),
             attrs = c(class="label", id="tooltip_bg", x="0", y="0", rx="4", ry="4", 
                       width="55", height="28", style="fill:#f6f6f6;fill-opacity:0.85;stroke:#696969;",
                       visibility="hidden"))
  
  newXMLNode("text", parent = g_id, newXMLTextNode('Tooltip'),
                   attrs = c(class="label", id="tooltip", x="0", y="0", 
                             visibility="hidden"))
  
  root_nd <- xmlRoot(g_id)

  saveXML(root_nd, file = './sandbar_boxplot.svg')
}