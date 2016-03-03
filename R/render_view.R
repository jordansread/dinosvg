render_view <- function(svg, view){
  window <- view[['window']]
  geoms <- view
  geoms[['window']] <- NULL
  par(window[['par']])
  
  render_window(svg, window)
  
  g.view <- g_view(svg,window[['side']])
  g.axes <- g_axes(g.view)
  view.bounds <- view_bounds(g.view)
  
  tick.len <- 5
  
  #render_axis(g.axes, window[['side']][1], lim=window$xlim, view.bounds = view.bounds, tick.len = tick.len, axis.label=window$xlab)
  render_axis(g.axes, window[['side']][2], lim=window$ylim, view.bounds = view.bounds, tick.len = tick.len, axis.label=window$ylab)
  
  render_geoms(g.view, geoms, window)
  
}


#' @importFrom XML newXMLTextNode
render_window <- function(svg, window){
  
  ax <- svg_view_bounds(svg, mai=par()$mai)
  
  g.view <- svg_node('g', svg, c('id'=sprintf('view-%s-%s', window$side[1], window$side[2])))
  svg_node('rect', svg_node('clipPath',svg_node('defs',g.view), c(id=sprintf('mask-%s-%s', window$side[1], window$side[2]))), c(x=ax[['x']], y=ax[['y']], height=ax[['height']], width=ax[['width']]))
  g.axes <- svg_node('g', g.view, c('id'="axes", 'fill'="none", 'stroke'="#000000", 'stroke-width'="1"))
  
  svg_node('rect', g.axes, c(x=ax[['x']], y=ax[['y']], height=ax[['height']], width=ax[['width']], id='axes-box'))
}

render_geoms <- function(g.view, geoms, window){
  for (i in seq_len(length(geoms))){
    fun_name <- paste0('render_',names(geoms[i]))
    if (exists(fun_name)){
      args <- append(list(g.view=g.view), geoms[[i]]) %>% 
        append(list(xlim=window[['xlim']], ylim=window[['ylim']]))
      # hack! but removes duplicate formals (e.g., xlim specified both in window and the args for the function)
      do.call(fun_name, args[unique(names(args))])
    } else {
      message(fun_name, " doesn't exist in ", packageName())
    }
  }
}
