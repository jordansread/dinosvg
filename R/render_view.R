#' @export
render_view <- function(svg, object, view.name){
  view <- object[[view.name]]
  geoms <- view
  geoms[['par']] <- NULL
  old.par <- par(view[['par']])
  x.side <- gsplot:::as.x_side(view.name)
  y.side <- gsplot:::as.y_side(view.name)
  xlim <- xlim(object, side = x.side)
  ylim <- ylim(object, side = y.side)
  render_window(svg, view, side=c(x.side, y.side))
  
  g.view <- g_view(svg, c(x.side, y.side))
  g.axes <- g_axes(g.view)

  # render_geoms(g.view, geoms, object, xlim, ylim)
  par(old.par)
}

#' @importFrom XML newXMLTextNode
render_window <- function(svg, view, side){

  ax <- svg_view_bounds(svg, mai=par()$mai, as.numeric=FALSE)

  g.view <- svg_node('g', svg, 'id'=sprintf('view-%s-%s', side[1], side[2]))
  g.mask <- svg_node('clipPath',svg_node('defs',g.view), id=sprintf('mask-%s-%s', side[1], side[2]))
  svg_node('rect', g.mask, x=ax[['x']], y=ax[['y']], height=ax[['height']], width=ax[['width']])
  g.axes <- svg_node('g', g.view, 'id'="axes", 'fill'="none", 'stroke'="#000000", 'stroke-width'="1")
  
  svg_node('rect', g.axes, x=ax[['x']], y=ax[['y']], height=ax[['height']], width=ax[['width']], id='axes-box')
}

render_geoms <- function(g.view, geoms, object, xlim, ylim){
  
  for (i in seq_len(length(geoms))){
    fun_name <- paste0('render_',names(geoms[i]))
    if (exists(fun_name)){
      args <- append(list(g.view=g.view), geoms[[i]]) %>% 
        append(list(xlim=xlim, ylim=ylim))
      # hack! but removes duplicate formals (e.g., xlim specified both in window and the args for the function)
      do.call(fun_name, args[unique(names(args))])
    } else {
      message(fun_name, " doesn't exist in ", packageName())
    }
  }
}
