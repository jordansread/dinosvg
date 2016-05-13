render_side <- function(svg, object, side.name){
  side <- object[[side.name]]
  g.view <- g_view(svg,window[['side']])
  g.axes <- g_axes(g.view)
  view.bounds <- view_bounds(g.view)
  
  render_x_axis(g.axes, side=1, at=object$axis$arguments$at, labels=object$axis$arguments$labels, lim=window$xlim, view.bounds = view.bounds, tick.len = tick.len, axis.label=window$xlab)
  
}