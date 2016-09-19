#' @export
render_side <- function(svg, object, side.name){
  side <- object[[side.name]]
  side.num <- gsplot:::as.side(side.name)
  lim <- lim(object, side = side.num)
  g.side <- svg_node('g', svg, c('id'=sprintf('side-%s', side.num)))
  g.view <- g_view(svg, gsplot:::set_sides(side.num)) # hack! this is going to break. We need to look for the other side to get dimensions for the axis
  view.bounds <- view_bounds(g.view)
  render_axis(g.side, side=side.num, at=side$axis$at, labels=side$axis$labels, lim=lim, view.bounds = view.bounds, tick.len = 5, axis.label=side$label, las=side$axis$las, id=side$axis$id)
  
}