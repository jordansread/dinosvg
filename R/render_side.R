#' @export
render_side <- function(svg, object, side.name, ...){
  
  side <- gsplot:::as.side(side.name)
  old.par <- par(object[[side.name]]$par)
  
  x = svglite::xmlSVG({
    plot.new()
    gsplot:::set_frame(object, side)
    if(object[[side.name]][['axes']] | object[[side.name]][['usr.axes']]){
      gsplot:::draw_axis(object, side.name)
    }
    if(par('ann')){
      #mtext(text=gsplot:::label(object, side), side=side, line = 2, las=config("mtext")$las)
    }
    par(old.par)
  }, ...)
  
  g.side <- xml_add_child(svg, 'g', id=as.svg_id(side.name))
  side.ele <- xml2::xml_find_all(x, '//rect[@width="100%"]/following-sibling::*')
  lapply(side.ele, xml_add_child, .x=g.side)

  invisible(svg)
  
}