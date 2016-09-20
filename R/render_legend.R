render_legend <- function(svg, object, ...){
  x = svglite::xmlSVG({
    plot.new()
    gsplot:::draw_legend(object)
  }, ...)
  
  legend.ele <- xml2::xml_find_all(x, '//rect[@width="100%"]/following-sibling::*')
  if (length(legend.ele) > 0){
    g.leg <- xml_add_child(svg, 'g', id='legend')
    lapply(legend.ele, xml_add_child, .x=g.leg)
  }
  
}