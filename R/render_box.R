render_box <- function(svg, object, ...){
  x = svglite::xmlSVG({
    plot.new()
    box()
  }, ...)
  
  box.ele <- xml2::xml_find_all(x, '//rect[@width="100%"]/following-sibling::*')
  g.frame <- xml_add_child(svg, 'g', id='frame')
  lapply(box.ele, xml_add_child, .x=g.frame)
  
}