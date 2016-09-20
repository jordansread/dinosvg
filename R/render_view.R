#' @export
#' @importFrom xml2 xml_attr<- xml_find_all xml_find_first
render_view <- function(svg, object, view.name, ...){
  view <- object[[view.name]]
  geoms <- view
  geoms[['par']] <- NULL
  old.par <- par(view[['par']]) # need side par too!
  x = svglite::xmlSVG({
    plot.new()
    gsplot:::set_frame(object, view.name)
    gsplot:::print.view(view)
  }, ...)
  clip.path <- xml2::xml_find_first(x, '//defs/clipPath')
  
  
  mask.id <- paste(c('mask', strsplit(view.name,'[.]')[[1]][2:3]), collapse ='-')
  
  xml_attr(clip.path, 'id') <- mask.id 
  xml_add_child(xml2::xml_find_first(svg, 'defs'), clip.path)
  g.view <- xml_add_child(svg, 'g', id=as.svg_id (view.name), 'clip-path'= sprintf("url(#%s)", mask.id))
  geoms <- xml2::xml_find_all(clip.path, '//defs/clipPath/parent::*[1]/following-sibling::*')
  lapply(geoms, function(x){
      xml_attr(x, 'clip-path') <- NULL
      xml_add_child(.x=g.view, x)
    })
  par(old.par)
  invisible(svg)
}

as.svg_id <- function(x){
  gsub('[.]','-', x)
}
