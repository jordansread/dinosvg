#' render svg
#' 
#' @examples 
#' \dontrun{
#' library(gsplot)
#' gs <- gsplot() %>% 
#'    points(y=1:11, x=1:11, 
#'             col="blue", pch=18, hovertext=paste0('text:',1:11), xlab='pizza', ylab='dogs') %>% 
#'    points(x=4:11, y=11:4, 
#'             col="red", pch=1, hovertext=paste0('text:',11:4)) %>% 
#'    points(3:5,4:6,side=c(1,4), col='green', hovertext='green', pch=5) %>% 
#'    lines(2:5, c(2,2.6,2.3), col='blue')
#' svg(gs)
#' }
#' @export
#' @import gsplot
svg <- function(object, ...){
  UseMethod('svg')
}

#' @export
svg.gsplot <- function(object, file = "Rplot.svg", width = 6, height = 4.3, pointsize = 12, ...){

  svg <- init_svg(width, height, ...)
  add_css(svg)
  
  for (view in gsplot:::views(object)){
    par(par(object)) # set global par to object par
    render_view(svg, view)
  }
  add_tooltip(svg)
  # get par
  # set the page dimensions
  # translate coordinates
  # build axes
  # do.call for gsplot elements, skip those w/o `svg_` functions and warn
  # invisible return of filename
  return(write_svg(svg, file))
}

render_view <- function(svg, view){
  window <- view[['window']]
  geoms <- view
  geoms[['window']] <- NULL
  par(window[['par']])
  
  render_window(svg, window)
  g.view <- g_view(svg,window[['side']])
  render_geoms(g.view, geoms, window)
  
  g.axes <- g_axes(g.view)
  view.bounds <- view_bounds(g.view)
  
  tick.len <- 5
  
  render_axis(g.axes, window[['side']][1], lim=window$xlim, view.bounds = view.bounds, tick.len = tick.len, axis.label=window$xlab)
  render_axis(g.axes, window[['side']][2], lim=window$ylim, view.bounds = view.bounds, tick.len = tick.len, axis.label=window$ylab)
  
}
render_geoms <- function(g.view, geoms, window){
  for (i in seq_len(length(geoms))){
    fun_name <- paste0('render_',names(geoms[i]))
    if (exists(fun_name)){
      args <- append(list(g.view=g.view), geoms[[i]]) %>% 
        append(list(xlim=window[['xlim']], ylim=window[['ylim']]))
      do.call(fun_name, args)
    } else {
      message(fun_name, " doesn't exist in ", packageName())
    }
  }
}


#' @importFrom XML xmlAttrs
svg_id <- function(ele){
  xmlAttrs(ele)[[1]]
}

#' @importFrom XML xmlAttrs
svg_view_bounds <- function(svg, mai){
  
  ppi <- 72 # points per inch, this SHOULD BE A PKG VAR...
  mar <- mai*ppi
  view.box <- strsplit(xmlAttrs(svg)[['viewBox']],'[ ]')[[1]] %>% 
    as.numeric()
  
  c(x = view.box[1]+mar[2], y = view.box[2]+mar[2], 
         width = view.box[3]-mar[2]-mar[4], 
         height = view.box[4]-mar[1]-mar[3])
}
#' @importFrom XML xpathApply
view_bounds <- function(svg, side=NULL){
  if (is.null(side)){
    box.node <- xpath_one(svg, "//*[local-name()='rect'][@id='axes-box']")
  } else {
    box.node <- xpath_one(svg, sprintf("//*[local-name()='g'][@id='view-%s-%s']//*[local-name()='rect'][@id='axes-box']", side[1], side[2]))
  }
  
  sapply(xmlAttrs(box.node)[c('x','y','height','width')], as.numeric)
}

g_mask <- function(svg, side=NULL){
  if (is.null(side)){
    xpath_one(svg, "//*[local-name()='clipPath'][contains(@id,'mask')]")
  } else {
    xpath_one(svg, sprintf("//*[local-name()='g'][@id='view-%s-%s']//*[local-name()='clipPath'][contains(@id,'mask')]", side[1], side[2]))
  }
}

g_view <- function(svg, side=NULL){
  if (is.null(side))
    xpath_one(svg, "//*[local-name()='g'][contains(@id,'view-']")
  else
    xpath_one(svg, sprintf("//*[local-name()='g'][@id='view-%s-%s']", side[1], side[2]))
}

g_axes <- function(svg, side=NULL){
  if (is.null(side))
    xpath_one(svg, "//*[local-name()='g'][@id='axes']")
  else
    xpath_one(svg, sprintf("//*[local-name()='g'][@id='view-%s-%s']//*[local-name()='g'][@id='axes']", side[1], side[2]))
}

xpath_one <- function(svg, xpath){
  nodes <- xpathApply(svg, xpath)
  
  if (length(nodes) > 1)
    stop('more than one element found for ', xpath)
  return(nodes[[1]])
}

#' @importFrom XML newXMLTextNode
render_window <- function(svg, window){

  ax <- svg_view_bounds(svg, mai=par()$mai)
  
  g.view <- svg_node('g', svg, c('id'=sprintf('view-%s-%s', window$side[1], window$side[2])))
  svg_node('rect', svg_node('clipPath',svg_node('defs',g.view), c(id=sprintf('mask-%s-%s', window$side[1], window$side[2]))), c(x=ax[['x']], y=ax[['y']], height=ax[['height']], width=ax[['width']]))
  g.axes <- svg_node('g', g.view, c('id'="axes", 'fill'="none", 'stroke'="#000000", 'stroke-width'="1"))
  
  svg_node('rect', g.axes, c(x=ax[['x']], y=ax[['y']], height=ax[['height']], width=ax[['width']], id='axes-box'))
}


#' @importFrom XML newXMLNode
svg_node <- function(name, parent, attrs=NULL, ...){
  invisible(newXMLNode(name = name, parent = parent,
             attrs=attrs, ...))
}

as.crd <- function(x){
  sprintf('%s',round(as.vector(x), digits = 3))
}


#' @importFrom XML newXMLTextNode
add_css <- function(svg){
  css <- 
    '\n.shown, .hidden {
      \t-webkit-transition: opacity 0.2s ease-in-out;
      \t-moz-transition: opacity 0.2s ease-in-out;
      \t-o-transition: opacity 0.2s ease-in-out;
      \ttransition: opacity 0.2s ease-in-out;
    }
  .hidden {
    \topacity:0;
  }\n'
  svg_node("style", svg, attrs=NULL, newXMLTextNode(css))
  
}

#' @importFrom XML newXMLCDataNode newXMLTextNode
add_tooltip <- function(svg, dx="0.2em", dy='-0.2em',fill="#000000"){
  svg_node("text", svg, c(id='tooltip',dx=dx, dy=dy, 'stroke'="none", 'fill'=fill), newXMLTextNode(" "))
  tool_fun <- 
    '\nfunction hovertext(text, x, y){
  \tvar tooltip = document.getElementById("tooltip");
  \tif (x === undefined){
  \t\ttooltip.setAttribute("class","hidden");
  \t\ttooltip.setAttribute("x",0);
  \t\ttooltip.setAttribute("y",0);
  \t\ttooltip.firstChild.data = text;
  \t} else {
  \t\ttooltip.setAttribute("x",x);
  \t\ttooltip.setAttribute("y",y);
  \t\ttooltip.firstChild.data = text;
  \t\ttooltip.setAttribute("class","shown");
  \t}
  }'
  svg_node("script", svg, attrs=c(type="text/ecmascript"), newXMLCDataNode(tool_fun))
}