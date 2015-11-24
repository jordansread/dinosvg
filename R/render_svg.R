#' render svg
#' 
#' @examples 
#' \dontrun{
#' library(gsplot)
#' gs <- gsplot() %>% 
#'    points(y=1:10, x=2:11, 
#'             col="blue", pch=18, hovertext=paste0('text:',1:10)) %>% 
#'    points(x=4:11, y=11:4, 
#'             col="red", pch=1, hovertext=paste0('text:',11:4)) %>% 
#'    points(3:5,4:6,side=c(1,4), col='green')
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
  
  g.view <- render_window(svg, window)

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
  
  
  g.axes <- xpathApply(g.view, "//*[local-name()='g'][@id='axes']")[[1]]
  view.bounds <- view_bounds(g.view)
  x.axis <- svg_node('g', g.axes, c(id='axis-side-1'))
  y.axis <- svg_node('g', g.axes, c(id='axis-side-2'))
  
  tick.len <- 5
  
  render_axis(x.axis, window[['side']][1], lim=window$xlim, view.bounds = view.bounds, tick.len = tick.len)
  render_axis(y.axis, window[['side']][2], lim=window$xlim, view.bounds = view.bounds, tick.len = tick.len)
  
}


render_points <- function(g.view, x, y, pch=par("pch"), col=par("col"), bg="#FFFFFF00", cex=1, lwd=par("lwd"), xlim, ylim, hovertext=NULL, ...){
  
  args <- expand.grid(..., stringsAsFactors = FALSE)
  view.bounds <- view_bounds(g.view)
  radius <- as.crd(cex*2.7) # need to use ppi?
  coords <- svg_coords(x, y, xlim, ylim, view.bounds)

  clip.id <- svg_id(view_mask(g.view))
  g.geom <- svg_node('g', g.view, c('fill'=col, 'clip-path'=sprintf("url(#%s)",clip.id), args))
  
  for (i in seq_len(length(coords$x))){
    if (!is.null(hovertext)){
      hover.args <- c(onmouseover=sprintf("hovertext('%s',%s,%s)",hovertext[i],coords$x[i],coords$y[i]), onmouseout="hovertext(' ')") 
    } else 
      hover.args <- NULL
    svg_node('circle', g.geom, c(cx=coords$x[i], cy=coords$y[i], r=radius, hover.args))
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
    box.node <- xpathApply(svg, "//*[local-name()='rect'][@id='axes-box']")
  } else {
    box.node <- xpathApply(svg, sprintf("//*[local-name()='g'][@id='view-%s-%s']//*[local-name()='rect'][@id='axes-box']", side[1], side[2]))
  }
  
  sapply(xmlAttrs(box.node[[1]])[c('x','y','height','width')], as.numeric)
}

view_mask <- function(svg, side=NULL){
  if (is.null(side)){
    xpathApply(svg, "//*[local-name()='clipPath'][contains(@id,'mask')]")[[1]]
  } else {
    xpathApply(svg, sprintf("//*[local-name()='g'][@id='view-%s-%s']//*[local-name()='clipPath'][contains(@id,'mask')]", side[1], side[2]))[[1]]
  }
}

#' @importFrom XML newXMLTextNode
render_window <- function(svg, window){

  ax <- svg_view_bounds(svg, mai=par()$mai)
  
  g.view <- svg_node('g', svg, c('id'=sprintf('view-%s-%s', window$side[1], window$side[2])))
  svg_node('rect', svg_node('clipPath',svg_node('defs',g.view), c(id=sprintf('mask-%s-%s', window$side[1], window$side[2]))), c(x=ax[['x']], y=ax[['y']], height=ax[['height']], width=ax[['width']]))
  g.axes <- svg_node('g', g.view, c('id'="axes", 'fill'="none", 'stroke'="#000000", 'stroke-width'="1"))
  
  svg_node('rect', g.axes, c(x=ax[['x']], y=ax[['y']], height=ax[['height']], width=ax[['width']], id='axes-box'))
  
  invisible(g.view)
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