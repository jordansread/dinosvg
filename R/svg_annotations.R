

#' @importFrom XML newXMLTextNode
add_css <- function(svg, css.text){
  if (missing(css.text) || is.null(css.text)){
    css.text <- 
      '\n.shown, .hidden {
    \t-webkit-transition: opacity 0.2s ease-in-out;
    \t-moz-transition: opacity 0.2s ease-in-out;
    \t-o-transition: opacity 0.2s ease-in-out;
    \ttransition: opacity 0.2s ease-in-out;
  }
    .hidden {
    \topacity:0;
    }\n'
  }
  
  svg_node("style", svg, attrs=NULL, newXMLTextNode(css.text))
  
}

#' @importFrom XML newXMLCDataNode
add_ecmascript <- function(svg, ecmascript.text){
  if (missing(ecmascript.text) || is.null(ecmascript.text)){
    invisible(svg)
  } else {
    svg_node("script", svg, attrs=c(type="text/ecmascript"), newXMLCDataNode(ecmascript.text))
  }
}
  
#' @importFrom XML newXMLTextNode
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
  add_ecmascript(svg, ecmascript.text = tool_fun)
  svg_node("script", svg, attrs=c(type="text/ecmascript"), newXMLCDataNode(tool_fun))
}