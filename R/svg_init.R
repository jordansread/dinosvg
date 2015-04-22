#'@importFrom XML xmlParse xmlRoot
#'@export
svg_init <- function(fig, def_opacity){
  doc <- xmlParse(paste0('<?xml version="1.0" encoding="UTF-8"?>
                         <svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" version="1.1" onload="init(evt)" width="',
                         fig$w,'" height="',fig$h,'">
                         <style>
                         
                         text{
                         font-size: 16px;
                         cursor: default;
                         font-family: Tahoma, Geneva, sans-serif;
                         }
                         .dater{
                         font-size: 21px;
                         }
                         .tooltip{
                         font-size: 12px;
                         }
                         .caption{
                         font-size: 12px;
                         font-family: Tahoma, Geneva, sans-serif;
                         }
                         </style>
                         
                         <script type="text/ecmascript">
                         <![CDATA[
                         
                         function init(evt)
{
                         if ( window.svgDocument == null )
{
                         svgDocument = evt.target.ownerDocument;
                         svgDocument.timeAdvance = this.timeAdvance;
}
                         
                         tooltip = svgDocument.getElementById("tooltip");
                         tooltip_bg = svgDocument.getElementById("tooltip_bg");
}
                         
                         function ShowTooltip(evt, mouseovertext)
{
                         tooltip.setAttributeNS(null,"x",evt.clientX+8);
                         tooltip.setAttributeNS(null,"y",evt.clientY+4);
                         tooltip.firstChild.data = mouseovertext;
                         tooltip.setAttributeNS(null,"visibility","visible");
                         length = tooltip.getComputedTextLength();
                         tooltip_bg.setAttributeNS(null,"width",length+16);
                         tooltip_bg.setAttributeNS(null,"y",evt.clientY-16);
                         tooltip_bg.setAttributeNS(null,"x",evt.clientX+1);
                         tooltip_bg.setAttributeNS(null,"visibility","visible");
}
                         
                         function HideTooltip(evt)
{
                         tooltip.setAttributeNS(null,"visibility","hidden");
                         tooltip_bg.setAttributeNS(null,"visibility","hidden");
                         tooltip.setAttributeNS(null,"transition","1s");
                         tooltip_bg.setAttributeNS(null,"transition","1s");
}   
                         function MakeTransparent(evt) {
                         evt.target.setAttributeNS(null,"fill-opacity","0.5");
                         evt.target.setAttributeNS(null,"transition","1s");
                         }
                         
                         function MakeOpaque(evt) {
                         evt.target.setAttributeNS(null,"fill-opacity","1");
                         evt.target.setAttributeNS(null,"transition","1s");
                         }
                         function timeAdvance(){
  							          var ele = document.getElementById("timeAdvance");
								          ele.beginElement();
							           }
                         ]]>
                         </script></svg>'))
  
  root_nd <- xmlRoot(doc)
  
  g_id <- newXMLNode("g",parent=root_nd, attrs = c(id="surface0"))

  
  return(g_id)
}