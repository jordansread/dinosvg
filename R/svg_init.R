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
                         
                         legend = svgDocument.getElementById("legend");
}
                         function legendViz(evt,elementname)
                         {
                          var r = document.getElementById(elementname).r.animVal.value

                          if (r === 0){
                            legend.setAttributeNS(null,"visibility","hidden");
                          } else {
                            legend.setAttributeNS(null,"visibility","visible");
                          }

}
                         function highlightViz(evt,elementname,opacity)
{
                          var r = document.getElementById(elementname).r.animVal.value
  
                          if (r === 0){
                            evt.target.setAttribute("fill-opacity", "0.0");
                          } else {
                            evt.target.setAttribute("fill-opacity", opacity);
                          }
                            
}

                         function ChangeText(evt, elementname, legendtext)
{
                         textelement = svgDocument.getElementById(elementname);                      
                         textelement.firstChild.data = legendtext;
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