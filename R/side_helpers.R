g_side <- function(svg, side){
  xpath_one(svg, sprintf("//*[local-name()='g'][@id='axis-side-%s']", side))
}

tick_location <- function(g.view, reference='x', side=NULL){
  
  stopifnot(is.null(side)) # not supported yet, but is here to explicity call the side arg
  
  tick.i <- c('x'=2,'y'=3) # for how to parse the path
  
  sides <- tail(strsplit(xmlAttrs(g.view)[['id']],'[-]')[[1]],-1L) %>% 
    setNames(c('x','y'))
  side <- sides[[reference]]
  
  nodes <- xpathApply(g_side(g.view, side), path = "//*[local-name()='g'][@id='ticks']/*[local-name()='path']")
  unname(sapply(nodes, function(x)strsplit(xmlAttrs(x), '[, ]')[[1]][tick.i[[reference]]]))
}
