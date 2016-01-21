
as.rgb <- function(col){
  paste0('rgb(',paste(as.vector(col2rgb(col)),collapse=','),')')
}

as.lty <- function(lty){
  # is numeric?
  ltys <- setNames(c("blank","solid","dashed","dotted","dotdash","longdash","twodash"),seq(0,6))
  if (is.numeric(lty))
    lty <- ltys[[as.character(lty)]]
  else
    lty <- match.arg(lty, as.vector(ltys))
  
  styles <- list("blank"=c('stroke'="none",'fill'="none"),
                 "solid"=c('fill'="none"),
                 "dashed"=c('stroke-dasharray'="10,10", 'fill'="none"),
                 "dotted"=c('stroke-dasharray'="5,5", 'fill'="none"),
                 "dotdash"=c('stroke-dasharray'="20,10,5,10", 'fill'="none"),
                 "longdash"=c('stroke-dasharray'="20,10", 'fill'="none"),
                 "twodash"=c('stroke-dasharray'="20,5,10,5", 'fill'="none"))
  return(styles[[lty]])
}


as.crd <- function(x){
  sprintf('%s',round(as.vector(x), digits = 3))
}
