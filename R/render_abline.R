#' create abline svg elements
#' 
#' creates abline elements using the syntax of \code{\link[graphics]{abline}}
#' 
#' @examples 
#' \dontrun{
#' library(gsplot)
#' gs <- gsplot() %>% 
#'    points(y=1:11, x=1:11, 
#'             col="blue", pch=18, xlab='pizza', ylab='dogs') %>% 
#'    abline(h=1:4, v=5:8)
#' svg(gs)
#' }
#' @export
render_abline <- function(g.view, a = NULL, b = NULL, h = NULL, v = NULL, reg = NULL, coef = NULL, untf = FALSE, 
                          lty=par("lty"), col=par("col"), lwd=par("lwd"), xlim, ylim, ...){
  
  stopifnot(is.null(reg), is.null(coef), !untf)
  args <- filter_dot_args(...)
  view.bounds <- view_bounds(g.view)
  clip.id <- svg_id(g_mask(g.view))
  g.geom <- svg_node('g', g.view, c('stroke'=as.rgb(col), as.lty(lty), 'clip-path'=sprintf("url(#%s)",clip.id), g_args(args)))
  
  if (!is.null(v)){
    
    x <- svg_coords(x=v,xlim=xlim, view.bounds = view.bounds)$x
    for (i in seq_len(length(x))){
      svg_node('path', g.geom, c(d=sprintf('M %s,%s v %s',x[i], view.bounds[['y']], view.bounds[['height']]), nd_args(args,i)))
    }
  } 
  if (!is.null(h)){
    
    y <- svg_coords(y=h,ylim=ylim, view.bounds = view.bounds)$y
    for (i in seq_len(length(y))){
      svg_node('path', g.geom, c(d=sprintf('M %s,%s h %s',view.bounds[['x']], y[i], view.bounds[['width']]), nd_args(args,i)))
    }
  }
  
  if (!is.null(a) && !is.null(b)){
    # // to do: check that a and b are length==1
    y <- a+b*xlim
    coords <- svg_coords(x=xlim, y, xlim, ylim, view.bounds)
    svg_node('path', g.geom, c(d=sprintf('M %s,%s L %s,%s', coords$x[1],coords$y[1],coords$x[2],coords$y[2]), nd_args(args,i)))
  }
  
}
