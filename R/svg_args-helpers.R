filter_dot_args <- function(...){
  args.out <- list(nd.args=list(), g.args=c())
  args <- list(...)
  if (length(args)){
    is.g <- which(unname(sapply(args, function(x) length(x)==1)))
    args.out$g.args <- do.call(c, args[is.g])
    
    if (is.null(args.out$g.args)){
      args.out$g.args <- list() # so that a NULL node isn't added. 
    }
    
    args[is.g] <- NULL
    if (length(args)){
      for (i in seq_len(length(args[[1]]))){
        args.out$nd.args[[i]] <- sapply(args, function(x) x[i])
      }
    } #// else no nd.args present
  }
  
  return(args.out)
}

as.path <- function(x,y){
  paste0('M ',paste(x,y,sep=',',collapse=' L'), 'Z')
}

nd_args <- function(args, i=NULL){
  if(length(args[['nd.args']]))
    args[['nd.args']][[i]]
  else
    args[['nd.args']]
}

g_args <- function(args){
  args[['g.args']]
}