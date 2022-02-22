get_genotype <- function(k,n,pos=NULL) {
  if (is.null(k)) {
    return(1:n)
  }
  if(is.null(pos)){
    ## For given number k, get the range k surrounding n/2
    ## but have to make sure it does not go over the bounds
    start = floor(n/2 - k/2)
    end = ceiling(n/2 + k/2)
  }else{
    start = floor(pos - k/2)
    end = ceiling(pos + k/2)
  }
  if (start<1) start = 1
  if (end>n) end = n
  return(start:end)
}

center_scale <- function(X){
  out = susieR:::compute_colstats(as.matrix(X), center=TRUE, scale = TRUE)
  return(t( (t(X) - out$cm) / out$csd  ))
}
