library(susieR)

susie_rsssuff_analyze = function(z, R, L, s_init, estimate_residual_variance) {
  fit = tryCatch(susie_suff_stat(XtX = R, Xty = z, n = length(z), yty = length(z)-1, 
                                 residual_variance = 1, standardize = F, L=L,
                                 s_init=s_init, 
                                 estimate_residual_variance = estimate_residual_variance,
                                 max_iter = 2000),
                 error = function(e) list(sets = NULL, pip=NULL, s2 = NA))
  return(fit)
}

susie_rsssuff_multiple = function(Z, R, L, s_init, estimate_residual_variance) {
  fitted = list()
  posterior = list()
  if (is.null(dim(Z))) Z = matrix(ncol=1, Z)
  for (r in 1:ncol(Z)) {
    if (is.na(s_init)){
      fitted[[r]] = susie_rsssuff_analyze(Z[,r], R, L,
                                           NULL,estimate_residual_variance)
    }else{
      fitted[[r]] = susie_rsssuff_analyze(Z[,r], R, L,
                                           s_init[[r]],estimate_residual_variance)
    }
    if(is.null(fitted[[r]]$sets))
      posterior[[r]] = NULL
    else
      posterior[[r]] = summary(fitted[[r]])
  }
  return(list(fitted=fitted, posterior=posterior))
}

