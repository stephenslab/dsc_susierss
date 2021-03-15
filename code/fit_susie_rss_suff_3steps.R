library(susieR)

susie_rsssuff_3steps = function(z, R, L){
  f = susie_suff_stat(XtX = R, Xty = z, n = length(z), yty = length(z)-1, residual_variance = 1,
                      standardize = F, L=L, max_iter = 2000, estimate_residual_variance=FALSE)
  check = TRUE
  while(check){
    m = list()
    for(cs in 1:length(f$sets$cs)){
      pw = rep(1, length(z))
      pw[f$sets$cs[[cs]]] = 0
      fs2 = susie_suff_stat(XtX = R, Xty = z, n = length(z), yty = length(z)-1, residual_variance = 1,
                            standardize = F, L=L, max_iter = 2000, estimate_residual_variance=FALSE, prior_weights = pw)
      sinit2 = fs2[c('alpha', 'mu', 'mu2')]
      class(sinit2) = 'susie'
      fs3 = susie_suff_stat(XtX = R, Xty = z, n = length(z), yty = length(z)-1, residual_variance = 1,
                            standardize = F, L=L, max_iter = 2000, estimate_residual_variance=FALSE, s_init = sinit2)
      m = c(m, list(fs3))
    }
    elbo = sapply(m, function(x) susie_get_objective(x))
    if((max(elbo) - susie_get_objective(f)) <= 0){
      check=FALSE
    }else{
      f = m[[which.max(elbo)]]
    }
  }
  return(f)
}

susie_rsssuff_3steps_analyze = function(z, R, L) {
  fit = tryCatch(susie_rsssuff_3steps(z, R, L),
                 error = function(e) list(sets = NULL, pip=NULL, s2 = NA))
  return(fit)
}

susie_rsssuff_3steps_multiple = function(Z, R, L) {
  fitted = list()
  posterior = list()
  if (is.null(dim(Z))) Z = matrix(ncol=1, Z)
  for (r in 1:ncol(Z)) {
    fitted[[r]] = susie_rsssuff_3steps_analyze(Z[,r], R, L)
    if(is.null(fitted[[r]]$sets))
      posterior[[r]] = NULL
    else
      posterior[[r]] = summary(fitted[[r]])
  }
  return(list(fitted=fitted, posterior=posterior))
}

