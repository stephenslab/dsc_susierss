library(susieR)

susie_suff_3steps = function(bhat, shat, R, n, L, estimate_residual_variance){
  f = susie_suff_stat(bhat, shat, R, n=n, L=L,
                      estimate_residual_variance = estimate_residual_variance,
                      max_iter = 200)
  check = TRUE
  while(check){
    m = list()
    for(cs in 1:length(f$sets$cs)){
      pw = rep(1, length(bhat))
      pw[f$sets$cs[[cs]]] = 0
      fs2 = susie_suff_stat(bhat, shat, R, n=n, L=L,
                            estimate_residual_variance = estimate_residual_variance,
                            max_iter = 200, prior_weights = pw)
      sinit2 = fs2[c('alpha', 'mu', 'mu2')]
      class(sinit2) = 'susie'
      fs3 = susie_suff_stat(bhat, shat, R, n=n, L=L,
                            estimate_residual_variance = estimate_residual_variance,
                            max_iter = 200, s_init = sinit2)
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

susie_suff_3steps_analyze = function(bhat, shat, R, n, L, estimate_residual_variance) {
  
  fit = tryCatch(susie_suff_3steps(bhat, shat, R, n, L,
                                 estimate_residual_variance = estimate_residual_variance),
                 error = function(e) list(sets = NULL, pip=NULL))
  return(fit)
}

susie_suff_3steps_multiple = function(Bhat,Shat,R, n, L, estimate_residual_variance) {
  fitted = list()
  posterior = list()
  if (is.null(dim(Bhat))) Bhat = matrix(ncol=1, Bhat)
  if (is.null(dim(Shat))) Shat = matrix(ncol=1, Shat)
  for (r in 1:ncol(Bhat)) {
    fitted[[r]] = susie_suff_3steps_analyze(Bhat[,r],Shat[,r], R, n,
                                           L=L, 
                                           estimate_residual_variance)
    if(is.null(fitted[[r]]$sets))
      posterior[[r]] = NULL
    else
      posterior[[r]] = summary(fitted[[r]])
  }
  return(list(fitted=fitted, posterior=posterior))
}

