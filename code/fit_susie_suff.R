library(susieR)

susie_suff_analyze = function(bhat, shat, R, n, L, s_init, estimate_residual_variance, refine) {
  fit = tryCatch(susie_suff_stat(bhat, shat, R, n=n,
                            L=L, s_init=s_init,
                            estimate_residual_variance = estimate_residual_variance,
                            max_iter = 200, refine = refine),
                 error = function(e) list(sets = NULL, pip=NULL))
  return(fit)
}

susie_suff_multiple = function(Bhat,Shat,R, n, L, s_init, estimate_residual_variance, refine) {
  fitted = list()
  posterior = list()
  if (is.null(dim(Bhat))) Bhat = matrix(ncol=1, Bhat)
  if (is.null(dim(Shat))) Shat = matrix(ncol=1, Shat)
  for (r in 1:ncol(Bhat)) {
    if (is.na(s_init))
      fitted[[r]] = susie_suff_analyze(Bhat[,r],Shat[,r], R, n,
                                       L=L, NULL,
                                       estimate_residual_variance, refine)
    else
      fitted[[r]] = susie_suff_analyze(Bhat[,r],Shat[,r], R, n,
                                       L=L, s_init[[r]],
                                       estimate_residual_variance, refine)
    if(is.null(fitted[[r]]$sets))
      posterior[[r]] = NULL
    else
      posterior[[r]] = summary(fitted[[r]])
  }
  return(list(fitted=fitted, posterior=posterior))
}

