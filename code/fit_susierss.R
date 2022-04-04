library(susieR)

susie_rss_analyze = function(z, R, n, L, s_init, estimate_residual_variance, refine) {
  fit = tryCatch(susieR::susie_rss(z, R, n=n, L=L, s_init = s_init, refine=refine, 
                                   estimate_residual_variance = estimate_residual_variance,
                                   max_iter = 1000, check_prior = FALSE),
                 error = function(e) list(sets = NULL, pip=NULL, s2 = NA))
  return(fit)
}

susie_rss_multiple = function(Z, R, n, L, s_init, estimate_residual_variance, refine) {
  fitted = list()
  posterior = list()
  if (is.null(dim(Z))) Z = matrix(ncol=1, Z)
  for (r in 1:ncol(Z)) {
    if (is.na(s_init))
      fitted[[r]] = susie_rss_analyze(Z[,r], R, n, L, NULL,estimate_residual_variance,refine)
    else
      fitted[[r]] = susie_rss_analyze(Z[,r], R, n, L, s_init[[r]],estimate_residual_variance,refine)
    if(is.null(fitted[[r]]$sets))
      posterior[[r]] = NULL
    else
      posterior[[r]] = summary(fitted[[r]])
  }
  return(list(fitted=fitted, posterior=posterior))
}

