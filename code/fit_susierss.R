library(susieR)

susie_rss_analyze = function(z, R, L, s_init, refine) {
  fit = tryCatch(susieR::susie_rss(z, R, L=L, s_init = s_init, refine=refine, max_iter = 1000),
                 error = function(e) list(sets = NULL, pip=NULL, s2 = NA))
  return(fit)
}

susie_rss_multiple = function(Z, R, L, s_init, refine) {
  fitted = list()
  posterior = list()
  if (is.null(dim(Z))) Z = matrix(ncol=1, Z)
  for (r in 1:ncol(Z)) {
    if (is.na(s_init))
      fitted[[r]] = susie_rss_analyze(Z[,r], R, L, NULL,refine)
    else
      fitted[[r]] = susie_rss_analyze(Z[,r], R, L, s_init[[r]],refine)
    if(is.null(fitted[[r]]$sets))
      posterior[[r]] = NULL
    else
      posterior[[r]] = summary(fitted[[r]])
  }
  return(list(fitted=fitted, posterior=posterior))
}

