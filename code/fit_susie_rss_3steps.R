library(susieR)

susie_rss_3steps = function(z, R, L, z_ld_weight, estimate_residual_variance){
  f = susie_rss(z, R, L=L, z_ld_weight = z_ld_weight, estimate_residual_variance = estimate_residual_variance,
                max_iter = 2000, check_z = FALSE)
  check = TRUE
  while(check){
    m = list()
    for(cs in 1:length(f$sets$cs)){
      pw = rep(1, length(z))
      pw[f$sets$cs[[cs]]] = 0
      fs2 = susie_rss(z, R, L=L,z_ld_weight = z_ld_weight, 
                      estimate_residual_variance = estimate_residual_variance,
                      max_iter = 2000, check_z = FALSE, prior_weights = pw)
      sinit2 = fs2[c('alpha', 'mu', 'mu2')]
      class(sinit2) = 'susie'
      fs3 = susie_rss(z, R, L=L,z_ld_weight = z_ld_weight, 
                      estimate_residual_variance = estimate_residual_variance,
                      max_iter = 2000, check_z = FALSE, s_init = sinit2)
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

susie_rss_3steps_analyze = function(z, R, L, z_ld_weight, estimate_residual_variance) {
  fit = tryCatch(susie_rss_3steps(z, R, L, z_ld_weight,estimate_residual_variance),
                 error = function(e) list(sets = NULL, pip=NULL))
  return(fit)
}

susie_rss_3steps_multiple = function(Z, R, L, z_ld_weight, estimate_residual_variance) {
  fitted = list()
  posterior = list()
  if (is.null(dim(Z))) Z = matrix(ncol=1, Z)
  for (r in 1:ncol(Z)) {
    fitted[[r]] = susie_rss_3steps_analyze(Z[,r], R, L,z_ld_weight,
                                          estimate_residual_variance)
    if(is.null(fitted[[r]]$sets))
      posterior[[r]] = NULL
    else
      posterior[[r]] = summary(fitted[[r]])
  }
  return(list(fitted=fitted, posterior=posterior))
}

