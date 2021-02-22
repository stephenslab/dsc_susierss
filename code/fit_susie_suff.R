library(susieR)

susie_suff_analyze = function(bhat, shat, R, n, L, s_init, estimate_residual_variance) {
  fit = tryCatch(susie_suff_stat(bhat, shat, R, n=n,
                            L=L, s_init=s_init,
                            estimate_residual_variance = estimate_residual_variance,
                            max_iter = 200),
                 error = function(e) list(sets = NULL, pip=NULL))
  return(fit)
}

susie_suff_multiple = function(Bhat,Shat,R, n, L, s_init, estimate_residual_variance) {
  fitted = list()
  posterior = list()
  if (is.null(dim(Bhat))) Bhat = matrix(ncol=1, Bhat)
  if (is.null(dim(Shat))) Shat = matrix(ncol=1, Shat)
  for (r in 1:ncol(Bhat)) {
    if (is.na(s_init))
      fitted[[r]] = susie_suff_analyze(Bhat[,r],Shat[,r], R, n,
                                       L=L, NULL,
                                       estimate_residual_variance)
    else
      fitted[[r]] = susie_suff_analyze(Bhat[,r],Shat[,r], R, n,
                                       L=L, s_init[[r]],
                                       estimate_residual_variance)
    if(is.null(fitted[[r]]$sets))
      posterior[[r]] = NULL
    else
      posterior[[r]] = summary(fitted[[r]])
  }
  return(list(fitted=fitted, posterior=posterior))
}

library(data.table)
z = sumstats$bhat/sumstats$shat;
r = as.matrix(fread(ld[[ld_method]]));
if(add_z){
  if(ld_method == 'refin_sample' || ld_method == 'refout_sample'){
    if (is.null(N_ref)) stop("Cannot use add_z out sample LD when N_out is not available (NULL)")
    r = cov2cor(r*(N_ref-1) + tcrossprod(z));
    r = (r + t(r))/2;
  }else{
    r = cov2cor(r*(n-1) + tcrossprod(z));
    r = (r + t(r))/2;
  }
}
res = susie_suff_multiple(sumstats$bhat, sumstats$shat, r, n, L, s_init, estimate_residual_variance)
