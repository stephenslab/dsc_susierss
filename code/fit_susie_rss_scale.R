library(susieR)
library(data.table)

z = as.matrix(sumstats$bhat/sumstats$shat);
r = as.matrix(fread(ld[[ld_method]]));

susie_rss_analyze = function(z, R, L, z_ld_weight, s_init, estimate_residual_variance) {
  fit = tryCatch(susie_rss(z, R, L=L,
                           z_ld_weight = z_ld_weight,
                           s_init=s_init,
                           estimate_residual_variance = estimate_residual_variance,
                           check_z = FALSE,
                           max_iter = 2000),
                 error = function(e) list(sets = NULL, pip=NULL))
  return(fit)
}

susie_rss_multiple = function(Z, R, L, z_ld_weight, s_init, estimate_residual_variance) {
  fitted = list()
  posterior = list()
  if (is.null(dim(Z))) Z = matrix(ncol=1, Z)
  for (r in 1:ncol(Z)) {
    if (is.na(s_init))
      fitted[[r]] = susie_rss_analyze(Z[,r], R, L,z_ld_weight,
                                      NULL,
                                      estimate_residual_variance)
    else
      fitted[[r]] = susie_rss_analyze(Z[,r], R, L,z_ld_weight,
                                      s_init[[r]],
                                      estimate_residual_variance)
    if(is.null(fitted[[r]]$sets))
      posterior[[r]] = NULL
    else
      posterior[[r]] = summary(fitted[[r]])
  }
  return(list(fitted=fitted, posterior=posterior))
}

library(data.table);
z = as.matrix(sumstats$bhat/sumstats$shat);
r = as.matrix(fread(ld[[ld_method]]));
if(!is.na(z_ld_weight)){
  res = susie_rss_multiple(z, r, L, z_ld_weight, s_init, estimate_residual_variance)
}else{
  if(add_z){
    if(ld_method == 'refin_sample' || ld_method == 'refout_sample'){
      if (is.null(N_ref)) stop("Cannot use add_z out sample LD when N_out is not available (NULL)")
      if(scalez == 'max'){
        r = (r*(N_ref-1) + tcrossprod(z/max(abs(z))))/N_ref;
      }else if(scalez == 'ratio'){
        r = (r*(N_ref-1) + tcrossprod(z)*N_ref/n)/N_ref;
      }else{
        r = (r*(N_ref-1) + tcrossprod(z))/N_ref;
      }
      if(rcor){
        r = cov2cor(r)
      }
      r = (r + t(r))/2;
    }else{
      if(scalez == 'max'){
        r = (r*(n-1) + tcrossprod(z/max(abs(z))))/n;
      }else{
        r = (r*(n-1) + tcrossprod(z))/n;
      }
      if(rcor){
        r = cov2cor(r);
      }
      r = (r + t(r))/2;
    }
  }
  res = susie_rss_multiple(z, r, L, 0, s_init, estimate_residual_variance)
}


