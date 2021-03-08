library(data.table)
z = sumstats$bhat/sumstats$shat;
r = as.matrix(fread(ld[[ld_method]]));

if(add_z){
  if(ld_method == 'refin_sample' || ld_method == 'refout_sample'){
    if (is.null(N_ref)) stop("Cannot use add_z out sample LD when N_out is not available (NULL)")
    r = (r*(N_ref-1) + tcrossprod(z))/N_ref;
    if(rcor){
      r = cov2cor(r);
    }
    r = (r + t(r))/2;
  }else{
    r = (r*(n-1) + tcrossprod(z))/n;
    if(rcor){
      r = cov2cor(r);
    }
    r = (r + t(r))/2;
  }
}

if(fullrank){
  eigenr = eigen(r, symmetric = T, only.values = TRUE)
  if(min(eigenr$values) < 1e-8){
    r = r + 1e-4 * diag(length(z))
  }
}

