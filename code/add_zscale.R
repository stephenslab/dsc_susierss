library(data.table);
z = as.matrix(sumstats$bhat / sumstats$shat);
if(addz == TRUE && (ld_method == 'refin_sample' || ld_method == 'refout_sample') ){
  r = as.matrix(fread(ld[[ld_method]]));
  if (is.null(N_ref)) stop("Cannot use add_z out sample LD when N_ref is not available (NULL)")
  if(scalez == 'max'){
    r = (r*(N_ref-1) + tcrossprod(z/max(abs(z))))/N_ref;
  }else if(scalez == 'ratio'){
    r = (r*(N_ref-1) + tcrossprod(z)*N_ref/N_in)/N_ref;
  }else{
    r = (r*(N_ref-1) + tcrossprod(z))/N_ref;
  }
  if(rcor){
    r = cov2cor(r)
  }
  r = (r + t(r))/2;
  write.table(r,ld_ref_z_file,quote=F,col.names=F,row.names=F);
  ld_file = ld_ref_z_file;
}else{
  ld_file = ld[[ld_method]]
}
