library(abind)
mm_regression = function(X, Y, Z=NULL) {
  if (!is.null(Z)) {
      Z = as.matrix(Z)
  }
  reg = lapply(seq_len(ncol(Y)), function (i) simplify2array(susieR:::univariate_regression(X, Y[,i], Z)))
  reg = do.call(abind, c(reg, list(along=0)))
  # return array: out[1,,] is betahat, out[2,,] is shat
  out = aperm(reg, c(3,2,1))
  return(list(bhat = out[1,,], shat=out[2,,]))
}

mixed_regression = function(X_file, Y_file, sample_file, snp_file, n_trait, prefix='gcta') {
  bhat = c()
  shat = c()
  for(r in 1:n_trait){
    cmd1 = paste0("code/linux/gcta64", " --pfile ", X_file, " --keep ", sample_file,
                  " --extract ", snp_file, " --make-grm --sparse-cutoff 0.05 --threads 10 --out ", Y_file,"_sp_grm")
    dscrutils::run_cmd(cmd1)
    cmd2 = paste0("code/linux/gcta64", " --pfile ", X_file, " --pheno ", Y_file,r, 
                 " --keep ", sample_file, " --extract ", snp_file,
                 " --grm-sparse ", Y_file, "_sp_grm", " --fastGWA-mlm --nofilter --threads 10 --out ", Y_file)
    dscrutils::run_cmd(cmd2)
    out = fread(paste0(Y_file, '.fastGWA'))
    bhat = cbind(bhat, out$BETA)
    shat = cbind(shat, out$SE)
  }
  return(list(bhat = bhat, shat=shat))
}

if(method == 'lm'){
  res = mm_regression(as.matrix(X), as.matrix(Y))
}else if(method == 'mixed'){
  res = mixed_regression(X_file, Y_file, sample_file, snp_file, n_trait)
}
