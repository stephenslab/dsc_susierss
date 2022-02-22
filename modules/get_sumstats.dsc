# Modules to compute summary statistics

get_sumstats: regression.R
  @CONF: R_libs = (abind, data.table)
  method: 'lm'
  X: $X_sample
  Y: $Y
  X_file: $X_file
  Y_file: $pheno_file
  sample_file: $sample_file
  snp_file: $snp_file
  n_trait: ncol(Y)
  $sumstats: res

get_sumstats_n: regression.R + R(ld = cor(as.matrix(X));
                                write.table(ld, ld_sample_n_file, quote=F, col.names=F, row.names=F))
  @CONF: R_libs = (abind, data.table)
  method: 'lm'
  X: $X_sample_n
  Y: $Y
  X_file: $X_file
  Y_file: $pheno_file
  sample_file: $sample_file
  snp_file: $snp_file
  n_trait: ncol(Y)
  ld_sample_n_file: file(in_n.ld)
  $sumstats: res
  $ld_n: c(ld_sample_n_file)
  
