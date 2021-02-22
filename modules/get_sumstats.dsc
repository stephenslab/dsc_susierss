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

