sim_gaussian: simulate.R + \
                R(set.seed(seed);
                res=sim_gaussian_multiple(X, pve, n_signal, n_traits, file_name, sample_file))
  @CONF: R_libs = susieR
  seed: $seed
  X: $X_sample
  pve: 0.005
  n_signal: 1,2,3
  n_traits: 1
  file_name: file(pheno)
  sample_file: $sample_file
  $Y: res$Y
  $meta: res$meta
  $pheno_file: file_name
  


