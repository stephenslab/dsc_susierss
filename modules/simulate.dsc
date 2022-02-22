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

sim_gaussian_pve(sim_gaussian):
  pve: 0.02, 0.1, 0.3

sim_gaussian_pve_n: sim_utils.R + simulate.R + \
                      R(set.seed(seed);
                      res=sim_gaussian_n_multiple(X, pve, n_signal, n_traits, file_name, sample_file))
  @CONF: R_libs = susieR
  seed: $seed
  X: $X_sample
  pve: 0.005, 0.02, 0.1, 0.3
  n_signal: 1,2,3
  n_traits: 1
  file_name: file(pheno)
  sample_file: $sample_file
  $Y: res$Y
  $X_sample_n: res$X
  $N_sample_n: nrow(res$X)
  $meta: res$meta
  $pheno_file: file_name
  
base_sim: lib_regression_simulator.py + \
                regression_simulator.py + \
                Python(res = simulate_main(dict(X=X,Y=Y), conf, conf['cache']))
  @CONF: python_modules = (seaborn, matplotlib, pprint)
  X: $X_sample
  Y: $Y
  n_signal: 3
  n_traits: 2
  amplitude: 1
  pve: 0.05, 0.1, 0.2, 0.4
  polygenic_pve: 0
  eff_mode: "simple_lm"
  residual_mode: "identity"
  swap_eff: False
  keep_ld: True
  center_data: True
  cache: file(sim)
  tag: "sim1"
  @ALIAS: conf = Dict(!X, !Y, !eff_mode)
  $Y: res['Y']
  $V: res['V']
  $meta: dict(true_coef=res['true_coef'], residual_variance=res['residual_variance'])
  $pheno_file: ''

simple_lm(base_sim):
  n_signal: 1, 2, 3, 4, 5

lm_pve02(simple_lm):
  pve: 0.2
  n_traits: 1
