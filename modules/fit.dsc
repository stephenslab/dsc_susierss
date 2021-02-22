# workhorse(s) for finemapping

# Module input
# ============
# $X, $Y: full data; or
# $sumstats: summary statistics; or / and
# $ld: LD information

# Module output
# =============
# $fitted: for diagnostics
# $posterior: for inference

caviar: fit_caviar.R + add_z.R + R(posterior = finemap_mcaviar(z,ld_file, args, prefix=cache))
  @CONF: R_libs = (dplyr, magrittr, data.table)
  sumstats: $sumstats
  ld: $ld
  N_ref: $N_ref
  (addz, ld_method): (FALSE, "in_sample"),(FALSE, "refin_sample"),(FALSE, "refout_sample"),(TRUE, "refin_sample"),(TRUE, "refout_sample")
  ld_ref_z_file: file(ref.z.ld)
  args: "-g 0.001 -c 1", "-g 0.001 -c 2", "-g 0.001 -c 3"
  cache: file(CAVIAR)
  $posterior: posterior

caviar_simple(caviar):
  args: "-g 0.001 -c 3"

finemap(caviar): fit_finemap.R + add_z.R + R(posterior = finemap_mvar(z,ld_file, N_in, k, args, prefix=cache))
  k: NULL
  N_in: $N_sample
  args: "--n-causal-max 1", "--n-causal-max 2", "--n-causal-max 3"
  cache: file(FM)

finemap_simple(finemap):
  args: "--n-causal-max 3"

finemapv3(caviar): fit_finemap_v3.R + add_z.R + R(posterior = finemap_mvar_v1.3.1(sumstats$bhat, sumstats$shat,
                                        maf[[ld_method]], ld_file, N_in, k, method, args, prefix=cache))
  k: NULL
  maf: $maf
  N_in: $N_sample
  method: 'sss'
  args: "--n-causal-snps 1", "--n-causal-snps 2", "--n-causal-snps 3"
  cache: file(FM)

finemapv3_simple(finemapv3):
  args: "--n-causal-snps 3"

susie: initialize.R + R(if(is.na(init)){
                          s_init = NA
                        }else if(init == 'oracle'){
                          s_init = init_susie_true($(meta)$true_coef)
                        }else if(init == 'lasso'){
                          s_init = init_lasso(X,Y,maxL)
                        }) + fit_susie.R
  # Prior variance of nonzero effects.
  @CONF: R_libs = susieR
  maxI: 1000
  maxL: 10
  null_weight: 0
  prior_var: 0
  X: $X_sample
  Y: $Y
  estimate_residual_variance: TRUE, FALSE
  init: NA
  $posterior: posterior
  $fitted: fitted

susie_init(susie):
  init: NA, 'oracle', 'lasso'
  
#------------------------------
# SuSiE with sufficient statistics
#------------------------------

susie_suff: fit_susie_suff.R
  @CONF: R_libs = (susieR, data.table)
  sumstats: $sumstats
  s_init: NA
  n: $N_sample
  N_ref: $N_ref
  L: 5, 10
  ld: $ld
  estimate_residual_variance: TRUE, FALSE
  (add_z, ld_method): (FALSE,"in_sample")
  $fitted: res$fitted
  $posterior: res$posterior


#------------------------------
# SuSiE with summary statistics
#------------------------------

susie_rss: initialize.R + R(if(is.na(init)){
                          s_init = NA
                        }else if(init == 'oracle'){
                          s_init = init_susie_rss_true($(meta)$true_coef, n)
                        }else if(init == 'lasso'){
                          s_init = init_rss_lasso(z,r,L)
                        }) + fit_susie_rss.R
  @CONF: R_libs = (susieR, data.table)
  sumstats: $sumstats
  ld: $ld
  L: 5, 10
  n: $N_sample
  N_ref: $N_ref
  estimate_residual_variance: TRUE, FALSE
  z_ld_weight: NA
  add_z: FALSE, TRUE
  ld_method: "in_sample", "refin_sample", "refout_sample"
  init: NA
  $fitted: res$fitted
  $posterior: res$posterior

susie_rss_init(susie_rss):
  init: NA, 'oracle', 'lasso'

susie_rss_zldweight(susie_rss):
  z_ld_weight: 0, 0.001, 0.002, 0.005, 0.01, 0.02


susie_rss_lambda: initialize.R + R(if(is.na(init)){
                          s_init = NA
                        }else if(init == 'oracle'){
                          s_init = init_susie_rss_true($(meta)$true_coef, n)
                        }else if(init == 'lasso'){
                          s_init = init_rss_lasso(z,r,L)
                        }) + fit_susie_rss_lambda.R
  @CONF: R_libs = (susieR, data.table)
  sumstats: $sumstats
  ld: $ld
  L: 5, 10
  n: $N_sample
  N_ref: $N_ref
  lamb: "estimate"
  estimate_residual_variance: TRUE, FALSE
  add_z: FALSE, TRUE
  ld_method: "in_sample", "refin_sample", "refout_sample"
  init: NA
  $fitted: res$fitted
  $posterior: res$posterior

susie_rss_lambda_init(susie_rss_lambda):
  init: NA, 'oracle', 'lasso'
