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

caviar: fit_caviar.R + add_z.R + R(b = $(meta)$true_coef;
                                   nc = sum(b[,1]!=0);
                                   if(nc > 3) nc = 3;
                                   args = paste0('-g 0.001 -c ', nc);
                                   posterior = finemap_mcaviar(z,ld_file, args, prefix=cache))
  @CONF: R_libs = (dplyr, magrittr, data.table)
  sumstats: $sumstats
  ld: $ld
  N_ref: $N_ref
  (addz, ld_method, rcor): (FALSE, "in_sample",TRUE),(FALSE, "refout_sample",TRUE),(TRUE, "refout_sample",TRUE),(TRUE, "refout_sample",FALSE)
  ld_ref_z_file: file(ref.z.ld)
  cache: file(CAVIAR)
  $posterior: posterior

caviar_simple(caviar):
  args: "-g 0.001 -c 3"

caviar_gtex(caviar):
  (addz, ld_method, rcor): (FALSE, "in_sample",TRUE),(FALSE, "out_sample",TRUE),(TRUE, "out_sample",TRUE),(TRUE, "out_sample",FALSE)

caviar_scale(caviar): fit_caviar.R + add_zscale.R + R(b = $(meta)$true_coef;
                                   nc = sum(b[,1]!=0);
                                   if(nc > 3) nc = 3;
                                   args = paste0('-g 0.001 -c ', nc);
                                   posterior = finemap_mcaviar(z,ld_file, args, prefix=cache))
  (addz, ld_method, rcor): (TRUE, "refout_sample",TRUE)
  scalez: 'max', 'ratio'
  N_in: $N_sample

caviar_scale_gtex(caviar_scale):
  (addz, ld_method, rcor): (TRUE, "out_sample",TRUE)
  scalez: 'max', 'ratio'
  
finemap(caviar): fit_finemap.R + add_z.R + R(b = $(meta)$true_coef;
                                             nc = sum(b[,1]!=0);
                                             if(nc > 3) nc = 3;
                                             args = paste0('--n-causal-max ', nc);
                                             posterior = finemap_mvar(z,ld_file, N_in, k, args, prefix=cache))
  k: NULL
  N_in: $N_sample
  (addz, ld_method, rcor): (FALSE, "in_sample",TRUE),(FALSE, "refout_sample",TRUE),(TRUE, "refout_sample",TRUE)
  cache: file(FM)

finemap_simple(finemap):
  args: "--n-causal-max 3"

finemap_gtex(finemap):
  (addz, ld_method, rcor): (FALSE, "in_sample",TRUE),(FALSE, "out_sample",TRUE),(TRUE, "out_sample",TRUE)

finemap_scale(finemap): fit_finemap.R + add_zscale.R + R(b = $(meta)$true_coef;
                                             nc = sum(b[,1]!=0);
                                             if(nc > 3) nc = 3;
                                             args = paste0('--n-causal-max ', nc);
                                             posterior = finemap_mvar(z,ld_file, N_in, k, args, prefix=cache))
  (addz, ld_method, rcor): (TRUE, "refout_sample",TRUE)
  scalez: 'max', 'ratio'

finemap_scale_gtex(finemap_scale):
  (addz, ld_method, rcor): (TRUE, "out_sample",TRUE)
  scalez: 'max', 'ratio'
  
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

finemapv4(caviar): fit_finemap_v4.R + add_z.R + R(b = $(meta)$true_coef;
                                                  nc = sum(b[,1]!=0);
                                                  if(nc > 3) nc = 3;
                                                  args = paste0('--n-causal-snps ', nc);
                                                  posterior = finemap_mvar_v1.4(sumstats$bhat, sumstats$shat,
                                                  maf[[ld_method]], ld_file, N_in, k, method, args, prefix=cache))
  k: NULL
  maf: $maf
  N_in: $N_sample
  method: 'sss'
  (addz, ld_method, rcor): (FALSE, "in_sample",TRUE),(FALSE, "refout_sample",TRUE),(TRUE, "refout_sample",TRUE)
  cache: file(FM)

finemapv4_gtex(finemapv4):
  (addz, ld_method, rcor): (FALSE, "in_sample",TRUE),(FALSE, "out_sample",TRUE),(TRUE, "out_sample",TRUE)

finemapv4_scale(finemapv4): fit_finemap_v4.R + add_zscale.R + R(b = $(meta)$true_coef;
                                                  nc = sum(b[,1]!=0);
                                                  if(nc > 3) nc = 3;
                                                  args = paste0('--n-causal-snps ', nc);
                                                  posterior = finemap_mvar_v1.4(sumstats$bhat, sumstats$shat,
                                                  maf[[ld_method]], ld_file, N_in, k, method, args, prefix=cache))
  (addz, ld_method, rcor): (TRUE, "refout_sample",TRUE)
  scalez: 'max', 'ratio'
 
finemapv4_scale_gtex(finemapv4_scale):
  (addz, ld_method, rcor): (TRUE, "out_sample",TRUE)
  scalez: 'max', 'ratio'

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
  L: 10
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
  L: 10
  n: $N_sample
  N_ref: $N_ref
  estimate_residual_variance: TRUE, FALSE
  z_ld_weight: NA
  add_z: FALSE, TRUE
  ld_method: "in_sample", "refin_sample", "refout_sample"
  rcor: FALSE, TRUE
  init: NA
  $fitted: res$fitted
  $posterior: res$posterior

susie_rss_gtex(susie_rss):
  ld_method: "in_sample", "out_sample"

susie_rss_scale(susie_rss): initialize.R + R(if(is.na(init)){
                          s_init = NA
                        }else if(init == 'oracle'){
                          s_init = init_susie_rss_true($(meta)$true_coef, n)
                        }else if(init == 'lasso'){
                          s_init = init_rss_lasso(z,r,L)
                        }) + fit_susie_rss_scale.R
  add_z: TRUE
  scalez: 'max', 'ratio'
  rcor: TRUE, FALSE
  ld_method: "refin_sample", "refout_sample"
  estimate_residual_variance: TRUE
  
susie_rss_scale_gtex(susie_rss_scale):
  ld_method: "out_sample"
  
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
  L: 10
  n: $N_sample
  N_ref: $N_ref
  lamb: "estimate"
  estimate_residual_variance: TRUE, FALSE
  add_z: FALSE, TRUE
  ld_method: "in_sample", "refin_sample", "refout_sample"
  rcor: FALSE, TRUE
  init: NA
  $fitted: res$fitted
  $posterior: res$posterior

susie_rss_lambda_scale(susie_rss_lambda): initialize.R + R(if(is.na(init)){
                          s_init = NA
                        }else if(init == 'oracle'){
                          s_init = init_susie_rss_true($(meta)$true_coef, n)
                        }else if(init == 'lasso'){
                          s_init = init_rss_lasso(z,r,L)
                        }) + fit_susie_rss_lambda_scale.R
  estimate_residual_variance: TRUE
  add_z: TRUE
  ld_method: "refin_sample", "refout_sample"
  rcor: TRUE, FALSE
  scalez: 'max', 'ratio'
  
susie_rss_lambda_gtex(susie_rss_lambda):
  ld_method: "in_sample", "out_sample"
  
susie_rss_lambda_scale_gtex(susie_rss_lambda_scale):
  ld_method: "out_sample"
  
susie_rss_lambda_init(susie_rss_lambda):
  init: NA, 'oracle', 'lasso'
