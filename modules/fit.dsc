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

finemapv4L4(finemapv4): fit_finemap_v4.R + add_z.R + R(posterior = finemap_mvar_v1.4(sumstats$bhat, sumstats$shat,
                                                                  maf[[ld_method]], ld_file, N_in, k, method, args, 
                                                                  prefix=cache))
  args: '-n-causal-snps 4'


finemapv4_gtex(finemapv4):
  (addz, ld_method, rcor): (FALSE, "in_sample",TRUE),(FALSE, "out_sample",TRUE),(TRUE, "out_sample",TRUE)

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
  estimate_residual_variance: TRUE
  
#------------------------------
# SuSiE with sufficient statistics
#------------------------------

susie_suff: initialize.R + adjustld.R + fit_susie_suff.R + R(if(is.na(init)){
                                                               s_init = NA;
                                                            }else if(init == 'oracle'){
                                                              s_init = init_susie_true($(meta)$true_coef);
                                                            }else if(init == 'lasso'){
                                                              s_init = init_lasso(X,Y,L);
                                                            };
                                                            res = susie_suff_multiple(sumstats$bhat, sumstats$shat, 
                                                            r, n, L, s_init, estimate_residual_variance))
  @CONF: R_libs = (susieR, data.table)
  sumstats: $sumstats
  X: $X_sample
  Y: $Y
  s_init: NA
  n: $N_sample
  N_ref: $N_ref
  L: 10
  ld: $ld
  estimate_residual_variance: TRUE, FALSE
  (add_z, ld_method): (FALSE,"in_sample"),(FALSE,"refin_sample"),(FALSE,"refout_sample")
  fullrank: FALSE
  rcor: FALSE
  init: NA
  $fitted: res$fitted
  $posterior: res$posterior

susie_suff_init(susie_suff):
  init: 'oracle', 'lasso'
  (add_z, ld_method): (FALSE,"in_sample")
  fullrank: FALSE
  estimate_residual_variance: TRUE

susie_suff_addz(susie_suff):
  (add_z, ld_method): (TRUE,"refin_sample"), (TRUE,"refout_sample")
  rcor: FALSE, TRUE
  
susie_suff_3steps(susie_suff): adjustld.R + fit_susie_suff_3steps.R + R(res = susie_suff_3steps_multiple(sumstats$bhat, sumstats$shat, r, n, L, estimate_residual_variance))
  
susie_suff_3steps_addz(susie_suff_3steps):
  (add_z, ld_method): (TRUE,"refin_sample"), (TRUE,"refout_sample")
  rcor: FALSE, TRUE

###### gtex

susie_suff_gtex(susie_suff):
  (add_z, ld_method): (FALSE,"in_sample"),(FALSE,"out_sample")

susie_suff_addz_gtex(susie_suff_gtex):
  (add_z, ld_method): (TRUE,"out_sample")
  rcor: FALSE, TRUE

susie_suff_3steps_gtex(susie_suff_3steps):
  (add_z, ld_method): (FALSE,"in_sample"),(FALSE,"out_sample")

susie_suff_3steps_addz_gtex(susie_suff_3steps):
  (add_z, ld_method): (TRUE,"out_sample")
  rcor: FALSE, TRUE
  
#------------------------------
# SuSiE with summary statistics
#------------------------------

susie_rss: initialize.R + adjustld.R + fit_susie_rss.R + R(if(is.na(init)){
                                                             s_init = NA;
                                                          }else if(init == 'oracle'){
                                                            s_init = init_susie_rss_true($(meta)$true_coef, n);
                                                          }else if(init == 'lasso'){
                                                            s_init = init_rss_lasso(z,r,L);
                                                          };
                                                          if(!is.na(z_ld_weight)){
                                                            res = susie_rss_multiple(z, r, L, z_ld_weight, s_init, estimate_residual_variance);
                                                          }else{
                                                            res = susie_rss_multiple(z, r, L, 0, s_init, estimate_residual_variance);})
  @CONF: R_libs = (susieR, data.table)
  sumstats: $sumstats
  ld: $ld
  L: 10
  n: $N_sample
  N_ref: $N_ref
  estimate_residual_variance: TRUE, FALSE
  ld_method: "in_sample", "refin_sample", "refout_sample"
  z_ld_weight: NA
  add_z: FALSE
  rcor: FALSE
  fullrank: FALSE
  init: NA
  $fitted: res$fitted
  $posterior: res$posterior

susie_rss_addz(susie_rss):
  ld_method: "refin_sample", "refout_sample"
  add_z: TRUE
  rcor: FALSE, TRUE

susie_rss_3steps(susie_rss): adjustld.R + fit_susie_rss_3steps.R + \
                          R(if(!is.na(z_ld_weight)){
                              res = susie_rss_3steps_multiple(z, r, L, z_ld_weight, estimate_residual_variance);
                            }else{
                              res = susie_rss_3steps_multiple(z, r, L, 0, estimate_residual_variance);})

susie_rss_3steps_addz(susie_rss_3steps):
  ld_method: "refin_sample", "refout_sample"
  add_z: TRUE
  rcor: FALSE, TRUE

#########gtex

susie_rss_gtex(susie_rss):
  ld_method: "in_sample", "out_sample"

susie_rss_addz_gtex(susie_rss):
  ld_method: "out_sample"
  add_z: TRUE
  rcor: FALSE, TRUE

susie_rss_3steps_gtex(susie_rss_3steps):
  ld_method: "in_sample", "out_sample"

susie_rss_3steps_addz_gtex(susie_rss_3steps):
  ld_method: "out_sample"
  add_z: TRUE
  rcor: FALSE, TRUE
  
susie_rss_init(susie_rss):
  init: NA, 'oracle', 'lasso'

susie_rss_zldweight(susie_rss):
  z_ld_weight: 0, 0.001, 0.002, 0.005, 0.01, 0.02

###################################

susie_rss_suff(susie_rss): initialize.R + adjustld.R + fit_susie_rsssuff.R + \
                           R(if(is.na(init)){
                               s_init = NA;
                             }else if(init == 'oracle'){
                               s_init = init_susie_rss_true($(meta)$true_coef, n);
                             }else if(init == 'lasso'){
                               s_init = init_rss_lasso(z,r,L);
                             };
                             res = susie_rsssuff_multiple(z, r, L, s_init, estimate_residual_variance))
  estimate_residual_variance: FALSE

susie_rss_suff_addz(susie_rss_suff):
  ld_method: "refin_sample", "refout_sample"
  add_z: TRUE
  rcor: FALSE, TRUE
  
susie_rss_suff_3steps(susie_rss): adjustld.R + fit_susie_rss_suff_3steps.R + \
                                  R(res = susie_rsssuff_3steps_multiple(z, r, L))
  
susie_rss_suff_3steps_addz(susie_rss_suff_3steps):
  ld_method: "refin_sample", "refout_sample"
  add_z: TRUE
  rcor: FALSE, TRUE

############ gtex
susie_rss_suff_gtex(susie_rss_suff):
  ld_method: "in_sample", "out_sample"

susie_rss_suff_addz_gtex(susie_rss_suff):
  ld_method: "out_sample"
  add_z: TRUE
  rcor: FALSE, TRUE

susie_rss_suff_3steps_gtex(susie_rss_suff_3steps):
  ld_method: "in_sample", "out_sample"

susie_rss_suff_3steps_addz_gtex(susie_rss_suff_3steps):
  ld_method: "out_sample"
  add_z: TRUE
  rcor: FALSE, TRUE

########################################
susie_rss_lambda: initialize.R + adjustld.R + fit_susie_rss_lambda.R + \
                  R(if(is.na(init)){
                      s_init = NA;
                    }else if(init == 'oracle'){
                      s_init = init_susie_rss_true($(meta)$true_coef, n);
                    }else if(init == 'lasso'){
                      s_init = init_rss_lasso(z,r,L);
                    };
                    res = susie_rss_lamb_multiple(z, r, L, lamb, s_init, estimate_residual_variance))
  @CONF: R_libs = (susieR, data.table)
  sumstats: $sumstats
  ld: $ld
  L: 10
  n: $N_sample
  N_ref: $N_ref
  lamb: "estimate"
  estimate_residual_variance: TRUE, FALSE
  add_z: FALSE
  ld_method: "in_sample", "refin_sample", "refout_sample"
  fullrank: FALSE
  rcor: FALSE
  init: NA
  $fitted: res$fitted
  $posterior: res$posterior

susie_rss_lambda_addz(susie_rss_lambda):
  add_z: TRUE
  ld_method: "refin_sample", "refout_sample"
  rcor: FALSE, TRUE

susie_rss_lambda_3steps(susie_rss_lambda): adjustld.R + fit_susie_rss_lambda_3steps.R + \
                                           R(res = susie_rss_lamb_3steps_multiple(z, r, L, lamb,estimate_residual_variance))

susie_rss_lambda_3steps_addz(susie_rss_lambda_3steps):
  add_z: TRUE
  ld_method: "refin_sample", "refout_sample"
  rcor: FALSE, TRUE

############gtex

susie_rss_lambda_gtex(susie_rss_lambda):
  ld_method: "in_sample", "out_sample"
  
susie_rss_lambda_addz_gtex(susie_rss_lambda_gtex):
  ld_method: 'out_sample'
  add_z: TRUE
  rcor: FALSE, TRUE
  
susie_rss_lambda_3steps_gtex(susie_rss_lambda_3steps):
  ld_method: "in_sample", "out_sample"

susie_rss_lambda_3steps_addz_gtex(susie_rss_lambda_3steps):
  ld_method: "out_sample"
  add_z: TRUE
  rcor: FALSE, TRUE
  
susie_rss_lambda_init(susie_rss_lambda):
  init: NA, 'oracle', 'lasso'
