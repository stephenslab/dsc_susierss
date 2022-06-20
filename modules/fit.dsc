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

adjustld: adjustld.R
  sumstats: $sumstats
  ld: $ld
  ldmethod: "in_sample", "refout_sample"
  lamb: 0, 1e-3, 'mlelikelihood'
  addz: FALSE
  rcor: TRUE
  ldfile: file(ld)
  N_ref: $N_ref
  n: $N_sample
  $z: z
  $ldinfo: list(ldfile = ldfile, ldmethod = ldmethod, lamb = lamb)

adjustld_lamb0(adjustld):
  lamb: 0

adjustld_addz(adjustld):
  lamb: 0
  addz: TRUE

adjustld_n_lamb0: adjustld_lamb0_n.R
  sumstats: $sumstats
  ld: $ld
  ldn: $ld_n
  ldmethod: "in_sample", "refout_sample"
  $z: z
  $ldinfo: ld_info

adjustld_insample(adjustld): R(z = sumstats$bhat/sumstats$shat;
                               ldfile = ld[[ldmethod]])
  ldmethod: "in_sample"
  lamb: 0

adjustld_gtex(adjustld):
  ldmethod: "in_sample", "out_sample"

adjustld_addz_gtex(adjustld_gtex):
  lamb: 0
  addz: TRUE

caviar: fit_caviar.R + R(b = $(meta)$true_coef;
                         nc = sum(b[,1]!=0);
                         if(nc > 3) nc = 3;
                         args = paste0('-g 0.001 -c ', nc);
                         posterior = finemap_mcaviar(z,ld_info[['ldfile']], args, prefix=cache))
  @CONF: R_libs = (dplyr, magrittr, data.table)
  z: $z
  ld_info: $ldinfo
  cache: file(CAVIAR)
  $posterior: posterior

finemap(caviar): fit_finemap.R + R(b = $(meta)$true_coef;
                                   nc = sum(b[,1]!=0);
                                   if(nc > 3) nc = 3;
                                   args = paste0('--n-causal-max ', nc);
                                   posterior = finemap_mvar(z,ld_info[['ldfile']], N_in, k, args, prefix=cache))
  k: NULL
  N_in: $N_sample
  cache: file(FM)

finemapv3(caviar): fit_finemap_v3.R + R(posterior = finemap_mvar_v1.3.1(sumstats$bhat, sumstats$shat,
                                        maf[[ld_info[['ldmethod']]]], ld_info[['ldfile']], N_in, k, method, args, prefix=cache))
  sumstats: $sumstats
  k: NULL
  maf: $maf
  N_in: $N_sample
  method: 'sss'
  args: "--n-causal-snps 1", "--n-causal-snps 2", "--n-causal-snps 3"
  cache: file(FM)

finemapv3_simple(finemapv3):
  args: "--n-causal-snps 3"

finemapv4(caviar): fit_finemap_v4.R + R(b = $(meta)$true_coef;
                                        nc = sum(b[,1]!=0);
                                        if(nc > 3) nc = 3;
                                        args = paste0('--n-causal-snps ', nc);
                                        posterior = finemap_mvar_v1.4(sumstats$bhat, sumstats$shat,
                                        maf[[ld_info[['ldmethod']]]], ld_info[['ldfile']], N_in, k, method, args, prefix=cache))
  sumstats: $sumstats
  k: NULL
  maf: $maf
  N_in: $N_sample
  method: 'sss'
  cache: file(FM)

finemapv4_n(finemapv4):
  N_in: $N_sample_n

finemapv4L5(finemapv4): fit_finemap_v4.R + R(posterior = finemap_mvar_v1.4(sumstats$bhat, sumstats$shat,
                                                         maf[[ld_info[['ldmethod']]]], ld_info[['ldfile']],
                                                         N_in, k, method, args, prefix=cache))
  args: '--n-causal-snps 5'

finemapv4L5_n(finemapv4L5):
  N_in: $N_sample_n

dap_z: fit_dap.R + R(posterior = finemap_dap(z, ld_info[['ldfile']], args, cache))
  z: $z
  ld_info: $ldinfo
  args: ""
  cache: file(DAP)
  $posterior: posterior

paintor: fit_paintor.R + R(b = $(meta)$true_coef;
                           nc = sum(b[,1]!=0);
                           if(nc > 3) nc = 3;
                           args = paste0('-enumerate ', nc);
                           z[is.na(z)] = 0;
                           pip = finemap_paintor(z, ld_info[['ldfile']], args, prefix=cache))
  z: $z
  ld_info: $ldinfo
  cache: file(PAINTOR)
  $pip: pip

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

susie_suff: initialize.R + fit_susie_suff.R + R(if(is.na(init)){
                                                  s_init = NA;
                                                }else if(init == 'oracle'){
                                                  s_init = init_susie_true($(meta)$true_coef);
                                                }else if(init == 'lasso'){
                                                  s_init = init_lasso(X,Y,L);
                                                };
                                                r = as.matrix(fread(ld_info[['ldfile']]));
                                                res = susie_suff_multiple(sumstats$bhat, sumstats$shat,
                                                r, n, L, s_init, estimate_residual_variance, refine))
  @CONF: R_libs = (susieR, data.table)
  sumstats: $sumstats
  X: $X_sample
  Y: $Y
  s_init: NA
  n: $N_sample
  L: 10
  ld_info: $ldinfo
  estimate_residual_variance: TRUE, FALSE
  init: NA
  refine: FALSE, TRUE
  $fitted: res$fitted
  $posterior: res$posterior

susie_suff_init(susie_suff):
  init: 'oracle', 'lasso'
  estimate_residual_variance: TRUE

susie_suff_refine(susie_suff):
  refine: TRUE

susie_suff_refine_n(susie_suff_refine):
  X: $X_sample_n
  n: $N_sample_n

susie_suff_Ltrue: fit_susie_suff.R + R(b = $(meta)$true_coef;
                                    L = sum(b[,1]!=0);
                                    r = as.matrix(fread(ld_info[['ldfile']]));
                                    res = susie_suff_multiple(sumstats$bhat, sumstats$shat,
                                                r, n, L, s_init, estimate_residual_variance, refine))
  @CONF: R_libs = (susieR, data.table)
  sumstats: $sumstats
  ld_info: $ldinfo
  estimate_residual_variance: TRUE, FALSE
  n: $N_sample
  refine: TRUE
  s_init: NA
  $fitted: res$fitted
  $posterior: res$posterior

susie_suff_Ltrue_n(susie_suff_Ltrue):
  n: $N_sample_n

#------------------------------
# SuSiE with summary statistics
#------------------------------

susie_rss: initialize.R + fit_susierss.R + R(if(is.na(init)){
                                                s_init = NA;
                                              }else if(init == 'oracle'){
                                                s_init = init_susie_rss_true($(meta)$true_coef, n);
                                              }else if(init == 'lasso'){
                                                s_init = init_rss_lasso(z,r,L);
                                              };
                                              r = as.matrix(fread(ld_info[['ldfile']]));
                                              res = susie_rss_multiple(z, r, n, L, s_init,
                                              estimate_residual_variance, refine))
  @CONF: R_libs = (susieR, data.table)
  z: $z
  L: 10
  ld_info: $ldinfo
  n: $N_sample
  refine: FALSE, TRUE
  init: NA
  estimate_residual_variance: TRUE, FALSE
  $fitted: res$fitted
  $posterior: res$posterior

susie_rss_Ltrue: fit_susierss.R + R(b = $(meta)$true_coef;
                                    L = sum(b[,1]!=0);
                                    r = as.matrix(fread(ld_info[['ldfile']]));
                                    res = susie_rss_multiple(z, r, n, L, NA,
                                    estimate_residual_variance, refine);)
  @CONF: R_libs = (susieR, data.table)
  z: $z
  ld_info: $ldinfo
  refine: TRUE
  n: $N_sample
  estimate_residual_variance: TRUE, FALSE
  $fitted: res$fitted
  $posterior: res$posterior

susie_rss_n(susie_rss):
  n: $N_sample_n
  refine: TRUE

susie_rss_Ltrue_n(susie_rss_Ltrue):
  n: $N_sample_n

