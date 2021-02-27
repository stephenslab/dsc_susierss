library(dscrutils)
out = dscrutils::dscquery('output/susierss_ukb_REF1000_pve005/', 
                          targets = c('sim_gaussian', 'sim_gaussian.n_signal', 'sim_gaussian.meta',
                                      'method_susie', 'method_susie.estimate_residual_variance',
                                      'method_susie.add_z', 'method_susie.ld_method',
                                      'method_susie.L','method_susie.rcor', 'method_susie.DSC_TIME',
                                      'score_susie', 'score_susie.total', 'score_susie.valid',
                                      'score_susie.size', 'score_susie.purity', 'score_susie.converged', 
                                      'score_susie.pip', 'score_susie.sigma2', 'score_susie.lamb'),
                          module.output.files = c('method_susie','score_susie'), 
                          ignore.missing.files = TRUE)

saveRDS(out, 'results/susierss_ukb_20210218_REF1000_pve005/susierss_ukb_20210218_REF1000_pve005.rds')
