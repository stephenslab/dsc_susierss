library(dscrutils)
out_susie = dscrutils::dscquery('output/susierss_ukb_REF1000_pve005/', 
                          targets = c('sim_gaussian', 'sim_gaussian.n_signal', 'sim_gaussian.meta',
                                      'method_susie', 'method_susie.estimate_residual_variance',
                                      'method_susie.add_z', 'method_susie.ld_method',
                                      'method_susie.L','method_susie.rcor', 'method_susie.scalez',
                                      'method_susie.DSC_TIME',
                                      'score_susie', 'score_susie.total', 'score_susie.valid',
                                      'score_susie.size', 'score_susie.purity', 'score_susie.converged', 
                                      'score_susie.pip', 'score_susie.sigma2', 'score_susie.lamb'),
                          module.output.files = c('method_susie','score_susie'), 
                          ignore.missing.files = TRUE)

out_fmv1 = dscrutils::dscquery('output/susierss_ukb_REF1000_pve005/', 
                               targets = c('sim_gaussian', 'sim_gaussian.n_signal', 'sim_gaussian.meta',
                                           'method_finemap', 'method_finemap.addz',
                                           'method_finemap.ld_method', 'method_finemap.rcor', 'method_finemap.scalez',
                                           'method_finemap.DSC_TIME',
                                           'score_finemap', 'score_finemap.total', 'score_finemap.valid',
                                           'score_finemap.size','score_finemap.pip'),
                               module.output.files = c('method_finemap','score_finemap'), 
                               ignore.missing.files = TRUE)

out_fmv4 = dscrutils::dscquery('output/susierss_ukb_REF1000_pve005/', 
                               targets = c('sim_gaussian', 'sim_gaussian.n_signal', 'sim_gaussian.meta',
                                           'method_finemapv4', 'method_finemapv4.addz',
                                           'method_finemapv4.ld_method', 'method_finemapv4.rcor', 'method_finemapv4.scalez',
                                           'method_finemapv4.DSC_TIME',
                                           'score_finemapv4', 'score_finemapv4.total', 'score_finemapv4.valid',
                                           'score_finemapv4.size','score_finemapv4.pip'),
                               module.output.files = c('method_finemapv4','score_finemapv4'), 
                               ignore.missing.files = TRUE)

out_caviar = dscrutils::dscquery('output/susierss_ukb_REF1000_pve005/', 
                          targets = c('sim_gaussian', 'sim_gaussian.n_signal', 'sim_gaussian.meta',
                                      'method_caviar', 'method_caviar.addz',
                                      'method_caviar.ld_method', 'method_caviar.rcor',
                                      'method_caviar.DSC_TIME',
                                      'score_caviar', 'score_caviar.total', 'score_caviar.valid',
                                      'score_caviar.size','score_caviar.pip'),
                          module.output.files = c('method_caviar','score_caviar'), 
                          ignore.missing.files = TRUE)


saveRDS(list(susie = out_susie, fm = out_fmv1, fmv4 = out_fmv4, caviar = out_caviar), 
        'results/susierss_ukb_20210218_REF1000_pve005/susierss_ukb_20210218_REF1000_pve005.rds')
