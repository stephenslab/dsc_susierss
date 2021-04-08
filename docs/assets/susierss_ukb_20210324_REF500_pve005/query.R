library(dscrutils)
out_susie = dscrutils::dscquery('output/susierss_ukb_REF500_pve005/',
                          targets = c('sim_gaussian', 'sim_gaussian.n_signal', 'sim_gaussian.meta',
                                      'adjustld_med.ldmethod', 'adjustld_med.lamb', 
                                      'adjustld_med.addz', 'adjustld_med.ldinfo',
                                      'method_susie', 'method_susie.estimate_residual_variance',
                                      'method_susie.refine','method_susie.DSC_TIME',
                                      'score_susie', 'score_susie.total', 'score_susie.valid',
                                      'score_susie.size', 'score_susie.purity',
                                      'score_susie.avgr2', 'score_susie.converged',
                                      'score_susie.pip', 'score_susie.sigma2'),
                          module.output.files = c('method_susie','score_susie'),
                          groups=c('method_susie: susie_suff, susie_rss',
                                   'adjustld_med: adjustld, adjustld_addz'),
                          ignore.missing.files = TRUE)

saveRDS(out_susie, 'results/susierss_ukb_20210324_REF500_pve005/susierss_ukb_20210324_REF500_pve005_susie.rds')

out_fmv1 = dscrutils::dscquery('output/susierss_ukb_REF500_pve005/',
                               targets = c('sim_gaussian', 'sim_gaussian.n_signal', 'sim_gaussian.meta',
                                           'adjustld_med.ldmethod', 'adjustld_med.lamb',
                                           'adjustld_med.addz','adjustld_med.ldinfo',
                                           'method_finemap','method_finemap.DSC_TIME',
                                           'score_finemap', 'score_finemap.total', 'score_finemap.valid',
                                           'score_finemap.size','score_finemap.pip'),
                               module.output.files = c('method_finemap','score_finemap'),
                               groups=c('adjustld_med: adjustld, adjustld_addz'),
                               ignore.missing.files = TRUE)

saveRDS(out_fmv1, 'results/susierss_ukb_20210324_REF500_pve005/susierss_ukb_20210324_REF500_pve005_fmv1.rds')

out_fmv4 = dscrutils::dscquery('output/susierss_ukb_REF500_pve005/',
                               targets = c('sim_gaussian', 'sim_gaussian.n_signal', 'sim_gaussian.meta',
                                           'adjustld_med.ldmethod', 'adjustld_med.lamb', 
                                           'adjustld_med.addz', 'adjustld_med.ldinfo',
                                           'method_finemapv4', 'method_finemapv4.DSC_TIME',
                                           'score_finemapv4', 'score_finemapv4.total', 'score_finemapv4.valid',
                                           'score_finemapv4.size', 'score_finemapv4.purity','score_finemapv4.avgr2',
                                           'score_finemapv4.total_pure', 'score_finemapv4.valid_pure',
                                           'score_finemapv4.size_pure', 'score_finemapv4.purity_pure',
                                           'score_finemapv4.avgr2_pure','score_finemapv4.pip'),
                               module.output.files = c('method_finemapv4','score_finemapv4'),
                               groups=c('adjustld_med: adjustld, adjustld_addz'),
                               ignore.missing.files = TRUE)

saveRDS(out_fmv4, 'results/susierss_ukb_20210324_REF500_pve005/susierss_ukb_20210324_REF500_pve005_fmv4.rds')

# out_caviar = dscrutils::dscquery('output/susierss_ukb_REF500_pve005/',
#                           targets = c('sim_gaussian', 'sim_gaussian.n_signal', 'sim_gaussian.meta',
#                                       'adjustld_med.ldmethod', 'adjustld_med.lamb', 
#                                       'adjustld_med.addz', 
#                                       'method_caviar', 'method_caviar.DSC_TIME',
#                                       'score_caviar', 'score_caviar.total', 'score_caviar.valid',
#                                       'score_caviar.size','score_caviar.pip'),
#                           module.output.files = c('method_caviar','score_caviar'),
#                           groups=c('adjustld_med: adjustld, adjustld_addz'),
#                           ignore.missing.files = TRUE)
# 
# saveRDS(out_caviar, 'results/susierss_ukb_20210324_REF500_pve005/susierss_ukb_20210324_REF500_pve005_caviar.rds')
