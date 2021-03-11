library(dscrutils)
# out_susie = dscrutils::dscquery('output/susierss_ukb_REF1000_pve005_rss_20210307/', 
#                           targets = c('sim_gaussian', 'sim_gaussian.n_signal', 'sim_gaussian.meta',
#                                       'ss_init', 'ss_init.init',
#                                       'ss_init.estimate_residual_variance',
#                                       'ss_init.add_z', 'ss_init.ld_method',
#                                       'ss_init.L','ss_init.fullrank',
#                                       'ss_init.DSC_TIME',
#                                       'score_susie', 'score_susie.total', 'score_susie.valid',
#                                       'score_susie.size', 'score_susie.purity', 'score_susie.converged', 
#                                       'score_susie.objective','score_susie.pip', 
#                                       'score_susie.sigma2', 'score_susie.lamb'),
#                           module.output.files = c('sim_gaussian','ss_init', 'score_susie'), 
#                           groups=c('ss_init: susie_init, susie_suff_init'),
#                           ignore.missing.files = TRUE)
# 
# saveRDS(out_susie, 
#         'results/susierss_ukb_REF1000_pve005_init_20210307/susierss_ukb_REF1000_pve005_init_20210307.rds')

## query 3 step procedure
out_susie_3steps = dscrutils::dscquery('output/susierss_ukb_REF1000_pve005_rss_20210307/', 
                                targets = c('sim_gaussian', 'sim_gaussian.n_signal', 'sim_gaussian.meta',
                                            'susie_suff_3steps', 'susie_suff_3steps.init',
                                            'susie_suff_3steps.estimate_residual_variance',
                                            'susie_suff_3steps.add_z', 'susie_suff_3steps.ld_method',
                                            'susie_suff_3steps.L',
                                            'susie_suff_3steps.DSC_TIME',
                                            'score_susie', 'score_susie.total', 'score_susie.valid',
                                            'score_susie.size', 'score_susie.purity', 'score_susie.converged', 
                                            'score_susie.objective','score_susie.pip', 
                                            'score_susie.sigma2', 'score_susie.lamb'),
                                module.output.files = c('sim_gaussian','susie_suff_3steps', 'score_susie'),
                                ignore.missing.files = TRUE)

saveRDS(out_susie_3steps, 
        'results/susierss_ukb_REF1000_pve005_init_20210307/susierss_ukb_REF1000_pve005_init_20210307_susie3steps.rds')
