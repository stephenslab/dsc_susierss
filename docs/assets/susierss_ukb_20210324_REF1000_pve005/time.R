dat = readRDS('susierss_ukb_20210324_REF1000_pve005.2.rds')

susie_suff_out = dat$susie %>% filter(method_susie == 'susie_suff', 
                                      method_susie.estimate_residual_variance == FALSE,
                                      adjustld_med.ldmethod == 'in',
                                      adjustld_med.addz == FALSE,
                                      adjustld_med.lamb == 0,
                                      method_susie.refine == FALSE)
susie_suff_out_refine = dat$susie %>% filter(method_susie == 'susie_suff', 
                                      method_susie.estimate_residual_variance == FALSE,
                                      adjustld_med.ldmethod == 'in',
                                      adjustld_med.addz == FALSE,
                                      adjustld_med.lamb == 0,
                                      method_susie.refine == TRUE)
susierss_out = dat$susie %>% filter(method_susie == 'susie_rss', 
                                    adjustld_med.ldmethod == 'in',
                                    adjustld_med.addz == FALSE,
                                    adjustld_med.lamb == 0,
                                    method_susie.refine == FALSE)
susierss_out_refine = dat$susie %>% filter(method_susie == 'susie_rss', 
                                    adjustld_med.ldmethod == 'in',
                                    adjustld_med.addz == FALSE,
                                    adjustld_med.lamb == 0,
                                    method_susie.refine == TRUE)

fmv4 = dat$fmv4 %>% filter(method_finemapv4 == 'finemapv4', 
                            adjustld_med.ldmethod == 'in',
                            adjustld_med.addz == FALSE,
                            adjustld_med.lamb == 0)
fmv4L4 = dat$fmv4 %>% filter(method_finemapv4 == 'finemapv4L4', 
                           adjustld_med.ldmethod == 'in',
                           adjustld_med.addz == FALSE,
                           adjustld_med.lamb == 0)
caviar = dat$caviar %>% filter(adjustld_med.ldmethod == 'in',
                               adjustld_med.addz == FALSE,
                               adjustld_med.lamb == 0)

round(summary(susie_suff_out$method_susie.DSC_TIME)[c(4,1,6)],2)
round(summary(susie_suff_out_refine$method_susie.DSC_TIME)[c(4,1,6)],2)
round(summary(susierss_out$method_susie.DSC_TIME)[c(4,1,6)],2)
round(summary(susierss_out_refine$method_susie.DSC_TIME)[c(4,1,6)],2)
round(summary(fmv4$method_finemapv4.DSC_TIME)[c(4,1,6)],2)
round(summary(fmv4L4$method_finemapv4.DSC_TIME)[c(4,1,6)],2)
round(summary(caviar$method_caviar.DSC_TIME)[c(4,1,6)],2)


