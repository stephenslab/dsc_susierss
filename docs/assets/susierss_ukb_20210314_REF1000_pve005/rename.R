input = 'susierss_ukb_20210218_REF1000_pve005.rds'

input_susie_purity = 'susierss_ukb_20210218_REF1000_pve005_susie_purity.rds'
input_fmv4_purity = 'susierss_ukb_20210218_REF1000_pve005_fmv4_purity.rds'

library(dplyr)
library(tibble)
dat = readRDS(input)

dat_purity = readRDS(input_susie_purity)
dat_purity = as_tibble(dat_purity)
dat_fmv4_purity = readRDS(input_fmv4_purity)
dat_fmv4_purity = as_tibble(dat_fmv4_purity)

## susie
dat$susie = as_tibble(dat$susie)
dat$susie$score_susie.avgr2 = dat_purity$score_susie.avgr2
dat$susie$method_susie = dat$susie$method
dat$susie$method.ld_method[dat$susie$method.ld_method == 'in_sample'] = 'in'
dat$susie$method.ld_method[dat$susie$method.ld_method == 'refin_sample'] = 'refin'
dat$susie$method.ld_method[dat$susie$method.ld_method == 'refout_sample'] = 'refout'
dat$susie$method = paste0(dat$susie$method_susie, '_ER', dat$susie$method.estimate_residual_variance, 
                          '_ld', dat$susie$method.ld_method, 
                          '_AZ', dat$susie$method.add_z,
                          '_rcor', dat$susie$method.rcor) 

## fm
dat$fm = as_tibble(dat$fm)
dat$fm$method_finemap.ld_method[dat$fm$method_finemap.ld_method == 'in_sample'] = 'in'
dat$fm$method_finemap.ld_method[dat$fm$method_finemap.ld_method == 'refin_sample'] = 'refin'
dat$fm$method_finemap.ld_method[dat$fm$method_finemap.ld_method == 'refout_sample'] = 'refout'
dat$fm$method = paste0('FINEMAPv1.1', 
                       '_ld', dat$fm$method_finemap.ld_method,
                       '_AZ', dat$fm$method_finemap.addz) 

## fmv1.4
dat$fmv4 = as_tibble(dat_fmv4_purity)
dat$fmv4$method_finemapv4.ld_method[dat$fmv4$method_finemapv4.ld_method == 'in_sample'] = 'in'
dat$fmv4$method_finemapv4.ld_method[dat$fmv4$method_finemapv4.ld_method == 'refin_sample'] = 'refin'
dat$fmv4$method_finemapv4.ld_method[dat$fmv4$method_finemapv4.ld_method == 'refout_sample'] = 'refout'
dat$fmv4$method = paste0(dat$fmv4$method_finemapv4,
                         '_ld', dat$fmv4$method_finemapv4.ld_method,
                         '_AZ', dat$fmv4$method_finemapv4.addz) 

## caviar
dat$caviar = as_tibble(dat$caviar)
dat$caviar$method_caviar.ld_method[dat$caviar$method_caviar.ld_method == 'in_sample'] = 'in'
dat$caviar$method_caviar.ld_method[dat$caviar$method_caviar.ld_method == 'refin_sample'] = 'refin'
dat$caviar$method_caviar.ld_method[dat$caviar$method_caviar.ld_method == 'refout_sample'] = 'refout'
dat$caviar$method = paste0('CAVIAR', 
                           '_ld', dat$caviar$method_caviar.ld_method,
                           '_AZ', dat$caviar$method_caviar.addz,
                           '_rcor', dat$caviar$method_caviar.rcor)

saveRDS(dat, 'susierss_ukb_20210218_REF1000_pve005.2.include.purity.rds')
