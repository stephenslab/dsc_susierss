input = 'susierss_ukb_20210218_REF1000_pve005.rds'

library(dplyr)
library(tibble)
dat = readRDS(input)

## susie
dat$susie = as_tibble(dat$susie)
dat$susie$method_susie.ld_method[dat$susie$method_susie.ld_method == 'in_sample'] = 'in'
dat$susie$method_susie.ld_method[dat$susie$method_susie.ld_method == 'refin_sample'] = 'refin'
dat$susie$method_susie.ld_method[dat$susie$method_susie.ld_method == 'refout_sample'] = 'refout'
dat$susie$method = paste0(dat$susie$method_susie, '_ER', dat$susie$method_susie.estimate_residual_variance, 
                    '_ld', dat$susie$method_susie.ld_method, 
                    '_AZ', dat$susie$method_susie.add_z,
                    '_rcor', dat$susie$method_susie.rcor,
                    '_scalez', dat$susie$method_susie.scalez) 

## fm
dat$fm = as_tibble(dat$fm)
dat$fm$method_finemap.ld_method[dat$fm$method_finemap.ld_method == 'in_sample'] = 'in'
dat$fm$method_finemap.ld_method[dat$fm$method_finemap.ld_method == 'refin_sample'] = 'refin'
dat$fm$method_finemap.ld_method[dat$fm$method_finemap.ld_method == 'refout_sample'] = 'refout'
dat$fm$method = paste0('FINEMAPv1.1', 
                       '_ld', dat$fm$method_finemap.ld_method,
                       '_AZ', dat$fm$method_finemap.addz,
                       '_scalez', dat$fm$method_finemap.scalez) 

## fmv1.4
dat$fmv4 = as_tibble(dat$fmv4)
dat$fmv4$method_finemapv4.ld_method[dat$fmv4$method_finemapv4.ld_method == 'in_sample'] = 'in'
dat$fmv4$method_finemapv4.ld_method[dat$fmv4$method_finemapv4.ld_method == 'refin_sample'] = 'refin'
dat$fmv4$method_finemapv4.ld_method[dat$fmv4$method_finemapv4.ld_method == 'refout_sample'] = 'refout'
dat$fmv4$method = paste0('FINEMAPv1.4', 
                       '_ld', dat$fmv4$method_finemapv4.ld_method,
                       '_AZ', dat$fmv4$method_finemapv4.addz,
                       '_scalez', dat$fmv4$method_finemapv4.scalez) 

## caviar
dat$caviar = as_tibble(dat$caviar)
dat$caviar$method_caviar.ld_method[dat$caviar$method_caviar.ld_method == 'in_sample'] = 'in'
dat$caviar$method_caviar.ld_method[dat$caviar$method_caviar.ld_method == 'refin_sample'] = 'refin'
dat$caviar$method_caviar.ld_method[dat$caviar$method_caviar.ld_method == 'refout_sample'] = 'refout'
dat$caviar$method = paste0('CAVIAR', 
                         '_ld', dat$caviar$method_caviar.ld_method,
                         '_AZ', dat$caviar$method_caviar.addz,
                         '_rcor', dat$caviar$method_caviar.rcor)

saveRDS(dat, 'susierss_ukb_20210218_REF1000_pve005.2.rds')
