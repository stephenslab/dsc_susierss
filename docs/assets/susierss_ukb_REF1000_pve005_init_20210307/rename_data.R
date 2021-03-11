input = 'susierss_ukb_REF1000_pve005_init_20210307.rds'

library(dplyr)
library(tibble)
# dat = readRDS(input)
# 
# ## susie
# ## all results are estimate residual vairance, add_z= FALSE, ld_method = 'in_sample', L = 10, fullrank=FALSE
# dat = as_tibble(dat)
# # dat$method_susie.ld_method[dat$method_susie.ld_method == 'in_sample'] = 'in'
# # dat$method_susie.ld_method[dat$method_susie.ld_method == 'refin_sample'] = 'refin'
# # dat$method_susie.ld_method[dat$method_susie.ld_method == 'refout_sample'] = 'refout'
# dat$method = paste0(dat$ss_init, dat$ss_init.init) 
# 
# saveRDS(dat, 'susierss_ukb_REF1000_pve005_init_20210307.2.rds')

## 3 steps 
input = 'susierss_ukb_REF1000_pve005_init_20210307_susie3steps.rds'
dat = readRDS(input)
dat = as_tibble(dat)
dat = dat %>% filter(susie_suff_3steps.estimate_residual_variance == TRUE)
dat$method = paste0('susie_suff_3steps')
saveRDS(dat, 'susierss_ukb_REF1000_pve005_init_20210307_susie3steps.2.rds')
