input = 'susierss_ukb_20210218_REF1000_pve005.rds'

library(dplyr)
library(tibble)
dat = readRDS(input)
dat = as_tibble(dat)
# rename_cols = function(dat) {
#   colnames.change = which(!grepl('output', colnames(dat)))
#   tmp = strsplit(colnames(dat)[colnames.change], "[.]")
#   colnames(dat)[colnames.change] = unlist(lapply(1:length(tmp), function(i) ifelse(length(tmp[[i]])>1, tmp[[i]][2], tmp[[i]][1])))
#   return(dat)
# }

# dat$method_susie.add_z[dat$method_susie.z_ld_weight == 0] = FALSE
# dat$method_susie.add_z[dat$method_susie.z_ld_weight == 2e-05] = TRUE
# dat$method_susie.add_z[dat$method_susie.z_ld_weight == 0.001] = TRUE

dat$method_susie.ld_method[dat$method_susie.ld_method == 'in_sample'] = 'in'
dat$method_susie.ld_method[dat$method_susie.ld_method == 'refin_sample'] = 'refin'
dat$method_susie.ld_method[dat$method_susie.ld_method == 'refout_sample'] = 'refout'

dat$method = paste0(dat$method_susie, '_ER', dat$method_susie.estimate_residual_variance, 
                    '_ld', dat$method_susie.ld_method, 
                    '_AZ', dat$method_susie.add_z,
                    '_rcor', dat$method_susie.rcor) 

saveRDS(dat, 'susierss_ukb_20210218_REF1000_pve005.2.rds')
