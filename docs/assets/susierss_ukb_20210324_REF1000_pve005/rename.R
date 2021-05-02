library(dplyr)
library(tibble)

input_susie = 'susierss_ukb_20210324_REF1000_pve005_susie.rds'
input_fmv1 = 'susierss_ukb_20210324_REF1000_pve005_fmv1.rds'
input_fmv4 = 'susierss_ukb_20210324_REF1000_pve005_fmv4.rds'
input_caviar = 'susierss_ukb_20210324_REF1000_pve005_caviar.rds'

dat_susie = readRDS(input_susie)
## susie
dat_susie = as_tibble(dat_susie)
dat_susie$adjustld_med.lamb[dat_susie$adjustld_med.lamb == '0.0'] = '0'
dat_susie$adjustld_med.ldmethod[dat_susie$adjustld_med.ldmethod == 'in_sample'] = 'in'
dat_susie$adjustld_med.ldmethod[dat_susie$adjustld_med.ldmethod == 'refout_sample'] = 'refout'

dat_susie$method = paste0(dat_susie$method_susie, '_refine', dat_susie$method_susie.refine,
                          '_ER', dat_susie$method_susie.estimate_residual_variance, 
                          '_ld', dat_susie$adjustld_med.ldmethod, 
                          '_AZ', dat_susie$adjustld_med.addz,
                          '_lamb', dat_susie$adjustld_med.lamb) 

dat_fmv1 = readRDS(input_fmv1)
## fm
dat_fmv1 = as_tibble(dat_fmv1)
# dat_fmv1 = dat_fmv1 %>% filter(!is.na(score_finemap.total))
dat_fmv1$adjustld_med.lamb[dat_fmv1$adjustld_med.lamb == '0.0'] = '0'
dat_fmv1$adjustld_med.ldmethod[dat_fmv1$adjustld_med.ldmethod == 'in_sample'] = 'in'
dat_fmv1$adjustld_med.ldmethod[dat_fmv1$adjustld_med.ldmethod == 'refout_sample'] = 'refout'
dat_fmv1$method = paste0('FINEMAPv1.1', 
                         '_ld', dat_fmv1$adjustld_med.ldmethod,
                         '_AZ', dat_fmv1$adjustld_med.addz,
                         '_lamb', dat_fmv1$adjustld_med.lamb) 

dat_fmv4 = readRDS(input_fmv4)
## fmv1.4
dat_fmv4 = as_tibble(dat_fmv4)
dat_fmv4$adjustld_med.lamb[dat_fmv4$adjustld_med.lamb == '0.0'] = '0'
dat_fmv4$adjustld_med.ldmethod[dat_fmv4$adjustld_med.ldmethod == 'in_sample'] = 'in'
dat_fmv4$adjustld_med.ldmethod[dat_fmv4$adjustld_med.ldmethod == 'refout_sample'] = 'refout'
dat_fmv4$method = paste0(dat_fmv4$method_finemapv4,
                         '_ld', dat_fmv4$adjustld_med.ldmethod,
                         '_AZ', dat_fmv4$adjustld_med.addz,
                         '_lamb', dat_fmv4$adjustld_med.lamb) 

dat_caviar = readRDS(input_caviar)
## caviar
dat_caviar = as_tibble(dat_caviar)
# dat_caviar = dat_caviar %>% filter(!is.na(score_caviar.total))
dat_caviar$adjustld_med.lamb[dat_caviar$adjustld_med.lamb == '0.0'] = '0'
dat_caviar$adjustld_med.ldmethod[dat_caviar$adjustld_med.ldmethod == 'in_sample'] = 'in'
dat_caviar$adjustld_med.ldmethod[dat_caviar$adjustld_med.ldmethod == 'refout_sample'] = 'refout'
dat_caviar$method = paste0('CAVIAR',
                           '_ld', dat_caviar$adjustld_med.ldmethod,
                           '_AZ', dat_caviar$adjustld_med.addz,
                           '_lamb', dat_caviar$adjustld_med.lamb)

# saveRDS(list(susie = dat_susie,fmv1 = dat_fmv1,fmv4 = dat_fmv4),
#         'susierss_ukb_20210324_REF1000_pve005.2.rds')

saveRDS(list(susie = dat_susie, fmv1 = dat_fmv1,
             fmv4 = dat_fmv4, caviar = dat_caviar),
        'susierss_ukb_20210324_REF1000_pve005.2.rds')
