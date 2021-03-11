input = 'susierss_ukb_REF1000_pve005_init_20210307.2.rds'

library(dplyr)
dat = readRDS(input)

output = paste0('susierss_ukb_REF1000_pve005_init_20210307_pip_extraction/susie_pip.rds')

## susierss
res = list()
for(i in 1:nrow(dat)){
  method = dat[i,]$method
  true_coef = c(dat[i,]$sim_gaussian.meta[[1]]$true_coef != 0)
  pip = dat[i,]$score_susie.pip[[1]]
  
  if (!(method %in% names(res))) {
    res[[method]] = list(pip = pip, truth = true_coef)
  } else {
    res[[method]]$pip = append(res[[method]]$pip, pip)
    res[[method]]$truth = append(res[[method]]$truth, true_coef)
  }
  if (i%%100==0) print(i)
}

for (method in unique(names(res))) {
  res[[method]] = do.call(cbind, res[[method]])
}

saveRDS(res, output)

## susie 3 steps
library(dplyr)
input = 'susierss_ukb_REF1000_pve005_init_20210307_susie3steps.2.rds'
dat = readRDS(input)

output = paste0('susierss_ukb_REF1000_pve005_init_20210307_pip_extraction/susie_pip.rds')
res = readRDS(output)
## susierss
for(i in 1:nrow(dat)){
  method = dat[i,]$method
  true_coef = c(dat[i,]$sim_gaussian.meta[[1]]$true_coef != 0)
  pip = dat[i,]$score_susie.pip[[1]]
  
  if (!(method %in% names(res))) {
    res[[method]] = list(pip = pip, truth = true_coef)
  } else {
    res[[method]]$pip = append(res[[method]]$pip, pip)
    res[[method]]$truth = append(res[[method]]$truth, true_coef)
  }
  if (i%%100==0) print(i)
}

res[['susie_suff_3steps']] = do.call(cbind, res[['susie_suff_3steps']])

saveRDS(res, output)

