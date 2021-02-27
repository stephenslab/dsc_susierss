input = 'susierss_ukb_20210218_REF1000_pve005.2.rds'

library(dplyr)
dat = readRDS(input)

output = paste0('susierss_ukb_20210218_REF1000_pve005_pip_extraction/susierss_ukb_pip.rds')
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

for (method in unique(dat$method)) {
  res[[method]] = do.call(cbind, res[[method]])
}
saveRDS(res, output)
