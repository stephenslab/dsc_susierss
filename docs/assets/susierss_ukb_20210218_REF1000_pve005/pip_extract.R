input = 'susierss_ukb_20210218_REF1000_pve005.2.rds'

library(dplyr)
dat = readRDS(input)

output = paste0('susierss_ukb_20210218_REF1000_pve005_pip_extraction/ukb_pip.rds')

## susierss
res = list()
for(i in 1:nrow(dat$susie)){
  method = dat$susie[i,]$method
  true_coef = c(dat$susie[i,]$sim_gaussian.meta[[1]]$true_coef != 0)
  pip = dat$susie[i,]$score_susie.pip[[1]]
  
  if (!(method %in% names(res))) {
    res[[method]] = list(pip = pip, truth = true_coef)
  } else {
    res[[method]]$pip = append(res[[method]]$pip, pip)
    res[[method]]$truth = append(res[[method]]$truth, true_coef)
  }
  if (i%%100==0) print(i)
}

## fm
for(i in 1:nrow(dat$fm)){
  method = dat$fm[i,]$method
  true_coef = c(dat$fm[i,]$sim_gaussian.meta[[1]]$true_coef != 0)
  pip = dat$fm[i,]$score_finemap.pip[[1]]
  
  if (!(method %in% names(res))) {
    res[[method]] = list(pip = pip, truth = true_coef)
  } else {
    res[[method]]$pip = append(res[[method]]$pip, pip)
    res[[method]]$truth = append(res[[method]]$truth, true_coef)
  }
  if (i%%100==0) print(i)
}

## fmv1.4
for(i in 1:nrow(dat$fmv4)){
  method = dat$fmv4[i,]$method
  true_coef = c(dat$fmv4[i,]$sim_gaussian.meta[[1]]$true_coef != 0)
  pip = dat$fmv4[i,]$score_finemapv4.pip[[1]]
  
  if (!(method %in% names(res))) {
    res[[method]] = list(pip = pip, truth = true_coef)
  } else {
    res[[method]]$pip = append(res[[method]]$pip, pip)
    res[[method]]$truth = append(res[[method]]$truth, true_coef)
  }
  if (i%%100==0) print(i)
}

## caviar
for(i in 1:nrow(dat$caviar)){
  method = dat$caviar[i,]$method
  true_coef = c(dat$caviar[i,]$sim_gaussian.meta[[1]]$true_coef != 0)
  pip = dat$caviar[i,]$score_caviar.pip[[1]]
  
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


