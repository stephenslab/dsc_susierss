input = 'susierss_ukb_20210324_REF500_pve005.2.rds'

library(dplyr)

removedat_all = c(TRUE, FALSE)
for( rem in removedat_all){
  dat = readRDS(input)
  if(rem){
    nonconverge_dat = unique(dat$susie$small_data.dataset[which(dat$susie$score_susie.converged == FALSE)])
    
    susie_rmidx = which(dat$susie$small_data.dataset %in% nonconverge_dat)
    fmv1_rmidx = which(dat$fmv1$small_data.dataset %in% nonconverge_dat)
    fmv4_rmidx = which(dat$fmv4$small_data.dataset %in% nonconverge_dat)
    caviar_rmidx = which(dat$caviar$small_data.dataset %in% nonconverge_dat)
    
    dat$susie = dat$susie[-susie_rmidx,]
    dat$fmv1 = dat$fmv1[-fmv1_rmidx,]
    dat$fmv4 = dat$fmv4[-fmv4_rmidx,]
    dat$caviar = dat$caviar[-caviar_rmidx,]
  }
  output = paste0('susierss_ukb_20210324_REF500_pve005_pip_extraction/ukb_pip_remdatall',rem,'.rds')
  
  ## susierss
  res = list()
  for(i in 1:nrow(dat$susie)){
    method = dat$susie[i,]$method
    true_coef = c(dat$susie[i,]$sim_gaussian.meta[[1]]$true_coef != 0)
    pip = dat$susie[i,]$score_susie.pip[[1]]
    if(dat$susie[i,]$score_susie.converged == FALSE){
      next
    }
    if(all(is.na(pip))){
      next
    }
    if (!(method %in% names(res))) {
      res[[method]] = list(pip = pip, truth = true_coef)
    } else {
      res[[method]]$pip = append(res[[method]]$pip, pip)
      res[[method]]$truth = append(res[[method]]$truth, true_coef)
    }
    if (i%%100==0) print(i)
  }
  
  ## fm
  for(i in 1:nrow(dat$fmv1)){
    method = dat$fmv1[i,]$method
    true_coef = c(dat$fmv1[i,]$sim_gaussian.meta[[1]]$true_coef != 0)
    pip = dat$fmv1[i,]$score_finemap.pip[[1]]
    
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
}




