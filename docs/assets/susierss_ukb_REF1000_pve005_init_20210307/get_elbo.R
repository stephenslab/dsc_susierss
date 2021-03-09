## compare elbo

input = 'susierss_ukb_REF1000_pve005_init_20210307.2.rds'

output_susie = 'susie_ukb_elbo'

# dat = readRDS(input)
# res = list()
# for (i in 1:nrow(dat)) {
#   method = dat[i,]$method
#   elbo = dat[i,]$score_susie.objective
#   dname = dat[i,]$sim_gaussian.output.file
#   # m = readRDS(paste0(pref, dat[i,]$ss_init.output.file, '.rds'))
#   # elbo = susie_get_objective(m$fitted[[1]])
#   
#   if (!(method %in% names(res))) {
#     res[[method]] = list(elbo = elbo, datnum = dname)
#   } else {
#     res[[method]]$elbo = append(res[[method]]$elbo, elbo)
#     res[[method]]$datnum = append(res[[method]]$datnum, dname)
#   }
#   if (i%%100==0) print(i)
# }
# 
# dat_susiesuffnull = readRDS('../susierss_ukb_20210218_REF1000_pve005/susierss_ukb_20210218_REF1000_pve005.2.rds')
# dat_susiesuffnull = dat_susiesuffnull$susie %>% filter(method == 'susie_suff_ERTRUE_ldin_AZFALSE_rcorNA_scalezNA')
# pref = '../../output/susierss_ukb_REF1000_pve005/'
# for (i in 1:nrow(dat_susiesuffnull)) {
#   method = dat_susiesuffnull[i,]$method
#   m = readRDS(paste0(pref, dat_susiesuffnull[i,]$method_susie.output.file, '.rds'))
#   elbo = susie_get_objective(m$fitted[[1]])
#   if (!(method %in% names(res))) {
#     res[[method]] = list(elbo = elbo)
#   } else {
#     res[[method]]$elbo = append(res[[method]]$elbo, elbo)
#   }
#   if (i%%100==0) print(i)
# }
# 
# names(res)[1] = 'susie_initnull'
# names(res)[6] = 'susie_suff_initnull'
# 
# saveRDS(res, paste0(output_susie, '.rds'))

res = readRDS(paste0(output_susie, '.rds'))

res_susie_initnull = do.call(cbind.data.frame, res$susie_initnull) 
res_susie_initlasso = do.call(cbind.data.frame, res$susie_initlasso) 
res_susie_inittrue = do.call(cbind.data.frame, res$susie_initoracle)

idx = which(res_susie_inittrue$elbo - res_susie_initnull$elbo > 1e-2)
res_susie_inittrue$elbo[idx][1:3]
res_susie_initnull$elbo[idx][1:3]

idx = which(res_susie_inittrue$elbo - res_susie_initlasso$elbo > 1e-2)
res_susie_inittrue$elbo[idx][1:3]
res_susie_initlasso$elbo[idx][1:3]

res_susiess_initnull = do.call(cbind.data.frame, res$susie_suff_initnull)
res_susiess_initlasso = do.call(cbind.data.frame, res$susie_suff_initlasso) 
res_susiess_inittrue = do.call(cbind.data.frame, res$susie_suff_initoracle)

idx = which(res_susiess_inittrue$elbo - res_susiess_initnull$elbo > 1e-2)
res_susiess_inittrue$elbo[idx][1:3]
res_susiess_initnull$elbo[idx][1:3]

idx = which(res_susiess_inittrue$elbo - res_susiess_initlasso$elbo > 1e-2)
res_susiess_inittrue$elbo[idx][1:3]
res_susiess_initlasso$elbo[idx][1:3]

res_susie_initnull$elbo[208]
res_susie_initlasso$elbo[208]
res_susie_inittrue$elbo[208]

res_susiess_initnull$elbo[208]
res_susiess_initlasso$elbo[208]
res_susiess_inittrue$elbo[208]


