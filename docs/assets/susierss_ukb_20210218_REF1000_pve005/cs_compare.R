library(dplyr)
library(ggplot2)
library(gridExtra)
library(cowplot)
## Functions
plot_panel = function(dat, quantity, legend = TRUE) {
  p = ggplot(dat, aes_string(x="method", y=quantity[1])) + 
    geom_point(position=position_dodge(.25), size=2.5)
  if (quantity[1] == 'power'){
    p = p + geom_errorbar(aes(ymin=power-power_se, ymax=power+power_se), 
                          width=.2, position=position_dodge(.25))
  }
  if (quantity[1] == 'coverage') {
    p = p + geom_errorbar(aes(ymin=coverage-coverage_se, ymax=coverage+coverage_se), 
                          width=.2, position=position_dodge(.25)) + 
      geom_hline(yintercept = 0.95, colour = 'gray') 
  }
  p = p + labs(x = "method", y = "") + theme_cowplot() + 
    background_grid(major = "x", minor = "none") + 
    ggtitle(quantity[2]) + theme(axis.title.x = element_text(size = 8),
                                 axis.text.x = element_text(angle = 60, size=6,hjust = 1),
                                 plot.title = element_text(size=10))
  if (!legend) p = p + theme(legend.position="none")
  return(p)
}
## parameters
input = 'susierss_ukb_20210218_REF1000_pve005.2.rds'

output_susierss = 'susierss_ukb_20210218_REF1000_pve005_cs/susierss_ukb_cs'
output_fm = 'susierss_ukb_20210218_REF1000_pve005_cs/finemapv4_ukb_cs'

# dat = readRDS(input)
# 
# ## susierss
# methods = unique(dat$susie$method)
# methods = methods[-grep('ldrefin', methods)]
# methods = methods[-grep('AZFALSE_rcorFALSE', methods)]
# res = list()
# for(met in methods){
#   print(met)
#   dat_sub = dat$susie %>% filter(method == met)
#   converged = unlist(dat_sub$score_susie.converged)
#   total = unlist(dat_sub$score_susie.total) * converged
#   valid = unlist(dat_sub$score_susie.valid) * converged
#   sizes = unlist(dat_sub$score_susie.size[converged])
#   purity = unlist(dat_sub$score_susie.purity[converged])
#   expected = unlist(dat_sub$sim_gaussian.n_signal) * converged
#   sigma2 = unlist(dat_sub$score_susie.sigma2) * converged
#   cat('sigma2:\n')
#   print(summary(sigma2))
#   if(grepl('lambda', met)){
#     lambda = unlist(dat_sub$score_susie.lamb) * converged
#     cat('lambda:\n')
#     print(summary(lambda))
#   }
#   res[[met]] = list(total = sum(total), valid = sum(valid),
#                     size = median(sizes, na.rm=T), purity = median(purity, na.rm=T),
#                     expected = sum(expected), nonconverged = sum(!converged))
#   res[[met]]$power = res[[met]]$valid/res[[met]]$expected
#   res[[met]]$coverage = res[[met]]$valid/res[[met]]$total
#   res[[met]]$power_se = sqrt(res[[met]]$power * (1-res[[met]]$power) / res[[met]]$expected)
#   res[[met]]$power_se[is.nan(res[[met]]$power_se)] = 0
#   res[[met]]$coverage_se = sqrt(res[[met]]$coverage * (1-res[[met]]$coverage) / res[[met]]$total)
# }
# saveRDS(res, paste0(output_susierss, '.rds'))
# 
# ## fm
# methods = unique(dat$fmv4$method)
# res = list()
# for(met in methods){
#   print(met)
#   dat_sub = dat$fmv4 %>% filter(method == met)
#   total = unlist(dat_sub$score_finemapv4.total)
#   valid = unlist(dat_sub$score_finemapv4.valid)
#   sizes = unlist(dat_sub$score_finemapv4.size)
#   expected = unlist(dat_sub$sim_gaussian.n_signal)
#   
#   res[[met]] = list(total = sum(total), valid = sum(valid),
#                     size = median(sizes, na.rm=T), purity = NA, 
#                     expected = sum(expected), nonconverged = NA)
#   res[[met]]$power = res[[met]]$valid/res[[met]]$expected
#   res[[met]]$coverage = res[[met]]$valid/res[[met]]$total
#   res[[met]]$power_se = sqrt(res[[met]]$power * (1-res[[met]]$power) / res[[met]]$expected)
#   res[[met]]$power_se[is.nan(res[[met]]$power_se)] = 0
#   res[[met]]$coverage_se = sqrt(res[[met]]$coverage * (1-res[[met]]$coverage) / res[[met]]$total)
# }
# saveRDS(res, paste0(output_fm, '.rds'))

# for(s in 1:3){
#   methods = unique(dat$susie$method)
#   methods = methods[-grep('ldrefin', methods)]
#   methods = methods[-grep('AZFALSE_rcorFALSE', methods)]
#   res = list()
#   for(met in methods){
#     print(met)
#     dat_sub = dat$susie %>% filter(method == met, sim_gaussian.n_signal == s)
#     converged = unlist(dat_sub$score_susie.converged)
#     total = unlist(dat_sub$score_susie.total) * converged
#     valid = unlist(dat_sub$score_susie.valid) * converged
#     sizes = unlist(dat_sub$score_susie.size[converged])
#     purity = unlist(dat_sub$score_susie.purity[converged])
#     expected = unlist(dat_sub$sim_gaussian.n_signal) * converged
# 
#     res[[met]] = list(total = sum(total), valid = sum(valid),
#                       size = median(sizes, na.rm=T), purity = median(purity, na.rm=T),
#                       expected = sum(expected), nonconverged = sum(!converged))
#     res[[met]]$power = res[[met]]$valid/res[[met]]$expected
#     res[[met]]$coverage = res[[met]]$valid/res[[met]]$total
#     res[[met]]$power_se = sqrt(res[[met]]$power * (1-res[[met]]$power) / res[[met]]$expected)
#     res[[met]]$power_se[is.nan(res[[met]]$power_se)] = 0
#     res[[met]]$coverage_se = sqrt(res[[met]]$coverage * (1-res[[met]]$coverage) / res[[met]]$total)
#   }
#   saveRDS(res, paste0(output_susierss, '_s', s,'.rds'))
#   
#   methods = unique(dat$fmv4$method)
#   res = list()
#   for(met in methods){
#     print(met)
#     dat_sub = dat$fmv4 %>% filter(method == met, sim_gaussian.n_signal == s)
#     total = unlist(dat_sub$score_finemapv4.total)
#     valid = unlist(dat_sub$score_finemapv4.valid)
#     sizes = unlist(dat_sub$score_finemapv4.size)
#     expected = unlist(dat_sub$sim_gaussian.n_signal)
# 
#     res[[met]] = list(total = sum(total), valid = sum(valid),
#                       size = median(sizes, na.rm=T), purity = NA,
#                       expected = sum(expected), nonconverged = NA)
#     res[[met]]$power = res[[met]]$valid/res[[met]]$expected
#     res[[met]]$coverage = res[[met]]$valid/res[[met]]$total
#     res[[met]]$power_se = sqrt(res[[met]]$power * (1-res[[met]]$power) / res[[met]]$expected)
#     res[[met]]$power_se[is.nan(res[[met]]$power_se)] = 0
#     res[[met]]$coverage_se = sqrt(res[[met]]$coverage * (1-res[[met]]$coverage) / res[[met]]$total)
#   }
#   saveRDS(res, paste0(output_fm, '_s', s,'.rds'))
# }

rename = list("susie_suff_ERTRUE_ldin_AZFALSE_rcorNA_scalezNA" = 'susie_suff',
              "susie_rss_ERTRUE_ldin_AZFALSE_rcorTRUE_scalezNA" = "susie_rss",
              "susie_rss_ERTRUE_ldin_AZTRUE_rcorTRUE_scalezNA" = "susie_rss_AZ_cor",
              "susie_rss_ERTRUE_ldin_AZTRUE_rcorFALSE_scalezNA" = "susie_rss_AZ_cov",
              "susie_rss_ERTRUE_ldrefout_AZFALSE_rcorTRUE_scalezNA" = "susie_rss_ldref",
              "susie_rss_ERTRUE_ldrefout_AZTRUE_rcorTRUE_scalezNA" = "susie_rss_ldref_AZ_cor",
              "susie_rss_ERTRUE_ldrefout_AZTRUE_rcorFALSE_scalezNA" = "susie_rss_ldref_AZ_cov",
              "susie_rss_scale_ERTRUE_ldrefout_AZTRUE_rcorTRUE_scalezmax" = "susie_rss_ldref_AZ_cor_scalemax",
              "susie_rss_scale_ERTRUE_ldrefout_AZTRUE_rcorTRUE_scalezratio" = "susie_rss_ldref_AZ_cor_scaleratio",
              "susie_rss_scale_ERTRUE_ldrefout_AZTRUE_rcorFALSE_scalezmax" = "susie_rss_ldref_AZ_cov_scalemax",
              "susie_rss_scale_ERTRUE_ldrefout_AZTRUE_rcorFALSE_scalezratio" = "susie_rss_ldref_AZ_cov_scaleratio",
              "susie_rss_ERTRUE_ldrefin_AZFALSE_rcorTRUE_scalezNA" = "susie_rss_ldrefin",
              "susie_rss_ERTRUE_ldrefin_AZTRUE_rcorTRUE_scalezNA" = "susie_rss_ldrefin_AZ_cor",
              "susie_rss_ERTRUE_ldrefin_AZTRUE_rcorFALSE_scalezNA" = "susie_rss_ldrefin_AZ_cov",
              "susie_rss_scale_ERTRUE_ldrefin_AZTRUE_rcorTRUE_scalezmax" = "susie_rss_ldrefin_AZ_cor_scalemax",
              "susie_rss_scale_ERTRUE_ldrefin_AZTRUE_rcorFALSE_scalezmax" = "susie_rss_ldrefin_AZ_cov_scalemax",
              "susie_rss_scale_ERTRUE_ldrefin_AZTRUE_rcorTRUE_scalezratio" = "susie_rss_ldrefin_AZ_cor_scaleratio",
              "susie_rss_scale_ERTRUE_ldrefin_AZTRUE_rcorFALSE_scalezratio" = "susie_rss_ldrefin_AZ_cov_scaleratio",
              "susie_rss_lambda_ERTRUE_ldin_AZFALSE_rcorTRUE_scalezNA" = "susie_rss_lambda",
              "susie_rss_lambda_ERTRUE_ldin_AZTRUE_rcorFALSE_scalezNA" = "susie_rss_lambda_AZ_cov",
              "susie_rss_lambda_ERTRUE_ldin_AZTRUE_rcorTRUE_scalezNA" = "susie_rss_lambda_AZ_cor",
              "susie_rss_lambda_ERTRUE_ldrefout_AZFALSE_rcorTRUE_scalezNA" = "susie_rss_lambda_ldref",
              "susie_rss_lambda_ERTRUE_ldrefout_AZTRUE_rcorTRUE_scalezNA" = "susie_rss_lambda_ldref_AZ_cor",
              "susie_rss_lambda_ERTRUE_ldrefout_AZTRUE_rcorFALSE_scalezNA" = "susie_rss_lambda_ldref_AZ_cov",
              "susie_rss_lambda_scale_ERTRUE_ldrefout_AZTRUE_rcorTRUE_scalezmax" = "susie_rss_lambda_ldref_AZ_cor_scalemax",
              "susie_rss_lambda_scale_ERTRUE_ldrefout_AZTRUE_rcorFALSE_scalezmax" = "susie_rss_lambda_ldref_AZ_cov_scalemax",
              "susie_rss_lambda_scale_ERTRUE_ldrefout_AZTRUE_rcorTRUE_scalezratio" = "susie_rss_lambda_ldref_AZ_cor_scaleratio",
              "susie_rss_lambda_scale_ERTRUE_ldrefout_AZTRUE_rcorFALSE_scalezratio" = "susie_rss_lambda_ldref_AZ_cov_scaleratio",
              "susie_rss_lambda_ERTRUE_ldrefin_AZFALSE_rcorTRUE_scalezNA" = "susie_rss_lambda_ldrefin",
              "susie_rss_lambda_ERTRUE_ldrefin_AZTRUE_rcorTRUE_scalezNA" = "susie_rss_lambda_ldrefin_AZ_cor",
              "susie_rss_lambda_ERTRUE_ldrefin_AZTRUE_rcorFALSE_scalezNA" = "susie_rss_lambda_ldrefin_AZ_cov",
              "susie_rss_lambda_scale_ERTRUE_ldrefin_AZTRUE_rcorTRUE_scalezmax" = "susie_rss_lambda_ldrefin_AZ_cor_scalemax",
              "susie_rss_lambda_scale_ERTRUE_ldrefin_AZTRUE_rcorFALSE_scalezmax" = "susie_rss_lambda_ldrefin_AZ_cov_scalemax",
              "susie_rss_lambda_scale_ERTRUE_ldrefin_AZTRUE_rcorTRUE_scalezratio" = "susie_rss_lambda_ldrefin_AZ_cor_scaleratio",
              "susie_rss_lambda_scale_ERTRUE_ldrefin_AZTRUE_rcorFALSE_scalezratio" = "susie_rss_lambda_ldrefin_AZ_cov_scaleratio",
              "FINEMAPv1.4_ldin_AZFALSE_scalezNA" = 'FINEMAPv1.4',
              "FINEMAPv1.4_ldrefout_AZFALSE_scalezNA" = 'FINEMAPv1.4_ldref',
              "FINEMAPv1.4_ldrefout_AZTRUE_scalezNA" = 'FINEMAPv1.4_ldref_AZ_cor',
              "FINEMAPv1.4_ldrefout_AZTRUE_scalezmax" = 'FINEMAPv1.4_ldref_AZ_cor_scalemax',
              "FINEMAPv1.4_ldrefout_AZTRUE_scalezratio" = 'FINEMAPv1.4_ldref_AZ_cor_scaleratio')

res_susierss = readRDS(paste0(output_susierss, '.rds'))
res_fm = readRDS(paste0(output_fm, '.rds'))
res = c(res_susierss, res_fm)
rates = matrix(unlist(res), length(res), byrow = T)
rownames(rates) = names(res)
colnames(rates) = c('discoveries', 'valid', 'size', 'purity', 'expected', 'nonconverge',
                    'power', 'coverage', 'power_se', 'coverage_se')
rates = as.data.frame(rates)
rates$method = rownames(rates)

rates_ERTRUE = rates[-grep('ERFALSE', rates$method),]
rates_ERTRUE = rates_ERTRUE[-grep('ldin_AZTRUE', rates_ERTRUE$method),]
rates_ERTRUE = rates_ERTRUE[-c(grep('scalezmax', rates_ERTRUE$method), grep('scalezratio', rates_ERTRUE$method)),]
rates_ERTRUE$method = sapply(rownames(rates_ERTRUE), function(x) rename[[x]])

rates_ERTRUE$method = factor(rates_ERTRUE$method, levels = c('susie_suff', 'susie_rss', 'susie_rss_ldref',
                                                             'susie_rss_ldref_AZ_cor', 'susie_rss_ldref_AZ_cov',
                                                             'susie_rss_lambda', 'susie_rss_lambda_ldref',
                                                             'susie_rss_lambda_ldref_AZ_cor','susie_rss_lambda_ldref_AZ_cov',
                                                             'FINEMAPv1.4', 'FINEMAPv1.4_ldref', 'FINEMAPv1.4_ldref_AZ_cor'))
p1 = plot_panel(rates_ERTRUE, c('coverage', 'coverage'), legend=F)
p2 = plot_panel(rates_ERTRUE, c('power', 'power'), legend=F)
p3 = plot_panel(rates_ERTRUE, c('size', 'median number of variables'), legend=F)
p4 = plot_panel(rates_ERTRUE, c('purity', 'median of purity'), legend=F)
output = 'susierss_ukb_20210218_REF1000_pve005_cs/ukb_cs'
pdf(paste0(output, '_plots.pdf'), width=12, height=3)
grid.arrange(p1,p2,p3,p4, ncol=4, widths=c(3,3,3,3))
dev.off()
system(paste0("convert -flatten -density 120 ", paste0(output, '_plots.pdf'), " ", paste0(output, '_plots.png')))

for(s in 1:3){
  res_susierss = readRDS(paste0(output_susierss, '_s',s,'.rds'))
  res_fm = readRDS(paste0(output_fm, '_s', s,'.rds'))
  res = c(res_susierss, res_fm)
  rates = matrix(unlist(res), length(res), byrow = T)
  rownames(rates) = names(res)
  colnames(rates) = c('discoveries', 'valid', 'size', 'purity', 'expected', 'nonconverge',
                      'power', 'coverage', 'power_se', 'coverage_se')
  rates = as.data.frame(rates)
  rates$method = rownames(rates)
  
  rates_ERTRUE = rates[-grep('ERFALSE', rates$method),]
  rates_ERTRUE = rates_ERTRUE[-grep('ldin_AZTRUE', rates_ERTRUE$method),]
  rates_ERTRUE = rates_ERTRUE[-c(grep('scalezmax', rates_ERTRUE$method), grep('scalezratio', rates_ERTRUE$method)),]
  rates_ERTRUE$method = sapply(rownames(rates_ERTRUE), function(x) rename[[x]])
  
  rates_ERTRUE$method = factor(rates_ERTRUE$method, levels = c('susie_suff', 'susie_rss', 'susie_rss_ldref',
                                                               'susie_rss_ldref_AZ_cor', 'susie_rss_ldref_AZ_cov',
                                                               'susie_rss_lambda', 'susie_rss_lambda_ldref',
                                                               'susie_rss_lambda_ldref_AZ_cor','susie_rss_lambda_ldref_AZ_cov',
                                                               'FINEMAPv1.4', 'FINEMAPv1.4_ldref', 'FINEMAPv1.4_ldref_AZ_cor'))
  p1 = plot_panel(rates_ERTRUE, c('coverage', 'coverage'), legend=F)
  p2 = plot_panel(rates_ERTRUE, c('power', 'power'), legend=F)
  p3 = plot_panel(rates_ERTRUE, c('size', 'median number of variables'), legend=F)
  p4 = plot_panel(rates_ERTRUE, c('purity', 'median of purity'), legend=F)
  pdf(paste0(output, '_s',s,'_plots.pdf'), width=12, height=3)
  grid.arrange(p1,p2,p3,p4, ncol=4, widths=c(3,3,3,3))
  dev.off()
  system(paste0("convert -flatten -density 120 ", paste0(output, '_s',s,'_plots.pdf'), " ", paste0(output, '_s',s,'_plots.png')))
}

