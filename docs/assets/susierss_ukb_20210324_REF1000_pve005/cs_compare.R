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
input = 'susierss_ukb_20210324_REF1000_pve005.2.rds'

output_susierss = 'susierss_ukb_20210324_REF1000_pve005_cs/susierss_ukb_cs'
output_fm = 'susierss_ukb_20210324_REF1000_pve005_cs/finemapv4_ukb_cs'

# dat = readRDS(input)
# 
# ## susierss
# methods = unique(dat$susie$method)
# # methods = methods[-grep('ldrefin', methods)]
# methods = methods[-grep('_ERTRUE', methods)]
# res = list()
# for(met in methods){
#   print(met)
#   dat_sub = dat$susie %>% filter(method == met)
#   converged = unlist(dat_sub$score_susie.converged)
#   converged[is.na(converged)] = FALSE
#   total = unlist(dat_sub$score_susie.total) * converged
#   valid = unlist(dat_sub$score_susie.valid) * converged
#   sizes = unlist(dat_sub$score_susie.size[converged])
#   purity = unlist(dat_sub$score_susie.purity[converged])
#   avgr2 = unlist(dat_sub$score_susie.avgr2[converged])
#   expected = unlist(dat_sub$sim_gaussian.n_signal) * converged
#   res[[met]] = list(total = sum(total), valid = sum(valid), size = median(sizes, na.rm=T),
#                     purity = median(purity, na.rm=T), avgr2 = mean(avgr2, na.rm=T),
#                     expected = sum(expected), nonconverged = sum(!converged))
#   res[[met]]$power = res[[met]]$valid/res[[met]]$expected
#   res[[met]]$coverage = res[[met]]$valid/res[[met]]$total
#   res[[met]]$power_se = sqrt(res[[met]]$power * (1-res[[met]]$power) / res[[met]]$expected)
#   res[[met]]$power_se[is.nan(res[[met]]$power_se)] = 0
#   res[[met]]$coverage_se = sqrt(res[[met]]$coverage * (1-res[[met]]$coverage) / res[[met]]$total)
# }
# saveRDS(res, paste0(output_susierss, '.rds'))
# #
# ## fm
# methods = unique(dat$fmv4$method)
# res = list()
# for(met in methods){
#   print(met)
#   dat_sub = dat$fmv4 %>% filter(method == met)
#   purity = unlist(dat_sub$score_finemapv4.purity)
#   avgr2 = unlist(dat_sub$score_finemapv4.avgr2)
#   total = unlist(dat_sub$score_finemapv4.total)
#   valid = unlist(dat_sub$score_finemapv4.valid)
#   sizes = unlist(dat_sub$score_finemapv4.size)
#   expected = unlist(dat_sub$sim_gaussian.n_signal)
# 
#   res[[met]] = list(total = sum(total), valid = sum(valid), size = median(sizes, na.rm=T),
#                     purity = median(purity, na.rm=T), avgr2 = mean(avgr2, na.rm=T),
#                     expected = sum(expected), nonconverged = NA)
#   res[[met]]$power = res[[met]]$valid/res[[met]]$expected
#   res[[met]]$coverage = res[[met]]$valid/res[[met]]$total
#   res[[met]]$power_se = sqrt(res[[met]]$power * (1-res[[met]]$power) / res[[met]]$expected)
#   res[[met]]$power_se[is.nan(res[[met]]$power_se)] = 0
#   res[[met]]$coverage_se = sqrt(res[[met]]$coverage * (1-res[[met]]$coverage) / res[[met]]$total)
# 
#   # filter for purity
#   purity = unlist(dat_sub$score_finemapv4.purity_pure)
#   avgr2 = unlist(dat_sub$score_finemapv4.avgr2_pure)
#   total = unlist(dat_sub$score_finemapv4.total_pure)
#   valid = unlist(dat_sub$score_finemapv4.valid_pure)
#   sizes = unlist(dat_sub$score_finemapv4.size_pure)
#   expected = unlist(dat_sub$sim_gaussian.n_signal)
# 
#   metp = paste0(met, '_pure')
#   res[[metp]] = list(total = sum(total), valid = sum(valid), size = median(sizes, na.rm=T),
#                      purity = median(purity, na.rm=T), avgr2 = mean(avgr2, na.rm=T),
#                      expected = sum(expected), nonconverged = NA)
#   res[[metp]]$power = res[[metp]]$valid/res[[metp]]$expected
#   res[[metp]]$coverage = res[[metp]]$valid/res[[metp]]$total
#   res[[metp]]$power_se = sqrt(res[[metp]]$power * (1-res[[metp]]$power) / res[[metp]]$expected)
#   res[[metp]]$power_se[is.nan(res[[metp]]$power_se)] = 0
#   res[[metp]]$coverage_se = sqrt(res[[metp]]$coverage * (1-res[[metp]]$coverage) / res[[metp]]$total)
# }
# saveRDS(res, paste0(output_fm, '.rds'))
# 
# for(s in 1:3){
#   methods = unique(dat$susie$method)
#   methods = methods[-grep('_ERTRUE', methods)]
#   res = list()
#   for(met in methods){
#     print(met)
#     dat_sub = dat$susie %>% filter(method == met, sim_gaussian.n_signal == s)
#     converged = unlist(dat_sub$score_susie.converged)
#     converged[is.na(converged)] = FALSE
#     total = unlist(dat_sub$score_susie.total) * converged
#     valid = unlist(dat_sub$score_susie.valid) * converged
#     sizes = unlist(dat_sub$score_susie.size[converged])
#     purity = unlist(dat_sub$score_susie.purity[converged])
#     expected = unlist(dat_sub$sim_gaussian.n_signal) * converged
# 
#     res[[met]] = list(total = sum(total), valid = sum(valid), size = median(sizes, na.rm=T),
#                       purity = median(purity, na.rm=T), avgr2 = mean(avgr2, na.rm=T),
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
#     purity = unlist(dat_sub$score_finemapv4.purity)
#     avgr2 = unlist(dat_sub$score_finemapv4.avgr2)
#     total = unlist(dat_sub$score_finemapv4.total)
#     valid = unlist(dat_sub$score_finemapv4.valid)
#     sizes = unlist(dat_sub$score_finemapv4.size)
#     expected = unlist(dat_sub$sim_gaussian.n_signal)
# 
#     res[[met]] = list(total = sum(total), valid = sum(valid), size = median(sizes, na.rm=T),
#                       purity = median(purity, na.rm=T), avgr2 = mean(avgr2, na.rm=T),
#                       expected = sum(expected), nonconverged = NA)
#     res[[met]]$power = res[[met]]$valid/res[[met]]$expected
#     res[[met]]$coverage = res[[met]]$valid/res[[met]]$total
#     res[[met]]$power_se = sqrt(res[[met]]$power * (1-res[[met]]$power) / res[[met]]$expected)
#     res[[met]]$power_se[is.nan(res[[met]]$power_se)] = 0
#     res[[met]]$coverage_se = sqrt(res[[met]]$coverage * (1-res[[met]]$coverage) / res[[met]]$total)
# 
#     # filter for purity
#     purity = unlist(dat_sub$score_finemapv4.purity_pure)
#     avgr2 = unlist(dat_sub$score_finemapv4.avgr2_pure)
#     total = unlist(dat_sub$score_finemapv4.total_pure)
#     valid = unlist(dat_sub$score_finemapv4.valid_pure)
#     sizes = unlist(dat_sub$score_finemapv4.size_pure)
#     expected = unlist(dat_sub$sim_gaussian.n_signal)
# 
#     metp = paste0(met, '_pure')
#     res[[metp]] = list(total = sum(total), valid = sum(valid), size = median(sizes, na.rm=T),
#                        purity = median(purity, na.rm=T), avgr2 = mean(avgr2, na.rm=T),
#                        expected = sum(expected), nonconverged = NA)
#     res[[metp]]$power = res[[metp]]$valid/res[[metp]]$expected
#     res[[metp]]$coverage = res[[metp]]$valid/res[[metp]]$total
#     res[[metp]]$power_se = sqrt(res[[metp]]$power * (1-res[[metp]]$power) / res[[metp]]$expected)
#     res[[metp]]$power_se[is.nan(res[[metp]]$power_se)] = 0
#     res[[metp]]$coverage_se = sqrt(res[[metp]]$coverage * (1-res[[metp]]$coverage) / res[[metp]]$total)
#   }
#   saveRDS(res, paste0(output_fm, '_s', s,'.rds'))
# }

res_susierss = readRDS(paste0(output_susierss, '.rds'))
res_fm = readRDS(paste0(output_fm, '.rds'))
res = c(res_susierss, res_fm)
rates = matrix(unlist(res), length(res), byrow = T)
rownames(rates) = names(res)
colnames(rates) = c('discoveries', 'valid', 'size', 'purity', 'avgr2','expected', 'nonconverge',
                    'power', 'coverage', 'power_se', 'coverage_se')
rates = as.data.frame(rates)
rates$method = rownames(rates)
# we used purified finemap CS
rates = rates[c(grep('susie', rates$method), grep('pure', rates$method)),]
rates = rates[c(grep('susie', rates$method), grep('finemapv4L4', rates$method)),]
rates = rates[-c(2:8, 10:16, 19, 21, 23, 27, 29, 31, 33:36, 37, 39, 
                 43, 45, 47, 49),]
rates = rates[-c(6,11,18),]

methods = rates$method
rename_mets = gsub('_ldin', '', methods)
rename_mets = gsub('_ldrefout', '_ldref', rename_mets)
rename_mets = gsub('_ERNA', '', rename_mets)
rename_mets = gsub('_ERFALSE', '', rename_mets)
rename_mets = gsub('_AZFALSE', '', rename_mets)
rename_mets = gsub('_AZTRUE', '_AZ', rename_mets)
rename_mets = gsub('_pure', '', rename_mets)
rename_mets = gsub('finemapv4', 'FINEMAPv1.4', rename_mets)
rename_mets = gsub('lamb0.001', 's=0.001', rename_mets)
rename_mets = gsub('lambmlelikelihood', 'estimated s', rename_mets)
rename = as.list(rename_mets)
names(rename) = methods

d1 = c(grep('refineTRUE', rates$method), grep('finemap', rates$method)) # 3 steps and all finemap
d2 = c(grep('refineFALSE', rates$method), grep('finemap', rates$method)) # original and all finemap

rates_d1 = rates[d1,]
rates_d2 = rates[d2,]

rates_d1$method = sapply(rownames(rates_d1), function(x) rename[[x]])
rates_d1$method = gsub('_refineTRUE', '', rates_d1$method)
rates_d1$method = gsub('_lamb0$', '', rates_d1$method)
rates_d1$method = factor(rates_d1$method,
                         levels = c('susie_suff',
                                    'susie_rss', 'susie_rss_ldref', 'susie_rss_ldref_s=0.001',
                                    'susie_rss_ldref_estimated s', 'susie_rss_ldref_AZ',
                                    'FINEMAPv1.4L4', 'FINEMAPv1.4L4_ldref', 'FINEMAPv1.4L4_ldref_s=0.001',
                                    'FINEMAPv1.4L4_ldref_estimated s', 'FINEMAPv1.4L4_ldref_AZ'))

p1 = plot_panel(rates_d1, c('coverage', 'coverage'), legend=F)
p2 = plot_panel(rates_d1, c('power', 'power'), legend=F)
p3 = plot_panel(rates_d1, c('size', 'median number of variables'), legend=F)
p4 = plot_panel(rates_d1, c('purity', 'median of purity'), legend=F)
output = 'susierss_ukb_20210324_REF1000_pve005_cs/ukb_cs_refine'
pdf(paste0(output, '_plots.pdf'), width=12, height=3)
grid.arrange(p1,p2,p3,p4, ncol=4, widths=c(3,3,3,3))
dev.off()
system(paste0("convert -flatten -density 120 ", paste0(output, '_plots.pdf'), " ", paste0(output, '_plots.png')))

rates_d2$method = sapply(rownames(rates_d2), function(x) rename[[x]])
rates_d2$method = gsub('_refineFALSE', '', rates_d2$method)
rates_d2$method = gsub('_lamb0$', '', rates_d2$method)
rates_d2$method = factor(rates_d2$method,
                         levels = c("susie_suff",
                                    "susie_rss","susie_rss_ldref","susie_rss_ldref_s=0.001",
                                    "susie_rss_ldref_estimated s","susie_rss_ldref_AZ",
                                    "FINEMAPv1.4L4","FINEMAPv1.4L4_ldref","FINEMAPv1.4L4_ldref_s=0.001",
                                    "FINEMAPv1.4L4_ldref_estimated s", "FINEMAPv1.4L4_ldref_AZ"))

p1 = plot_panel(rates_d2, c('coverage', 'coverage'), legend=F)
p2 = plot_panel(rates_d2, c('power', 'power'), legend=F)
p3 = plot_panel(rates_d2, c('size', 'median number of variables'), legend=F)
p4 = plot_panel(rates_d2, c('purity', 'median of purity'), legend=F)
output = 'susierss_ukb_20210324_REF1000_pve005_cs/ukb_cs_original'
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
  colnames(rates) = c('discoveries', 'valid', 'size', 'purity', 'avgr2','expected', 'nonconverge',
                      'power', 'coverage', 'power_se', 'coverage_se')
  rates = as.data.frame(rates)
  rates$method = rownames(rates)
  # we used purified finemap CS
  rates = rates[c(grep('susie', rates$method), grep('pure', rates$method)),]
  rates = rates[c(grep('susie', rates$method), grep('finemapv4L4', rates$method)),]
  rates = rates[-c(2:8, 10:16, 19, 21, 23, 27, 29, 31, 33:36, 37, 39, 
                   43, 45, 47, 49),]
  rates = rates[-c(6,11,18),]
  methods = rates$method
  rename_mets = gsub('_ldin', '', methods)
  rename_mets = gsub('_ldrefout', '_ldref', rename_mets)
  rename_mets = gsub('_ERNA', '', rename_mets)
  rename_mets = gsub('_ERFALSE', '', rename_mets)
  rename_mets = gsub('_AZFALSE', '', rename_mets)
  rename_mets = gsub('_AZTRUE', '_AZ', rename_mets)
  rename_mets = gsub('_pure', '', rename_mets)
  rename_mets = gsub('finemapv4', 'FINEMAPv1.4', rename_mets)
  rename_mets = gsub('lamb0.001', 's=0.001', rename_mets)
  rename_mets = gsub('lambmlelikelihood', 'estimated s', rename_mets)
  rename = as.list(rename_mets)
  names(rename) = methods

  d1 = c(grep('refineTRUE', rates$method), grep('finemap', rates$method)) # 3 steps and all finemap
  d2 = c(grep('refineFALSE', rates$method), grep('finemap', rates$method)) # original and all finemap

  rates_d1 = rates[d1,]
  rates_d2 = rates[d2,]

  rates_d1$method = sapply(rownames(rates_d1), function(x) rename[[x]])
  rates_d1$method = gsub('_refineTRUE', '', rates_d1$method)
  rates_d1$method = gsub('_lamb0$', '', rates_d1$method)
  rates_d1$method = factor(rates_d1$method,
                           levels = c('susie_suff',
                                      'susie_rss', 'susie_rss_ldref', 'susie_rss_ldref_s=0.001',
                                      'susie_rss_ldref_estimated s', "susie_rss_ldref_AZ",
                                      'FINEMAPv1.4L4', 'FINEMAPv1.4L4_ldref', 'FINEMAPv1.4L4_ldref_s=0.001',
                                      'FINEMAPv1.4L4_ldref_estimated s', "FINEMAPv1.4L4_ldref_AZ"))

  p1 = plot_panel(rates_d1, c('coverage', 'coverage'), legend=F)
  p2 = plot_panel(rates_d1, c('power', 'power'), legend=F)
  p3 = plot_panel(rates_d1, c('size', 'median number of variables'), legend=F)
  p4 = plot_panel(rates_d1, c('purity', 'median of purity'), legend=F)
  output = 'susierss_ukb_20210324_REF1000_pve005_cs/ukb_cs_refine'
  pdf(paste0(output, '_s',s,'_plots.pdf'), width=12, height=3)
  grid.arrange(p1,p2,p3,p4, ncol=4, widths=c(3,3,3,3))
  dev.off()
  system(paste0("convert -flatten -density 120 ", paste0(output, '_s',s,'_plots.pdf'), " ", paste0(output, '_s',s,'_plots.png')))

  rates_d2$method = sapply(rownames(rates_d2), function(x) rename[[x]])
  rates_d2$method = gsub('_refineFALSE', '', rates_d2$method)
  rates_d2$method = gsub('_lamb0$', '', rates_d2$method)
  rates_d2$method = factor(rates_d2$method,
                           levels = c("susie_suff",
                                      "susie_rss","susie_rss_ldref","susie_rss_ldref_s=0.001",
                                      "susie_rss_ldref_estimated s", "susie_rss_ldref_AZ",
                                      "FINEMAPv1.4L4","FINEMAPv1.4L4_ldref","FINEMAPv1.4L4_ldref_s=0.001",
                                      "FINEMAPv1.4L4_ldref_estimated s", "FINEMAPv1.4L4_ldref_AZ"))
  
  p1 = plot_panel(rates_d2, c('coverage', 'coverage'), legend=F)
  p2 = plot_panel(rates_d2, c('power', 'power'), legend=F)
  p3 = plot_panel(rates_d2, c('size', 'median number of variables'), legend=F)
  p4 = plot_panel(rates_d2, c('purity', 'median of purity'), legend=F)
  output = 'susierss_ukb_20210324_REF1000_pve005_cs/ukb_cs_original'
  pdf(paste0(output, '_s',s,'_plots.pdf'), width=12, height=3)
  grid.arrange(p1,p2,p3,p4, ncol=4, widths=c(3,3,3,3))
  dev.off()
  system(paste0("convert -flatten -density 120 ", paste0(output, '_s',s,'_plots.pdf'), " ", paste0(output, '_s',s,'_plots.png')))
}

