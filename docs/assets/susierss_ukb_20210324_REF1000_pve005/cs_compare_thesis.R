library(dplyr)
library(ggplot2)
library(gridExtra)
library(cowplot)
## Functions
plot_panel = function(dat, quantity, legend = TRUE) {
  p = ggplot(dat, aes_string(x="LD", y=quantity[1], fill="Method")) + 
    scale_color_manual("Method", values = c("SuSiE-suff" = "#000000", "SuSiE-RSS" = "#A60628", "FINEMAP L4" = "#348ABD")) + 
    geom_point(aes(colour=Method), position=position_dodge(.5), size=2.5)
  if (quantity[1] == 'power'){
    p = p + geom_errorbar(aes(ymin=power-power_se, ymax=power+power_se, color=Method), 
                          width=.2, position=position_dodge(.5))
  }
  if (quantity[1] == 'coverage') {
    p = p + geom_errorbar(aes(ymin=coverage-coverage_se, ymax=coverage+coverage_se, color=Method), 
                          width=.2, position=position_dodge(.5)) + 
      geom_hline(yintercept = 0.95, colour = 'gray') 
  }
  p = p + labs(x = "", y = "") + theme_cowplot() + 
    background_grid(major = "x", minor = "none") + 
    ggtitle(quantity[2]) + theme(axis.title.x = element_text(size = 10),
                                 axis.text.x = element_text(angle = 60, size=8,hjust = 1),
                                 plot.title = element_text(size=11))
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

output_susierss_500 = '../susierss_ukb_20210324_REF500_pve005/susierss_ukb_20210324_REF500_pve005_cs/susierss_ukb_cs_remdatallTRUE'
output_fm_500 = '../susierss_ukb_20210324_REF500_pve005/susierss_ukb_20210324_REF500_pve005_cs/finemapv4_ukb_cs_remdatallTRUE'

res_susierss = readRDS(paste0(output_susierss, '.rds'))
res_fm = readRDS(paste0(output_fm, '.rds'))
res_susierss_500 = readRDS(paste0(output_susierss_500, '.rds'))
res_fm_500 = readRDS(paste0(output_fm_500, '.rds'))

res_susierss = res_susierss[grep('refineTRUE', names(res_susierss))]
names(res_susierss) = gsub('ldrefout', 'ldref1000', names(res_susierss))
res_fm = res_fm[grep('pure', names(res_fm))]
names(res_fm) = gsub('ldrefout', 'ldref1000', names(res_fm))

res_susierss_500 = res_susierss_500[-grep('susie_suff', names(res_susierss_500))]
res_susierss_500 = res_susierss_500[grep('ldrefout', names(res_susierss_500))]
res_susierss_500 = res_susierss_500[grep('refineTRUE', names(res_susierss_500))]
names(res_susierss_500) = gsub('ldrefout', 'ldref500', names(res_susierss_500))
res_fm_500 = res_fm_500[grep('finemapv4L4', names(res_fm_500))]
res_fm_500 = res_fm_500[grep('ldrefout', names(res_fm_500))]
res_fm_500 = res_fm_500[grep('pure', names(res_fm_500))]
names(res_fm_500) = gsub('ldrefout', 'ldref500', names(res_fm_500))

res = c(res_susierss, res_susierss_500, res_fm, res_fm_500)
rates = matrix(unlist(res), length(res), byrow = T)
rownames(rates) = names(res)
colnames(rates) = c('discoveries', 'valid', 'size', 'purity', 'avgr2','expected', 'nonconverge',
                    'power', 'coverage', 'power_se', 'coverage_se')
rates = as.data.frame(rates)
rates$method = rownames(rates)
rates = rates[-c(2:8, 11, 13, 15, 17:19, 26:33, 36, 38, 40, 42:44),]

methods = rates$method
rename_mets = gsub('_ldin', '', methods)
rename_mets = gsub('_ldrefout', '_ldref1000', rename_mets)
rename_mets = gsub('_AZTRUE', '_AZ', rename_mets)
rename_mets = gsub('_AZFALSE', '', rename_mets)
rename_mets = gsub('_ERNA', '', rename_mets)
rename_mets = gsub('_lamb0$', '', rename_mets)
rename_mets = gsub('finemapv4L4', 'FINEMAP L4', rename_mets)
rename_mets = gsub('finemapv4', 'FINEMAP', rename_mets)
rename_mets = gsub('susie_suff', 'SuSiE-suff', rename_mets)
rename_mets = gsub('susie_rss', 'SuSiE-RSS', rename_mets)
rename_mets = gsub('ld', 'LD', rename_mets)
rename_mets = gsub('lamb0.001', 's=0.001', rename_mets)
rename_mets = gsub('lambmlelikelihood', 'estimated s', rename_mets)
rename_mets = gsub('_', ' ', rename_mets)
rename_mets = gsub(' pure', '', rename_mets)
rename = as.list(rename_mets)
names(rename) = methods

rates_d1 = rates[-grep('AZTRUE', rates$method),]
rates_d1 = rates_d1[-grep('lambestimate', rates_d1$method),]

rates_d1$method = sapply(rownames(rates_d1), function(x) rename[[x]])
rates_d1$method = gsub(' refineTRUE', '', rates_d1$method)
rates_d1$method = gsub(' lamb0$', '', rates_d1$method)
rates_d1$method = gsub(' ERFALSE', '', rates_d1$method)
rates_d1$method = factor(rates_d1$method,
                         levels = c('SuSiE-suff','SuSiE-RSS',
                                    'SuSiE-RSS LDref1000', "SuSiE-RSS LDref1000 s=0.001", 
                                    "SuSiE-RSS LDref1000 estimated s",
                                    "SuSiE-RSS LDref500", "SuSiE-RSS LDref500 s=0.001", "SuSiE-RSS LDref500 estimated s",
                                    "FINEMAP L4", "FINEMAP L4 LDref1000", 
                                    "FINEMAP L4 LDref1000 s=0.001", "FINEMAP L4 LDref1000 estimated s",
                                    "FINEMAP L4 LDref500", "FINEMAP L4 LDref500 s=0.001", "FINEMAP L4 LDref500 estimated s"))
rates_d1$LD = factor(c('In-sample LD', 'In-sample LD', 'LD ref1000', 'LD ref1000 s = 0.001', 'LD ref1000 estimated s',
                       'LD ref500', 'LD ref500 s = 0.001', 'LD ref500 estimated s', 
                       'In-sample LD', 'LD ref1000', 'LD ref1000 s = 0.001', 'LD ref1000 estimated s',
                       'LD ref500', 'LD ref500 s = 0.001', 'LD ref500 estimated s'),
                     levels = c('In-sample LD', 'LD ref1000', 'LD ref1000 s = 0.001', 'LD ref1000 estimated s',
                                'LD ref500', 'LD ref500 s = 0.001', 'LD ref500 estimated s'))
rates_d1$Method = factor(c('SuSiE-suff', 'SuSiE-RSS', 'SuSiE-RSS', 'SuSiE-RSS', 'SuSiE-RSS', 'SuSiE-RSS', 'SuSiE-RSS',
                           'SuSiE-RSS', 'FINEMAP L4', 'FINEMAP L4', 'FINEMAP L4', 'FINEMAP L4', 'FINEMAP L4',
                           'FINEMAP L4', 'FINEMAP L4'), levels = c('SuSiE-suff', 'SuSiE-RSS', 'FINEMAP L4'))

p1 = plot_panel(rates_d1, c('coverage', 'coverage'), legend=T)
p2 = plot_panel(rates_d1, c('power', 'power'), legend=T)
p3 = plot_panel(rates_d1, c('size', 'median number of variables'), legend=T)
p4 = plot_panel(rates_d1, c('purity', 'median of purity'), legend=T)

library(ggpubr)
output = 'susierss_ukb_20210324_cs_thesis/ukb_cs_refine'
pdf(paste0(output, '_plots.pdf'), width=8, height=8)
ggarrange(p1,p2,p3,p4, ncol=2, nrow=2, widths=c(4,4), heights=c(4,4), common.legend = TRUE, legend="right")
dev.off()
system(paste0("convert -flatten -density 120 ", paste0(output, '_plots.pdf'), " ", paste0(output, '_plots.png')))



