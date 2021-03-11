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
input = 'susierss_ukb_REF1000_pve005_init_20210307.2.rds'
input_3steps = 'susierss_ukb_REF1000_pve005_init_20210307_susie3steps.2.rds'

output_susie = 'susierss_ukb_REF1000_pve005_init_20210307_cs/susierss_ukb_cs'

dat = readRDS(input)
dat = dat %>% select(-c('ss_init', 'ss_init.fullrank'))
dat3steps = readRDS(input_3steps)
colnames(dat3steps) = colnames(dat)
dat = rbind(dat, dat3steps)

## susie
methods = unique(dat$method)
res = list()
for(met in methods){
  print(met)
  dat_sub = dat %>% filter(method == met)
  converged = unlist(dat_sub$score_susie.converged)
  total = unlist(dat_sub$score_susie.total) * converged
  valid = unlist(dat_sub$score_susie.valid) * converged
  sizes = unlist(dat_sub$score_susie.size[converged])
  purity = unlist(dat_sub$score_susie.purity[converged])
  expected = unlist(dat_sub$sim_gaussian.n_signal) * converged
  sigma2 = unlist(dat_sub$score_susie.sigma2) * converged
  cat('sigma2:\n')
  print(summary(sigma2))
  if(grepl('lambda', met)){
    lambda = unlist(dat_sub$score_susie.lamb) * converged
    cat('lambda:\n')
    print(summary(lambda))
  }
  res[[met]] = list(total = sum(total), valid = sum(valid),
                    size = median(sizes, na.rm=T), purity = median(purity, na.rm=T),
                    expected = sum(expected), nonconverged = sum(!converged))
  res[[met]]$power = res[[met]]$valid/res[[met]]$expected
  res[[met]]$coverage = res[[met]]$valid/res[[met]]$total
  res[[met]]$power_se = sqrt(res[[met]]$power * (1-res[[met]]$power) / res[[met]]$expected)
  res[[met]]$power_se[is.nan(res[[met]]$power_se)] = 0
  res[[met]]$coverage_se = sqrt(res[[met]]$coverage * (1-res[[met]]$coverage) / res[[met]]$total)
}
saveRDS(res, paste0(output_susie, '.rds'))

for(s in 1:3){
  methods = unique(dat$method)
  # methods = methods[-grep('ldrefin', methods)]
  # methods = methods[-grep('AZFALSE_rcorFALSE', methods)]
  res = list()
  for(met in methods){
    print(met)
    dat_sub = dat %>% filter(method == met, sim_gaussian.n_signal == s)
    converged = unlist(dat_sub$score_susie.converged)
    total = unlist(dat_sub$score_susie.total) * converged
    valid = unlist(dat_sub$score_susie.valid) * converged
    sizes = unlist(dat_sub$score_susie.size[converged])
    purity = unlist(dat_sub$score_susie.purity[converged])
    expected = unlist(dat_sub$sim_gaussian.n_signal) * converged

    res[[met]] = list(total = sum(total), valid = sum(valid),
                      size = median(sizes, na.rm=T), purity = median(purity, na.rm=T),
                      expected = sum(expected), nonconverged = sum(!converged))
    res[[met]]$power = res[[met]]$valid/res[[met]]$expected
    res[[met]]$coverage = res[[met]]$valid/res[[met]]$total
    res[[met]]$power_se = sqrt(res[[met]]$power * (1-res[[met]]$power) / res[[met]]$expected)
    res[[met]]$power_se[is.nan(res[[met]]$power_se)] = 0
    res[[met]]$coverage_se = sqrt(res[[met]]$coverage * (1-res[[met]]$coverage) / res[[met]]$total)
  }
  saveRDS(res, paste0(output_susie, '_s', s,'.rds'))
}

res_susierss = readRDS(paste0(output_susie, '.rds'))
res_susierssnull = readRDS('../susierss_ukb_20210218_REF1000_pve005/susierss_ukb_20210218_REF1000_pve005_cs/susierss_ukb_cs.rds')
res_susierssnull = list('susie_suff_initnull' = res_susierssnull$susie_suff_ERTRUE_ldin_AZFALSE_rcorNA_scalezNA)
res_fm = readRDS('../susierss_ukb_20210218_REF1000_pve005/susierss_ukb_20210218_REF1000_pve005_cs/finemapv4_ukb_cs.rds')
res_fm = list('FINEMAPv1.4' = res_fm$FINEMAPv1.4_ldin_AZFALSE_scalezNA)
res = c(res_susierss, res_susierssnull, res_fm)
rates = matrix(unlist(res), length(res), byrow = T)
rownames(rates) = names(res)
colnames(rates) = c('discoveries', 'valid', 'size', 'purity', 'expected', 'nonconverge',
                    'power', 'coverage', 'power_se', 'coverage_se')
rates = as.data.frame(rates)
rates$method = rownames(rates)
rates$method[1] = 'susie_initnull'

rates$method = factor(rates$method, levels = c('susie_initnull', 'susie_initoracle',
                                               'susie_initlasso', 'susie_suff_initnull',
                                               "susie_suff_initoracle", "susie_suff_initlasso" ,
                                               'susie_suff_3steps','FINEMAPv1.4'))
rates_s = rates[c(1:3,8),]
p1 = plot_panel(rates_s, c('coverage', 'coverage'), legend=F)
p2 = plot_panel(rates_s, c('power', 'power'), legend=F)
p3 = plot_panel(rates_s, c('size', 'median number of variables'), legend=F)
p4 = plot_panel(rates_s, c('purity', 'median of purity'), legend=F)
output = 'susierss_ukb_REF1000_pve005_init_20210307_cs/susie_cs'
pdf(paste0(output, '_plots.pdf'), width=12, height=3)
grid.arrange(p1,p2,p3,p4, ncol=4, widths=c(3,3,3,3))
dev.off()
system(paste0("convert -flatten -density 120 ", paste0(output, '_plots.pdf'), " ", paste0(output, '_plots.png')))

rates_ss = rates[c(4:8),]
p1 = plot_panel(rates_ss, c('coverage', 'coverage'), legend=F)
p2 = plot_panel(rates_ss, c('power', 'power'), legend=F)
p3 = plot_panel(rates_ss, c('size', 'median number of variables'), legend=F)
p4 = plot_panel(rates_ss, c('purity', 'median of purity'), legend=F)
output = 'susierss_ukb_REF1000_pve005_init_20210307_cs/susie_ss_cs'
pdf(paste0(output, '_plots.pdf'), width=12, height=3)
grid.arrange(p1,p2,p3,p4, ncol=4, widths=c(3,3,3,3))
dev.off()
system(paste0("convert -flatten -density 120 ", paste0(output, '_plots.pdf'), " ", paste0(output, '_plots.png')))



for(s in 1:3){
  res_susierss = readRDS(paste0(output_susie, '_s',s,'.rds'))
  res_susierssnull = readRDS(paste0('../susierss_ukb_20210218_REF1000_pve005/susierss_ukb_20210218_REF1000_pve005_cs/susierss_ukb_cs_s',s,'.rds'))
  res_susierssnull = list('susie_suff_initnull' = res_susierssnull$susie_suff_ERTRUE_ldin_AZFALSE_rcorNA_scalezNA)

  res_fm = readRDS(paste0('../susierss_ukb_20210218_REF1000_pve005/susierss_ukb_20210218_REF1000_pve005_cs/finemapv4_ukb_cs_s',s,'.rds'))
  res_fm = list('FINEMAPv1.4' = res_fm$FINEMAPv1.4_ldin_AZFALSE_scalezNA)

  res = c(res_susierss, res_susierssnull, res_fm)
  rates = matrix(unlist(res), length(res), byrow = T)
  rownames(rates) = names(res)
  colnames(rates) = c('discoveries', 'valid', 'size', 'purity', 'expected', 'nonconverge',
                      'power', 'coverage', 'power_se', 'coverage_se')
  rates = as.data.frame(rates)
  rates$method = rownames(rates)
  rates$method[1] = 'susie_initnull'

  rates$method = factor(rates$method, levels = c('susie_initnull', 'susie_initoracle',
                                                 'susie_initlasso', 'susie_suff_initnull',
                                                 'susie_suff_initoracle', 'susie_suff_initlasso',
                                                 'FINEMAPv1.4'))

  rates_s = rates[c(1:3,7),]
  p1 = plot_panel(rates_s, c('coverage', 'coverage'), legend=F)
  p2 = plot_panel(rates_s, c('power', 'power'), legend=F)
  p3 = plot_panel(rates_s, c('size', 'median number of variables'), legend=F)
  p4 = plot_panel(rates_s, c('purity', 'median of purity'), legend=F)
  output = 'susierss_ukb_REF1000_pve005_init_20210307_cs/susie_cs'
  pdf(paste0(output, '_s',s,'_plots.pdf'), width=12, height=3)
  grid.arrange(p1,p2,p3,p4, ncol=4, widths=c(3,3,3,3))
  dev.off()
  system(paste0("convert -flatten -density 120 ", paste0(output, '_s',s,'_plots.pdf'), " ", paste0(output, '_s',s,'_plots.png')))

  rates_ss = rates[c(4:7),]
  p1 = plot_panel(rates_ss, c('coverage', 'coverage'), legend=F)
  p2 = plot_panel(rates_ss, c('power', 'power'), legend=F)
  p3 = plot_panel(rates_ss, c('size', 'median number of variables'), legend=F)
  p4 = plot_panel(rates_ss, c('purity', 'median of purity'), legend=F)
  output = 'susierss_ukb_REF1000_pve005_init_20210307_cs/susie_ss_cs'
  pdf(paste0(output, '_s',s,'_plots.pdf'), width=12, height=3)
  grid.arrange(p1,p2,p3,p4, ncol=4, widths=c(3,3,3,3))
  dev.off()
  system(paste0("convert -flatten -density 120 ", paste0(output, '_s',s,'_plots.pdf'), " ", paste0(output, '_s',s,'_plots.png')))
}

