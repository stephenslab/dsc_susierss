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
dat = readRDS(input)
output = 'susierss_ukb_20210218_REF1000_pve005_cs/susierss_ukb_cs'
methods = unique(dat$method)
methods = methods[-grep('ldrefin', methods)]
methods = methods[-grep('AZFALSE_rcorFALSE', methods)]
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
  lambda = unlist(dat_sub$score_susie.lamb) * converged
  cat('sigma2:\n')
  print(summary(sigma2))
  cat('lambda:\n')
  print(summary(lambda))

  res[[met]] = list(total = sum(total), valid = sum(valid),
                    size = median(sizes, na.rm=T), purity = median(purity, na.rm=T),
                    expected = sum(expected), nonconverged = sum(!converged))
  res[[met]]$power = res[[met]]$valid/res[[met]]$expected
  res[[met]]$coverage = res[[met]]$valid/res[[met]]$total
  res[[met]]$power_se = sqrt(res[[met]]$power * (1-res[[met]]$power) / res[[met]]$expected)
  res[[met]]$power_se[is.nan(res[[met]]$power_se)] = 0
  res[[met]]$coverage_se = sqrt(res[[met]]$coverage * (1-res[[met]]$coverage) / res[[met]]$total)
}

saveRDS(res, paste0(output, '.rds'))

res = readRDS(paste0(output, '.rds'))
rates = matrix(unlist(res), length(res), byrow = T)
rownames(rates) = names(res)
colnames(rates) = c('discoveries', 'valid', 'size', 'purity', 'expected', 'nonconverge', 
                    'power', 'coverage', 'power_se', 'coverage_se')
rates = as.data.frame(rates)
rates$method = rownames(rates)

rename = list("susie_suff_ERTRUE_ldin_AZFALSE_rcorNA" = "susie_suff_ldin",
              "susie_rss_ERTRUE_ldin_AZFALSE_rcorTRUE" = "susie_rss_ldin",
              "susie_rss_ERTRUE_ldin_AZTRUE_rcorTRUE" = "susie_rss_ldin_AZ_cor",
              "susie_rss_ERTRUE_ldin_AZTRUE_rcorFALSE" = "susie_rss_ldin_AZ_cov",
              "susie_rss_ERTRUE_ldrefout_AZFALSE_rcorTRUE" = "susie_rss_ldref",
              "susie_rss_ERTRUE_ldrefout_AZTRUE_rcorTRUE" = "susie_rss_ldref_AZ_cor",
              "susie_rss_ERTRUE_ldrefout_AZTRUE_rcorFALSE" = "susie_rss_ldref_AZ_cov",
              "susie_rss_lambda_ERTRUE_ldin_AZFALSE_rcorTRUE" = "susie_rss_lambda_ldin",
              "susie_rss_lambda_ERTRUE_ldin_AZTRUE_rcorTRUE" = "susie_rss_lambda_ldin_AZ_cor",
              "susie_rss_lambda_ERTRUE_ldin_AZTRUE_rcorFALSE" = "susie_rss_lambda_ldin_AZ_cov",
              "susie_rss_lambda_ERTRUE_ldrefout_AZFALSE_rcorTRUE" = "susie_rss_lambda_ldref",
              "susie_rss_lambda_ERTRUE_ldrefout_AZTRUE_rcorTRUE" = "susie_rss_lambda_ldref_AZ_cor",
              "susie_rss_lambda_ERTRUE_ldrefout_AZTRUE_rcorFALSE" = "susie_rss_lambda_ldref_AZ_cov")

rates_ERTRUE = rates[grep('ERTRUE', rates$method),]
rates_ERTRUE = rates_ERTRUE[-grep('ldin_AZTRUE', rates_ERTRUE$method),]
rates_ERTRUE$method = sapply(rownames(rates_ERTRUE), function(x) rename[[x]])

rates_ERTRUE$method = factor(rates_ERTRUE$method, levels = c('susie_suff_ldin', 'susie_rss_ldin', 'susie_rss_ldref',
                                                             'susie_rss_ldref_AZ_cor', 'susie_rss_ldref_AZ_cov',
                                                             'susie_rss_lambda_ldin', 'susie_rss_lambda_ldref',
                                                             'susie_rss_lambda_ldref_AZ_cor','susie_rss_lambda_ldref_AZ_cov'))
p1 = plot_panel(rates_ERTRUE, c('coverage', 'coverage'), legend=F)
p2 = plot_panel(rates_ERTRUE, c('power', 'power'), legend=F)
p3 = plot_panel(rates_ERTRUE, c('size', 'median number of variables'), legend=F)
p4 = plot_panel(rates_ERTRUE, c('purity', 'median of purity'), legend=F)
pdf(paste0(output, '_plots.pdf'), width=12, height=3)
grid.arrange(p1,p2,p3,p4, ncol=4, widths=c(3,3,3,3))
dev.off()
system(paste0("convert -flatten -density 120 ", paste0(output, '_plots.pdf'), " ", paste0(output, '_plots.png')))

