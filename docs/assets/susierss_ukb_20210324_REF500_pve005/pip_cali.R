library(ggplot2)
library(cowplot)
dot_plot = function(dataframe) {
  ggplot(dataframe, aes(x=mean_pip, y=observed_freq)) + 
    geom_errorbar(aes(ymin=observed_freq-se, ymax=observed_freq+se), colour="gray", size = 0.2, width=.01) +
    geom_point(size=1.5, shape=21, fill="#002b36") + # 21 is filled circle
    xlab("Mean PIP") +
    ylab("Observed frequency") +
    coord_cartesian(ylim=c(0,1), xlim=c(0,1)) +
    geom_abline(slope=1,intercept=0,colour='red', size=0.2) +
    ggtitle(rename_ERF[[name]]) +
    expand_limits(y=0) +                        # Expand y range
    theme_cowplot() + theme(plot.title = element_text(size = 8))
}

# parameters
bin_size = 10

input = 'susierss_ukb_20210324_REF500_pve005_pip_extraction/ukb_pip.rds'
output = 'susierss_ukb_20210324_REF500_pve005_pip_calibration/ukb_pip_calib'

# dat = readRDS(input)
# bins = cbind(seq(1:bin_size)/bin_size-1/bin_size, seq(1:bin_size)/bin_size)
# pip_cali = list()
# for (method in names(dat)) {
#   pip_cali[[method]] = matrix(NA, nrow(bins), 3)
#   for (i in 1:nrow(bins)) {
#     data_in_bin = dat[[method]][which(dat[[method]][,1] > bins[i,1] & dat[[method]][,1] < bins[i,2]),]
#     if(!is.null(dim(data_in_bin))) {
#       pip_cali[[method]][i,1] = sum(data_in_bin[,1])
#       pip_cali[[method]][i,2] = sum(data_in_bin[,2])
#       pip_cali[[method]][i,3] = nrow(data_in_bin)
#     } else {
#       pip_cali[[method]][i,] = c(0,0,0)
#     }
#   }
# }
# for (method in names(dat)) {
#   pip_cali[[method]][,c(1,2)] = pip_cali[[method]][,c(1,2)] / pip_cali[[method]][,3]
# }
# saveRDS(pip_cali, paste0(output, '.rds'))

pip_cali = readRDS(paste0(output, '.rds'))

methods = names(pip_cali)
rename_mets = gsub('_ldin', '', methods)
rename_mets = gsub('_ldrefout', '_ldref', rename_mets)
rename_mets = gsub('_AZTRUE', '_AZ', rename_mets)
rename_mets = gsub('_AZFALSE', '', rename_mets)
rename_mets = gsub('_ERNA', '', rename_mets)
rename_mets = gsub('_lamb0$', '', rename_mets)
rename_mets = gsub('finemapv4', 'FINEMAPv1.4', rename_mets)
rename = as.list(rename_mets)
names(rename) = names(pip_cali)

rename_ERF = rename[-grep('ERTRUE', names(rename))]
rename_ERF = lapply(rename_ERF, function(x) gsub('_ERFALSE', '', x))

## susierss
print('susierss')
methods = c('susie_suff_refineFALSE_ERFALSE_ldin_AZFALSE_lamb0',
            'susie_rss_refineFALSE_ERNA_ldin_AZFALSE_lamb0',
            'susie_rss_refineFALSE_ERNA_ldrefout_AZFALSE_lamb0',
            'susie_rss_refineFALSE_ERNA_ldrefout_AZFALSE_lamb0.001',
            'susie_rss_refineFALSE_ERNA_ldrefout_AZFALSE_lambestimate',
            'susie_rss_refineFALSE_ERNA_ldrefout_AZTRUE_lamb0')
idx = 0
for (name in methods) {
  idx = idx + 1
  pip_cali[[name]][,3] = sqrt(pip_cali[[name]][,2] * (1 - pip_cali[[name]][,2]) / pip_cali[[name]][,3]) * 2
  pip_cali[[name]] = as.data.frame(pip_cali[[name]])
  colnames(pip_cali[[name]]) = c("mean_pip", "observed_freq", "se")
  pdf(paste0(output, '_susierss_' , idx, '.pdf'), width=3, height=3, pointsize=16)
  print(dot_plot(pip_cali[[name]]))
  dev.off()
  system(paste0("convert -flatten -density 120 ", output, '_susierss_' , idx, '.pdf', " ",output, '_susierss_' , idx, '.png'))
}
files = paste0(output, '_susierss_', 1:idx, '.png')
cmd = paste('convert +append', paste(files, collapse=" "), paste0(output, '_susierss.png'))
system(cmd)

## susierss refine
print('susierss refine')
methods = c('susie_suff_refineTRUE_ERFALSE_ldin_AZFALSE_lamb0',
            'susie_rss_refineTRUE_ERNA_ldin_AZFALSE_lamb0',
            'susie_rss_refineTRUE_ERNA_ldrefout_AZFALSE_lamb0',
            'susie_rss_refineTRUE_ERNA_ldrefout_AZFALSE_lamb0.001',
            'susie_rss_refineTRUE_ERNA_ldrefout_AZFALSE_lambestimate',
            'susie_rss_refineTRUE_ERNA_ldrefout_AZTRUE_lamb0')
idx = 0
for (name in methods) {
  idx = idx + 1
  pip_cali[[name]][,3] = sqrt(pip_cali[[name]][,2] * (1 - pip_cali[[name]][,2]) / pip_cali[[name]][,3]) * 2
  pip_cali[[name]] = as.data.frame(pip_cali[[name]])
  colnames(pip_cali[[name]]) = c("mean_pip", "observed_freq", "se")
  pdf(paste0(output, '_susierssrefine_' , idx, '.pdf'), width=3, height=3, pointsize=16)
  print(dot_plot(pip_cali[[name]]))
  dev.off()
  system(paste0("convert -flatten -density 120 ", output, '_susierssrefine_' , idx, '.pdf', " ",output, '_susierssrefine_' , idx, '.png'))
}
files = paste0(output, '_susierssrefine_', 1:idx, '.png')
cmd = paste('convert +append', paste(files, collapse=" "), paste0(output, '_susierssrefine.png'))
system(cmd)

# ## caviar
# print('caviar')
# methods = c('CAVIAR_ldin_AZFALSE_rcorTRUE',
#             'CAVIAR_ldrefout_AZFALSE_rcorTRUE',
#             'CAVIAR_ldrefout_AZTRUE_rcorTRUE',
#             'CAVIAR_ldrefout_AZTRUE_rcorFALSE')
# idx = 0
# for (name in methods) {
#   idx = idx + 1
#   pip_cali[[name]][,3] = sqrt(pip_cali[[name]][,2] * (1 - pip_cali[[name]][,2]) / pip_cali[[name]][,3]) * 2
#   pip_cali[[name]] = as.data.frame(pip_cali[[name]])
#   colnames(pip_cali[[name]]) = c("mean_pip", "observed_freq", "se")
#   pdf(paste0(output, '_caviar_' , idx, '.pdf'), width=3, height=3, pointsize=16)
#   print(dot_plot(pip_cali[[name]]))
#   dev.off()
#   system(paste0("convert -flatten -density 120 ", output, '_caviar_' , idx, '.pdf', " ",output, '_caviar_' , idx, '.png'))
# }
# files = paste0(output, '_caviar_', 1:idx, '.png')
# cmd = paste('convert +append', paste(files, collapse=" "), paste0(output, '_caviar.png'))
# system(cmd)

## finemap
print('finemapv1')
methods = c('FINEMAPv1.1_ldin_AZFALSE_lamb0',
            'FINEMAPv1.1_ldrefout_AZFALSE_lamb0',
            'FINEMAPv1.1_ldrefout_AZFALSE_lamb0.001',
            'FINEMAPv1.1_ldrefout_AZFALSE_lambestimate',
            'FINEMAPv1.1_ldrefout_AZTRUE_lamb0')
idx = 0
for (name in methods) {
  idx = idx + 1
  pip_cali[[name]][,3] = sqrt(pip_cali[[name]][,2] * (1 - pip_cali[[name]][,2]) / pip_cali[[name]][,3]) * 2
  pip_cali[[name]] = as.data.frame(pip_cali[[name]])
  colnames(pip_cali[[name]]) = c("mean_pip", "observed_freq", "se")
  pdf(paste0(output, '_fmv1_' , idx, '.pdf'), width=3, height=3, pointsize=16)
  print(dot_plot(pip_cali[[name]]))
  dev.off()
  system(paste0("convert -flatten -density 120 ", output, '_fmv1_' , idx, '.pdf', " ",output, '_fmv1_' , idx, '.png'))
}

files = paste0(output, '_fmv1_', 1:idx, '.png')
cmd = paste('convert +append', paste(files, collapse=" "), paste0(output, '_fmv1.png'))
system(cmd)

## finemapv1.4
print('finemapv4')
methods = c('finemapv4_ldin_AZFALSE_lamb0',
            'finemapv4_ldrefout_AZFALSE_lamb0',
            'finemapv4_ldrefout_AZFALSE_lamb0.001',
            'finemapv4_ldrefout_AZFALSE_lambestimate',
            'finemapv4_ldrefout_AZTRUE_lamb0')
idx = 0
for (name in methods) {
  idx = idx + 1
  pip_cali[[name]][,3] = sqrt(pip_cali[[name]][,2] * (1 - pip_cali[[name]][,2]) / pip_cali[[name]][,3]) * 2
  pip_cali[[name]] = as.data.frame(pip_cali[[name]])
  colnames(pip_cali[[name]]) = c("mean_pip", "observed_freq", "se")
  pdf(paste0(output, '_fmv4_' , idx, '.pdf'), width=3, height=3, pointsize=16)
  print(dot_plot(pip_cali[[name]]))
  dev.off()
  system(paste0("convert -flatten -density 120 ", output, '_fmv4_' , idx, '.pdf', " ",output, '_fmv4_' , idx, '.png'))
}
files = paste0(output, '_fmv4_', 1:idx, '.png')
cmd = paste('convert +append', paste(files, collapse=" "), paste0(output, '_fmv4.png'))
system(cmd)

## finemapv1.4L4
print('finemapv4L4')
methods = c('finemapv4L4_ldin_AZFALSE_lamb0',
            'finemapv4L4_ldrefout_AZFALSE_lamb0',
            'finemapv4L4_ldrefout_AZFALSE_lamb0.001',
            'finemapv4L4_ldrefout_AZFALSE_lambestimate',
            'finemapv4L4_ldrefout_AZTRUE_lamb0')
idx = 0
for (name in methods) {
  idx = idx + 1
  pip_cali[[name]][,3] = sqrt(pip_cali[[name]][,2] * (1 - pip_cali[[name]][,2]) / pip_cali[[name]][,3]) * 2
  pip_cali[[name]] = as.data.frame(pip_cali[[name]])
  colnames(pip_cali[[name]]) = c("mean_pip", "observed_freq", "se")
  pdf(paste0(output, '_fmv4L4_' , idx, '.pdf'), width=3, height=3, pointsize=16)
  print(dot_plot(pip_cali[[name]]))
  dev.off()
  system(paste0("convert -flatten -density 120 ", output, '_fmv4L4_' , idx, '.pdf', " ",output, '_fmv4L4_' , idx, '.png'))
}
files = paste0(output, '_fmv4L4_', 1:idx, '.png')
cmd = paste('convert +append', paste(files, collapse=" "), paste0(output, '_fmv4L4.png'))
system(cmd)
