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
    ggtitle(rename[[name]]) +
    expand_limits(y=0) +                        # Expand y range
    theme_cowplot() + theme(plot.title = element_text(size = 8))
}

# parameters
bin_size = 10

input = 'susierss_ukb_20210218_REF1000_pve005_pip_extraction/ukb_pip.rds'
output = 'susierss_ukb_20210218_REF1000_pve005_pip_calibration/ukb_pip_calib'

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
              "FINEMAPv1.1_ldin_AZFALSE_scalezNA" = 'FINEMAPv1.1',
              "FINEMAPv1.1_ldrefout_AZFALSE_scalezNA" = 'FINEMAPv1.1_ldref',
              "FINEMAPv1.1_ldrefout_AZTRUE_scalezNA" = 'FINEMAPv1.1_ldref_AZ_cor',
              "FINEMAPv1.1_ldrefout_AZTRUE_scalezmax" = 'FINEMAPv1.1_ldref_AZ_cor_scalemax',
              "FINEMAPv1.1_ldrefout_AZTRUE_scalezratio" = 'FINEMAPv1.1_ldref_AZ_cor_scaleratio',
              "FINEMAPv1.4_ldin_AZFALSE_scalezNA" = 'FINEMAPv1.4',
              "FINEMAPv1.4_ldrefout_AZFALSE_scalezNA" = 'FINEMAPv1.4_ldref',
              "FINEMAPv1.4_ldrefout_AZTRUE_scalezNA" = 'FINEMAPv1.4_ldref_AZ_cor',
              "FINEMAPv1.4_ldrefout_AZTRUE_scalezmax" = 'FINEMAPv1.4_ldref_AZ_cor_scalemax',
              "FINEMAPv1.4_ldrefout_AZTRUE_scalezratio" = 'FINEMAPv1.4_ldref_AZ_cor_scaleratio',
              'CAVIAR_ldin_AZFALSE_rcorTRUE' = 'CAVIAR',
              'CAVIAR_ldrefout_AZFALSE_rcorTRUE' = 'CAVIAR_ldref',
              'CAVIAR_ldrefout_AZTRUE_rcorTRUE' = 'CAVIAR_ldref_AZ_cor',
              'CAVIAR_ldrefout_AZTRUE_rcorFALSE' = 'CAVIAR_ldref_AZ_cov')
## susierss
methods = c('susie_suff_ERTRUE_ldin_AZFALSE_rcorNA_scalezNA',
            'susie_rss_ERTRUE_ldin_AZFALSE_rcorTRUE_scalezNA',
            'susie_rss_ERTRUE_ldrefout_AZFALSE_rcorTRUE_scalezNA',
            'susie_rss_ERTRUE_ldrefout_AZTRUE_rcorTRUE_scalezNA',
            'susie_rss_ERTRUE_ldrefout_AZTRUE_rcorFALSE_scalezNA')
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

## susierss
methods = c('susie_rss_lambda_ERTRUE_ldin_AZFALSE_rcorTRUE_scalezNA',
            'susie_rss_lambda_ERTRUE_ldrefout_AZFALSE_rcorTRUE_scalezNA',
            'susie_rss_lambda_ERTRUE_ldrefout_AZTRUE_rcorTRUE_scalezNA',
            'susie_rss_lambda_ERTRUE_ldrefout_AZTRUE_rcorFALSE_scalezNA')
idx = 0
for (name in methods) {
  idx = idx + 1
  pip_cali[[name]][,3] = sqrt(pip_cali[[name]][,2] * (1 - pip_cali[[name]][,2]) / pip_cali[[name]][,3]) * 2
  pip_cali[[name]] = as.data.frame(pip_cali[[name]])
  colnames(pip_cali[[name]]) = c("mean_pip", "observed_freq", "se")
  pdf(paste0(output, '_susiersslambda_' , idx, '.pdf'), width=3, height=3, pointsize=16)
  print(dot_plot(pip_cali[[name]]))
  dev.off()
  system(paste0("convert -flatten -density 120 ", output, '_susiersslambda_' , idx, '.pdf', " ",output, '_susiersslambda_' , idx, '.png'))
}

files = paste0(output, '_susiersslambda_', 1:idx, '.png')
cmd = paste('convert +append', paste(files, collapse=" "), paste0(output, '_susiersslambda.png'))
system(cmd)

## caviar
methods = c('CAVIAR_ldin_AZFALSE_rcorTRUE',
            'CAVIAR_ldrefout_AZFALSE_rcorTRUE',
            'CAVIAR_ldrefout_AZTRUE_rcorTRUE',
            'CAVIAR_ldrefout_AZTRUE_rcorFALSE')
idx = 0
for (name in methods) {
  idx = idx + 1
  pip_cali[[name]][,3] = sqrt(pip_cali[[name]][,2] * (1 - pip_cali[[name]][,2]) / pip_cali[[name]][,3]) * 2
  pip_cali[[name]] = as.data.frame(pip_cali[[name]])
  colnames(pip_cali[[name]]) = c("mean_pip", "observed_freq", "se")
  pdf(paste0(output, '_caviar_' , idx, '.pdf'), width=3, height=3, pointsize=16)
  print(dot_plot(pip_cali[[name]]))
  dev.off()
  system(paste0("convert -flatten -density 120 ", output, '_caviar_' , idx, '.pdf', " ",output, '_caviar_' , idx, '.png'))
}

files = paste0(output, '_caviar_', 1:idx, '.png')
cmd = paste('convert +append', paste(files, collapse=" "), paste0(output, '_caviar.png'))
system(cmd)

## finemap
methods = c('FINEMAPv1.1_ldin_AZFALSE_scalezNA',
            'FINEMAPv1.1_ldrefout_AZFALSE_scalezNA',
            'FINEMAPv1.1_ldrefout_AZTRUE_scalezNA')
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
methods = c('FINEMAPv1.4_ldin_AZFALSE_scalezNA',
            'FINEMAPv1.4_ldrefout_AZFALSE_scalezNA',
            'FINEMAPv1.4_ldrefout_AZTRUE_scalezNA')
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

