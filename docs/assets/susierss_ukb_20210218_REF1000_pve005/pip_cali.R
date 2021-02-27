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

input = 'susierss_ukb_20210218_REF1000_pve005_pip_extraction/susierss_ukb_pip.rds'
output = 'susierss_ukb_20210218_REF1000_pve005_pip_calibration/susierss_ukb_pip_calib'
dat = readRDS(input)

bins = cbind(seq(1:bin_size)/bin_size-1/bin_size, seq(1:bin_size)/bin_size)
pip_cali = list()
for (method in names(dat)) {
  pip_cali[[method]] = matrix(NA, nrow(bins), 3)
  for (i in 1:nrow(bins)) {
    data_in_bin = dat[[method]][which(dat[[method]][,1] > bins[i,1] & dat[[method]][,1] < bins[i,2]),]
    if(!is.null(dim(data_in_bin))) {
      pip_cali[[method]][i,1] = sum(data_in_bin[,1])
      pip_cali[[method]][i,2] = sum(data_in_bin[,2])
      pip_cali[[method]][i,3] = nrow(data_in_bin)
    } else {
      pip_cali[[method]][i,] = c(0,0,0) 
    }
  }
}
for (method in names(dat)) {
  pip_cali[[method]][,c(1,2)] = pip_cali[[method]][,c(1,2)] / pip_cali[[method]][,3]
}
saveRDS(pip_cali, paste0(output, '.rds'))

pip_cali = readRDS(paste0(output, '.rds'))

methods = names(pip_cali)
methods = methods[-grep('ldrefin', methods)]
methods = methods[grep('ERTRUE', methods)]
methods = methods[-grep('ldin_AZTRUE', methods)]
methods = methods[-grep('AZFALSE_rcorFALSE', methods)]

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

methods = c('susie_suff_ERTRUE_ldin_AZFALSE_rcorNA',
            'susie_rss_ERTRUE_ldin_AZFALSE_rcorTRUE',
            'susie_rss_ERTRUE_ldrefout_AZTRUE_rcorTRUE',
            'susie_rss_ERTRUE_ldrefout_AZTRUE_rcorFALSE',
            'susie_rss_lambda_ERTRUE_ldin_AZFALSE_rcorTRUE',
            'susie_rss_lambda_ERTRUE_ldin_AZTRUE_rcorTRUE',
            'susie_rss_lambda_ERTRUE_ldin_AZTRUE_rcorFALSE')

idx = 0
for (name in methods) {
  idx = idx + 1
  pip_cali[[name]][,3] = sqrt(pip_cali[[name]][,2] * (1 - pip_cali[[name]][,2]) / pip_cali[[name]][,3]) * 2
  pip_cali[[name]] = as.data.frame(pip_cali[[name]])
  colnames(pip_cali[[name]]) = c("mean_pip", "observed_freq", "se")
  pdf(paste0(output, '_' , idx, '.pdf'), width=3, height=3, pointsize=16)
  print(dot_plot(pip_cali[[name]]))
  dev.off()
  system(paste0("convert -flatten -density 120 ", output, '_' , idx, '.pdf', " ",output, '_' , idx, '.png'))
}

files = paste0(output, '_', 1:idx, '.png')
# files = paste0(output, '_', c(1,2,4,6), '.png')
# output2 = paste0('ukb_rss_20210107_pip_calibration_paper/ukb_rss_pip_cali_simu', simu, '_', level)
cmd = paste('convert +append', paste(files, collapse=" "), paste0(output, '.png'))
system(cmd)
# system(paste('rm -f', paste(files, collapse=" ")))
