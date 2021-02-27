pip_cutoff = 0.05

roc_data = function(d1, cutoff = c(pip_cutoff, 0.999), connect_org = F) {
  grid = 1000
  ttv = seq(1:grid)/grid
  ttv = ttv[which(ttv>=cutoff[1] & ttv<=cutoff[2])]
  rst1 = t(sapply(ttv, function(x) c(sum(d1[,2][d1[,1]>=x]), length(d1[,2][d1[,1]>=x]), sum(d1[,2][d1[,1]>=x]==0))))
  rst1 = cbind(rst1, sum(d1[,2]), sum(1-d1[,2]))
  rst1 = as.data.frame(rst1)
  colnames(rst1) = c('true_positive', 'total_positive', 'false_positive', 'total_signal', 'total_null')
  rst2 = as.data.frame(cbind(rst1$true_positive / rst1$total_positive, rst1$true_positive / rst1$total_signal,  ttv))
  rst3 = as.data.frame(cbind(1 - rst1$false_positive / rst1$total_null, rst1$true_positive / rst1$total_signal,  ttv))
  if (connect_org) {
    # make a stair to origin
    rst2 = rbind(rst2, c(max(0.995, rst2[nrow(rst2),1]), max(rst2[nrow(rst2),2]-0.01, 0), rst2[nrow(rst2),3]))
    rst2 = rbind(rst2, c(1, 0, 1))
    rst3 = rbind(rst3, c(1, 0, 1))
  }
  colnames(rst2) = c('Precision', 'Recall', 'Threshold')
  colnames(rst3) = c('TN', 'TP', 'Threshold')
  return(list(counts = rst1, pr = rst2, roc = rst3))
}
colors = c('#A60628', '#7A68A6', '#348ABD', '#467821', '#FF00FF', '#E2A233', 
           '#00FFFF', '#A9A9A9', '#ADFF2F', '#188487',  '#FF0000', '#000000', 
           '#FFD700', '#00FF00', '#9400D3', '#7FFFD4', '#A52A2A', '#000080')
library(scam)
create_chunks = function(item, n) {
  splitted = suppressWarnings(split(item, 1:n))
  return(c(splitted[[1]], splitted[[length(splitted)]][length(splitted[[length(splitted)]])]))
}
chunks = 0
xlim = 0.3
ylim = 0.3
make_smooth = function(x,y,subset=chunks, smooth = FALSE) {
  if (smooth) {
    if (subset < length(x) && subset > 0) {
      x = create_chunks(x, subset)
      y = create_chunks(y, subset)
    }
    dat = data.frame(cbind(x,y))
    colnames(dat) = c('x','y')
    y=predict(scam(y ~ s(x, bs = "mpi"), data = dat))
  }
  return(list(x=x,y=y))
}
add_text = function(thresholds, x, y, threshold, color, delta = -0.06) {
  idx = which(thresholds == threshold)
  # text(x[idx] - delta, y[idx], labels = threshold, col = color, cex=0.8)
  points(x[idx],y[idx], col=color)
}

input = paste0('susierss_ukb_20210218_REF1000_pve005_pip_extraction/susierss_ukb_pip.rds')
output = paste0('susierss_ukb_20210218_REF1000_pve005_roc/susierss_ukb_roc')
dat = readRDS(input)
print("Computing ROC data ...")
tb = list()
for (method in names(dat)) {
  print(method)
  tb[[method]] = roc_data(dat[[method]])
}
saveRDS(tb, paste0(output, '.rds'))

tb = readRDS(paste0(output, '.rds'))
type = 'pr'
main = "FDR vs Power"
ylab = "power"
xlab = "FDR"

rename_resid = list("susie_suff_ERTRUE_ldin_AZFALSE_rcorNA" = 'susie_suff_ERTRUE',
                    "susie_suff_ERFALSE_ldin_AZFALSE_rcorNA" = 'susie_suff_ERFALSE',
                    "susie_rss_ERTRUE_ldin_AZFALSE_rcorTRUE" = "susie_rss_ERTRUE",
                    "susie_rss_ERFALSE_ldin_AZFALSE_rcorTRUE" = "susie_rss_ERFALSE",
                    "susie_rss_lambda_ERTRUE_ldin_AZFALSE_rcorTRUE" = "susie_rss_lambda_ERTRUE",
                    "susie_rss_lambda_ERFALSE_ldin_AZFALSE_rcorTRUE" = "susie_rss_lambda_ERFALSE")

rename = list("susie_suff_ERTRUE_ldin_AZFALSE_rcorNA" = 'susie_suff_ldin',
              "susie_rss_ERTRUE_ldin_AZFALSE_rcorTRUE" = "susie_rss_ldin",
              "susie_rss_ERTRUE_ldin_AZTRUE_rcorTRUE" = "susie_rss_ldin_AZ_cor",
              "susie_rss_ERTRUE_ldin_AZTRUE_rcorFALSE" = "susie_rss_ldin_AZ_cov",
              "susie_rss_ERTRUE_ldrefout_AZFALSE_rcorTRUE" = "susie_rss_ldref",
              "susie_rss_ERTRUE_ldrefout_AZTRUE_rcorFALSE" = "susie_rss_ldref_AZ_cov",
              "susie_rss_ERTRUE_ldrefout_AZTRUE_rcorTRUE" = "susie_rss_ldref_AZ_cor",
              "susie_rss_ERTRUE_ldrefin_AZFALSE_rcorTRUE" = "susie_rss_ldrefin",
              "susie_rss_ERTRUE_ldrefin_AZTRUE_rcorFALSE" = "susie_rss_ldrefin_AZ_cov",
              "susie_rss_ERTRUE_ldrefin_AZTRUE_rcorTRUE" = "susie_rss_ldrefin_AZ_cor",
              "susie_rss_lambda_ERTRUE_ldin_AZFALSE_rcorTRUE" = "susie_rss_lambda_ldin",
              "susie_rss_lambda_ERTRUE_ldin_AZTRUE_rcorFALSE" = "susie_rss_lambda_ldin_AZ_cov",
              "susie_rss_lambda_ERTRUE_ldin_AZTRUE_rcorTRUE" = "susie_rss_lambda_ldin_AZ_cor",
              "susie_rss_lambda_ERTRUE_ldrefout_AZFALSE_rcorTRUE" = "susie_rss_lambda_ldref",
              "susie_rss_lambda_ERTRUE_ldrefout_AZTRUE_rcorTRUE" = "susie_rss_lambda_ldref_AZ_cor",
              "susie_rss_lambda_ERTRUE_ldrefout_AZTRUE_rcorFALSE" = "susie_rss_lambda_ldref_AZ_cov",
              "susie_rss_lambda_ERTRUE_ldrefin_AZFALSE_rcorTRUE" = "susie_rss_lambda_ldrefin",
              "susie_rss_lambda_ERTRUE_ldrefin_AZTRUE_rcorTRUE" = "susie_rss_lambda_ldrefin_AZ_cor",
              "susie_rss_lambda_ERTRUE_ldrefin_AZTRUE_rcorFALSE" = "susie_rss_lambda_ldrefin_AZ_cov")

pdf(paste0(output,'.', type,'residual_suff.pdf'), width=10, height=10, pointsize=15)
i = 1
labels = vector()
for (method in c('susie_suff_ERTRUE_ldin_AZFALSE_rcorNA', 'susie_suff_ERFALSE_ldin_AZFALSE_rcorNA')) {
  yy = make_smooth((1 - tb[[method]][[type]][,1]), tb[[method]][[type]][,2])
  if (i == 1) {
    plot(yy$x, yy$y, t="l", col=colors[i], ylab = ylab, xlab = xlab, main = main, bty='l',
         lwd = 2, xlim = c(0,xlim), ylim = c(0,ylim))
  } else {
    lines(yy$x, yy$y, col=colors[i], lwd = 2, xlim = c(0,xlim), ylim = c(0,ylim))
  }
  add_text(tb[[method]][[type]][,3], yy$x, yy$y, 0.95, colors[i])
  labels[i] = rename_resid[[method]]
  i = i + 1
}
legend("bottomright", legend=labels, col=colors[1:i], lty=1, cex=0.8)
dev.off()
system(paste0("convert -flatten -density 120 ", output, '.', type,'residual_suff.pdf', " ", output, '.', type, 'residual_suff.png'))

pdf(paste0(output,'.', type,'residual_rss.pdf'), width=10, height=10, pointsize=15)
i = 1
labels = vector()
for (method in c('susie_rss_ERTRUE_ldin_AZFALSE_rcorTRUE', 'susie_rss_ERFALSE_ldin_AZFALSE_rcorTRUE')) {
  yy = make_smooth((1 - tb[[method]][[type]][,1]), tb[[method]][[type]][,2])
  if (i == 1) {
    plot(yy$x, yy$y, t="l", col=colors[i], ylab = ylab, xlab = xlab, main = main, bty='l',
         lwd = 2, xlim = c(0,xlim), ylim = c(0,ylim))
  } else {
    lines(yy$x, yy$y, col=colors[i], lwd = 2, xlim = c(0,xlim), ylim = c(0,ylim))
  }
  add_text(tb[[method]][[type]][,3], yy$x, yy$y, 0.95, colors[i])
  labels[i] = rename_resid[[method]]
  i = i + 1
}
legend("bottomright", legend=labels, col=colors[1:i], lty=1, cex=0.8)
dev.off()
system(paste0("convert -flatten -density 120 ", output, '.', type,'residual_rss.pdf', " ", output, '.', type, 'residual_rss.png'))

pdf(paste0(output,'.', type,'residual_rss_lambda.pdf'), width=10, height=10, pointsize=15)
i = 1
labels = vector()
for (method in c('susie_rss_lambda_ERTRUE_ldin_AZFALSE_rcorTRUE', 'susie_rss_lambda_ERFALSE_ldin_AZFALSE_rcorTRUE')) {
  yy = make_smooth((1 - tb[[method]][[type]][,1]), tb[[method]][[type]][,2])
  if (i == 1) {
    plot(yy$x, yy$y, t="l", col=colors[i], ylab = ylab, xlab = xlab, main = main, bty='l',
         lwd = 2, xlim = c(0,xlim), ylim = c(0,ylim))
  } else {
    lines(yy$x, yy$y, col=colors[i], lwd = 2, xlim = c(0,xlim), ylim = c(0,ylim))
  }
  add_text(tb[[method]][[type]][,3], yy$x, yy$y, 0.95, colors[i])
  labels[i] = rename_resid[[method]]
  i = i + 1
}
legend("bottomright", legend=labels, col=colors[1:i], lty=1, cex=0.8)
dev.off()
system(paste0("convert -flatten -density 120 ", output, '.', type,'residual_rss_lambda.pdf', " ", output, '.', type, 'residual_rss_lambda.png'))

pdf(paste0(output,'.', type,'.ERTRUE_ldout_AZ.pdf'), width=10, height=10, pointsize=15)
i = 1
labels = vector()
for (method in c('susie_suff_ERTRUE_ldin_AZFALSE_rcorNA',
                 'susie_rss_ERTRUE_ldin_AZFALSE_rcorTRUE', 
                 'susie_rss_ERTRUE_ldin_AZTRUE_rcorTRUE', 
                 'susie_rss_ERTRUE_ldin_AZTRUE_rcorFALSE',
                 'susie_rss_ERTRUE_ldrefout_AZFALSE_rcorTRUE',
                 'susie_rss_ERTRUE_ldrefout_AZTRUE_rcorTRUE',
                 'susie_rss_ERTRUE_ldrefout_AZTRUE_rcorFALSE',
                 'susie_rss_lambda_ERTRUE_ldrefout_AZFALSE_rcorTRUE',
                 'susie_rss_lambda_ERTRUE_ldrefout_AZTRUE_rcorTRUE',
                 'susie_rss_lambda_ERTRUE_ldrefout_AZTRUE_rcorFALSE'
                 )) {
  yy = make_smooth((1 - tb[[method]][[type]][,1]), tb[[method]][[type]][,2])
  if (i == 1) {
    plot(yy$x, yy$y, t="l", col=colors[i], ylab = ylab, xlab = xlab, main = main, bty='l',
         lwd = 2, xlim = c(0,xlim), ylim = c(0,ylim))
  } else {
    lines(yy$x, yy$y, col=colors[i], lwd = 2, xlim = c(0,xlim), ylim = c(0,ylim))
  }
  add_text(tb[[method]][[type]][,3], yy$x, yy$y, 0.95, colors[i])
  labels[i] = rename[[method]]
  i = i + 1
}
legend("bottomright", legend=labels, col=colors[1:i], lty=1, cex=0.6)
dev.off()
system(paste0("convert -flatten -density 120 ", output, '.', type,'.ERTRUE_ldout_AZ.pdf', " ", 
              output, '.', type, '.ERTRUE_ldout_AZ.png'))

pdf(paste0(output,'.', type,'.ERTRUE_ldrefin_AZ.pdf'), width=10, height=10, pointsize=15)
i = 1
labels = vector()
for (method in c('susie_suff_ERTRUE_ldin_AZFALSE_rcorNA', 
                 'susie_rss_ERTRUE_ldin_AZFALSE_rcorTRUE', 
                 'susie_rss_ERTRUE_ldrefin_AZFALSE_rcorTRUE',
                 'susie_rss_ERTRUE_ldrefin_AZTRUE_rcorTRUE',
                 'susie_rss_ERTRUE_ldrefin_AZTRUE_rcorFALSE',
                 'susie_rss_lambda_ERTRUE_ldrefin_AZFALSE_rcorTRUE',
                 'susie_rss_lambda_ERTRUE_ldrefin_AZTRUE_rcorTRUE',
                 'susie_rss_lambda_ERTRUE_ldrefin_AZTRUE_rcorFALSE')) {
  yy = make_smooth((1 - tb[[method]][[type]][,1]), tb[[method]][[type]][,2])
  if (i == 1) {
    plot(yy$x, yy$y, t="l", col=colors[i], ylab = ylab, xlab = xlab, main = main, bty='l',
         lwd = 2, xlim = c(0,xlim), ylim = c(0,ylim))
  } else {
    lines(yy$x, yy$y, col=colors[i], lwd = 2, xlim = c(0,xlim), ylim = c(0,ylim))
  }
  add_text(tb[[method]][[type]][,3], yy$x, yy$y, 0.95, colors[i])
  labels[i] = rename[[method]]
  i = i + 1
}
legend("bottomright", legend=labels, col=colors[1:i], lty=1, cex=0.6)
dev.off()
system(paste0("convert -flatten -density 120 ", output, '.', type,'.ERTRUE_ldrefin_AZ.pdf', " ", 
              output, '.', type, '.ERTRUE_ldrefin_AZ.png'))
