pip_cutoff = 0.05

roc_data = function(d1, cutoff = c(pip_cutoff, 0.999), connect_org = F) {
  grid = 500
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

setwd('~/GitHub/dsc_susierss/results/susierss_ukb_20210324_REF500_pve005/')

removedat_all = c(TRUE, FALSE)
for( rem in removedat_all){
  input = paste0('susierss_ukb_20210324_REF500_pve005_pip_extraction/ukb_pip_remdatall',rem,'.rds')
  output = paste0('susierss_ukb_20210324_REF500_pve005_roc/ukb_roc_remdatall',rem)
  
  output_susiesuff = paste0('susierss_ukb_20210324_REF500_pve005_roc/susiesuff_ukb_roc_remdatall',rem)
  output_susiesuff_refine = paste0('susierss_ukb_20210324_REF500_pve005_roc/susiesuff_refine_ukb_roc_remdatall',rem)
  output_susierss = paste0('susierss_ukb_20210324_REF500_pve005_roc/susierss_ukb_roc_remdatall',rem)
  output_susierss_refine = paste0('susierss_ukb_20210324_REF500_pve005_roc/susierss_refine_ukb_roc_remdatall',rem)
  output_caviar = paste0('susierss_ukb_20210324_REF500_pve005_roc/caviar_ukb_roc_remdatall',rem)
  output_fm = paste0('susierss_ukb_20210324_REF500_pve005_roc/finemapv1_ukb_roc_remdatall',rem)
  output_fmv4 = paste0('susierss_ukb_20210324_REF500_pve005_roc/finemapv4_ukb_roc_remdatall',rem)
  output_fmv4L4 = paste0('susierss_ukb_20210324_REF500_pve005_roc/finemapv4L4_ukb_roc_remdatall',rem)
  
  # dat = readRDS(input)
  # 
  # print("Computing ROC data ...")
  # tb = list()
  # for (method in names(dat)) {
  #   print(method)
  #   tb[[method]] = roc_data(dat[[method]])
  # }
  # saveRDS(tb, paste0(output, '.rds'))

  # res = readRDS(paste0(output, '.rds'))
  # 
  # for (method in names(dat)[grep('lambmlelikelihood', names(dat))]){
  #   res[[method]] = roc_data(dat[[method]])
  # }
  # saveRDS(res, paste0(output, '.rds'))

  ## plot
  tb = readRDS(paste0(output, '.rds'))

  type = 'pr'
  main = "FDR vs Power"
  ylab = "power"
  xlab = "FDR"

  methods = names(tb)
  rename_mets = gsub('_ldin', '', methods)
  rename_mets = gsub('_ldrefout', '_ldref500', rename_mets)
  rename_mets = gsub('_AZTRUE', '_AZ', rename_mets)
  rename_mets = gsub('_AZFALSE', '', rename_mets)
  rename_mets = gsub('_ERNA', '', rename_mets)
  rename_mets = gsub('_lamb0$', '', rename_mets)
  rename_mets = gsub('finemapv4L4', 'FINEMAPv1.4 L4', rename_mets)
  rename_mets = gsub('finemapv4', 'FINEMAPv1.4', rename_mets)
  rename_mets = gsub('susie_suff', 'SuSiE-suff', rename_mets)
  rename_mets = gsub('susie_rss', 'SuSiE-RSS', rename_mets)
  rename_mets = gsub('ld', 'LD', rename_mets)
  rename_mets = gsub('lamb0.001', 's=0.001', rename_mets)
  rename_mets = gsub('lambmlelikelihood', 'estimated s', rename_mets)
  rename_mets = gsub('_', ' ', rename_mets)
  rename = as.list(rename_mets)
  names(rename) = names(tb)

  ## refine vs no refine
  pdf(paste0(output_susiesuff,'.', type,'.refine.pdf'), width=10, height=10, pointsize=15)
  i = 1
  labels = vector()
  for (method in c('susie_suff_refineFALSE_ERFALSE_ldin_AZFALSE_lamb0',
                   'susie_suff_refineTRUE_ERFALSE_ldin_AZFALSE_lamb0')) {
    yy = make_smooth((1 - tb[[method]][[type]][,1]), tb[[method]][[type]][,2])
    if (i == 1) {
      plot(yy$x, yy$y, t="l", col=colors[i], ylab = ylab, xlab = xlab, main = main, bty='l',
           lwd = 2, xlim = c(0,xlim), ylim = c(0,ylim))
    } else {
      lines(yy$x, yy$y, col=colors[i], lwd = 2, xlim = c(0,xlim), ylim = c(0,ylim))
    }
    add_text(tb[[method]][[type]][,3], yy$x, yy$y, 0.95, colors[i])
    labels[i] = rename[[method]]
    labels[i] = gsub(' ERFALSE', '', labels[i])
    i = i + 1
  }
  legend("bottomright", legend=labels, col=colors[1:i], lty=1, cex=0.8)
  dev.off()
  system(paste0("convert -flatten -density 120 ", output_susiesuff, '.', type,'.refine.pdf', " ",
                output_susiesuff, '.', type, '.refine.png'))

  ############### ER FALSE only

  ## compare methods
  ### refine FALSE
  pdf(paste0(output,'.', type,'.ERFALSE_ldin_methods.pdf'), width=10, height=10, pointsize=15)
  i = 1
  labels = vector()
  # names(tb)[grep('ldin', names(tb))]
  for (method in c('susie_suff_refineFALSE_ERFALSE_ldin_AZFALSE_lamb0',
                   'susie_rss_refineFALSE_ERNA_ldin_AZFALSE_lamb0',
                   'FINEMAPv1.1_ldin_AZFALSE_lamb0',
                   'finemapv4_ldin_AZFALSE_lamb0',
                   "finemapv4L4_ldin_AZFALSE_lamb0",
                   'CAVIAR_ldin_AZFALSE_lamb0'
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
    labels[i] = gsub(' ERFALSE', '', labels[i])
    labels[i] = gsub(' refineFALSE', '', labels[i])
    i = i + 1
  }
  legend("bottomright", legend=labels, col=colors[1:i], lty=1, cex=0.6)
  dev.off()
  system(paste0("convert -flatten -density 120 ", output, '.', type,'.ERFALSE_ldin_methods.pdf', " ",
                output, '.', type, '.ERFALSE_ldin_methods.png'))

  ### refine TRUE
  pdf(paste0(output,'.', type,'.ERFALSE_refine_ldin_methods.pdf'), width=10, height=10, pointsize=15)
  i = 1
  labels = vector()
  for (method in c('susie_suff_refineTRUE_ERFALSE_ldin_AZFALSE_lamb0',
                   'susie_rss_refineTRUE_ERNA_ldin_AZFALSE_lamb0',
                   'FINEMAPv1.1_ldin_AZFALSE_lamb0',
                   'finemapv4_ldin_AZFALSE_lamb0',
                   "finemapv4L4_ldin_AZFALSE_lamb0",
                   'CAVIAR_ldin_AZFALSE_lamb0'
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
    labels[i] = gsub(' ERFALSE', '', labels[i])
    labels[i] = gsub(' refineTRUE', '', labels[i])
    i = i + 1
  }
  legend("bottomright", legend=labels, col=colors[1:i], lty=1, cex=0.6)
  dev.off()
  system(paste0("convert -flatten -density 120 ", output, '.', type,'.ERFALSE_refine_ldin_methods.pdf', " ",
                output, '.', type, '.ERFALSE_refine_ldin_methods.png'))

  #### ld ref
  ###### refine FALSE
  pdf(paste0(output,'.', type,'.ERFALSE_ldref_methods.pdf'), width=10, height=10, pointsize=15)
  i = 1
  labels = vector()
  for (method in c('susie_rss_refineFALSE_ERNA_ldrefout_AZFALSE_lamb0',
                   'FINEMAPv1.1_ldrefout_AZFALSE_lamb0',
                   'finemapv4_ldrefout_AZFALSE_lamb0',
                   "finemapv4L4_ldrefout_AZFALSE_lamb0",
                   "CAVIAR_ldrefout_AZFALSE_lamb0"

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
    labels[i] = gsub(' ERFALSE', '', labels[i])
    labels[i] = gsub(' refineFALSE', '', labels[i])
    i = i + 1
  }
  legend("bottomright", legend=labels, col=colors[1:i], lty=1, cex=0.6)
  dev.off()
  system(paste0("convert -flatten -density 120 ", output, '.', type,'.ERFALSE_ldref_methods.pdf', " ",
                output, '.', type, '.ERFALSE_ldref_methods.png'))

  #### ld ref refine
  pdf(paste0(output,'.', type,'.ERFALSE_refine_ldref_methods.pdf'), width=10, height=10, pointsize=15)
  i = 1
  labels = vector()
  for (method in c('susie_rss_refineTRUE_ERNA_ldrefout_AZFALSE_lamb0',
                   'FINEMAPv1.1_ldrefout_AZFALSE_lamb0',
                   'finemapv4_ldrefout_AZFALSE_lamb0',
                   "finemapv4L4_ldrefout_AZFALSE_lamb0",
                   "CAVIAR_ldrefout_AZFALSE_lamb0"
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
    labels[i] = gsub(' ERFALSE', '', labels[i])
    labels[i] = gsub(' refineTRUE', '', labels[i])
    i = i + 1
  }
  legend("bottomright", legend=labels, col=colors[1:i], lty=1, cex=0.6)
  dev.off()
  system(paste0("convert -flatten -density 120 ", output, '.', type,'.ERFALSE_refine_ldref_methods.pdf', " ",
                output, '.', type, '.ERFALSE_refine_ldref_methods.png'))

  ## CAVIAR
  pdf(paste0(output_caviar,'.', type,'_ld.pdf'), width=10, height=10, pointsize=15)
  i = 1
  labels = vector()
  # names(rename)[grep('CAVIAR', names(rename))]
  for (method in c('CAVIAR_ldin_AZFALSE_lamb0',
                   'CAVIAR_ldrefout_AZFALSE_lamb0',
                   'CAVIAR_ldrefout_AZFALSE_lamb0.001',
                   # 'CAVIAR_ldrefout_AZFALSE_lambestimate',
                   'CAVIAR_ldrefout_AZFALSE_lambmlelikelihood',
                   'CAVIAR_ldrefout_AZTRUE_lamb0'
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
    labels[i] = gsub(' ERFALSE', '', labels[i])
    i = i + 1
  }
  legend("bottomright", legend=labels, col=colors[1:i], lty=1, cex=0.6)
  dev.off()
  system(paste0("convert -flatten -density 120 ", output_caviar, '.', type,'_ld.pdf', " ",
                output_caviar, '.', type, '_ld.png'))


  ## FINEMAPv1.1
  pdf(paste0(output_fm,'.', type,'_ld.pdf'), width=10, height=10, pointsize=15)
  i = 1
  labels = vector()
  # names(rename)[grep('FINEMAPv1.1', names(rename))]
  for (method in c('FINEMAPv1.1_ldin_AZFALSE_lamb0',
                   'FINEMAPv1.1_ldrefout_AZFALSE_lamb0',
                   'FINEMAPv1.1_ldrefout_AZFALSE_lamb0.001',
                   # 'FINEMAPv1.1_ldrefout_AZFALSE_lambestimate',
                   'FINEMAPv1.1_ldrefout_AZFALSE_lambmlelikelihood',
                   'FINEMAPv1.1_ldrefout_AZTRUE_lamb0'
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
    labels[i] = gsub(' ERFALSE', '', labels[i])
    i = i + 1
  }
  legend("bottomright", legend=labels, col=colors[1:i], lty=1, cex=0.6)
  dev.off()
  system(paste0("convert -flatten -density 120 ", output_fm, '.', type,'_ld.pdf', " ",
                output_fm, '.', type, '_ld.png'))

  ## FINEMAPv1.4
  pdf(paste0(output_fmv4,'.', type,'_ld.pdf'), width=10, height=10, pointsize=15)
  i = 1
  labels = vector()
  # names(rename)[grep('finemapv4', names(rename))]
  for (method in c('finemapv4_ldin_AZFALSE_lamb0',
                   'finemapv4_ldrefout_AZFALSE_lamb0',
                   'finemapv4_ldrefout_AZFALSE_lamb0.001',
                   # 'finemapv4_ldrefout_AZFALSE_lambestimate',
                   'finemapv4_ldrefout_AZFALSE_lambmlelikelihood',
                   'finemapv4_ldrefout_AZTRUE_lamb0'
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
    labels[i] = gsub('_ERFALSE', '', labels[i])
    i = i + 1
  }
  legend("bottomright", legend=labels, col=colors[1:i], lty=1, cex=0.6)
  dev.off()
  system(paste0("convert -flatten -density 120 ", output_fmv4, '.', type,'_ld.pdf', " ",
                output_fmv4, '.', type, '_ld.png'))

  ## FINEMAPv1.4L4
  pdf(paste0(output_fmv4L4,'.', type,'_ld.pdf'), width=10, height=10, pointsize=15)
  i = 1
  labels = vector()
  # names(rename)[grep('finemapv4L4', names(rename))]
  for (method in c('finemapv4L4_ldin_AZFALSE_lamb0',
                   'finemapv4L4_ldrefout_AZFALSE_lamb0',
                   'finemapv4L4_ldrefout_AZFALSE_lamb0.001',
                   # 'finemapv4L4_ldrefout_AZFALSE_lambestimate',
                   'finemapv4L4_ldrefout_AZFALSE_lambmlelikelihood',
                   'finemapv4L4_ldrefout_AZTRUE_lamb0'
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
    labels[i] = gsub(' ERFALSE', '', labels[i])
    i = i + 1
  }
  legend("bottomright", legend=labels, col=colors[1:i], lty=1, cex=0.6)
  dev.off()
  system(paste0("convert -flatten -density 120 ", output_fmv4L4, '.', type,'_ld.pdf', " ",
                output_fmv4L4, '.', type, '_ld.png'))

  ### susie rss
  pdf(paste0(output_susierss,'.', type,'.ERFALSE_ld.pdf'), width=10, height=10, pointsize=15)
  i = 1
  labels = vector()
  # names(tb)[grep('susie_rss_refineFALSE', names(tb))]
  for (method in c('susie_rss_refineFALSE_ERNA_ldin_AZFALSE_lamb0',
                   'susie_rss_refineFALSE_ERNA_ldrefout_AZFALSE_lamb0',
                   'susie_rss_refineFALSE_ERNA_ldrefout_AZFALSE_lamb0.001',
                   # 'susie_rss_refineFALSE_ERNA_ldrefout_AZFALSE_lambestimate',
                   'susie_rss_refineFALSE_ERNA_ldrefout_AZFALSE_lambmlelikelihood',
                   'susie_rss_refineFALSE_ERNA_ldrefout_AZTRUE_lamb0'
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
    labels[i] = gsub(' ERFALSE', '', labels[i])
    labels[i] = gsub(' refineFALSE', '', labels[i])
    i = i + 1
  }
  legend("bottomright", legend=labels, col=colors[1:i], lty=1, cex=0.6)
  dev.off()
  system(paste0("convert -flatten -density 120 ", output_susierss, '.', type,'.ERFALSE_ld.pdf', " ",
                output_susierss, '.', type, '.ERFALSE_ld.png'))

  ### susie rss refine
  pdf(paste0(output_susierss_refine,'.', type,'.ERFALSE_ld.pdf'), width=10, height=10, pointsize=15)
  i = 1
  labels = vector()
  # names(tb)[grep('susie_rss_refineTRUE', names(tb))]
  for (method in c('susie_rss_refineTRUE_ERNA_ldin_AZFALSE_lamb0',
                   'susie_rss_refineTRUE_ERNA_ldrefout_AZFALSE_lamb0',
                   # 'susie_suff_refineTRUE_ERFALSE_ldrefout_AZFALSE_lamb0'
                   'susie_rss_refineTRUE_ERNA_ldrefout_AZFALSE_lamb0.001',
                   # 'susie_rss_refineTRUE_ERNA_ldrefout_AZFALSE_lambestimate',
                   'susie_rss_refineTRUE_ERNA_ldrefout_AZFALSE_lambmlelikelihood',
                   'susie_rss_refineTRUE_ERNA_ldrefout_AZTRUE_lamb0'
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
    labels[i] = gsub(' ERFALSE', '', labels[i])
    labels[i] = gsub(' refineTRUE', '', labels[i])
    i = i + 1
  }
  legend("bottomright", legend=labels, col=colors[1:i], lty=1, cex=0.6)
  dev.off()
  system(paste0("convert -flatten -density 120 ", output_susierss_refine, '.', type,'.ERFALSE_ld.pdf', " ",
                output_susierss_refine, '.', type, '.ERFALSE_ld.png'))


  ### susie suff
  pdf(paste0(output_susiesuff,'.', type,'.ERFALSE_ld.pdf'), width=10, height=10, pointsize=15)
  i = 1
  labels = vector()
  # names(tb)[grep('susie_suff_refineFALSE', names(tb))]
  for (method in c('susie_suff_refineFALSE_ERFALSE_ldin_AZFALSE_lamb0',
                   'susie_suff_refineFALSE_ERFALSE_ldrefout_AZFALSE_lamb0',
                   'susie_suff_refineFALSE_ERFALSE_ldrefout_AZFALSE_lamb0.001',
                   # 'susie_suff_refineFALSE_ERFALSE_ldrefout_AZFALSE_lambestimate',
                   'susie_suff_refineFALSE_ERFALSE_ldrefout_AZFALSE_lambmlelikelihood',
                   'susie_suff_refineFALSE_ERFALSE_ldrefout_AZTRUE_lamb0'
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
    labels[i] = gsub(' ERFALSE', '', labels[i])
    labels[i] = gsub(' refineFALSE', '', labels[i])
    i = i + 1
  }
  legend("bottomright", legend=labels, col=colors[1:i], lty=1, cex=0.6)
  dev.off()
  system(paste0("convert -flatten -density 120 ", output_susiesuff, '.', type,'.ERFALSE_ld.pdf', " ",
                output_susiesuff, '.', type, '.ERFALSE_ld.png'))

  ### susie suff refine
  pdf(paste0(output_susiesuff_refine,'.', type,'.ERFALSE_ld.pdf'), width=10, height=10, pointsize=15)
  i = 1
  labels = vector()
  # names(tb)[grep('susie_suff_refineTRUE', names(tb))]
  for (method in c('susie_suff_refineTRUE_ERFALSE_ldin_AZFALSE_lamb0',
                   'susie_suff_refineTRUE_ERFALSE_ldrefout_AZFALSE_lamb0',
                   'susie_suff_refineTRUE_ERFALSE_ldrefout_AZFALSE_lamb0.001',
                   # 'susie_suff_refineTRUE_ERFALSE_ldrefout_AZFALSE_lambestimate',
                   'susie_suff_refineTRUE_ERFALSE_ldrefout_AZFALSE_lambmlelikelihood',
                   'susie_suff_refineTRUE_ERFALSE_ldrefout_AZTRUE_lamb0'
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
    labels[i] = gsub(' ERFALSE', '', labels[i])
    labels[i] = gsub(' refineTRUE', '', labels[i])
    i = i + 1
  }
  legend("bottomright", legend=labels, col=colors[1:i], lty=1, cex=0.6)
  dev.off()
  system(paste0("convert -flatten -density 120 ", output_susiesuff_refine, '.', type,'.ERFALSE_ld.pdf', " ",
                output_susiesuff_refine, '.', type, '.ERFALSE_ld.png'))

}

