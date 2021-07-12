dat1000 = readRDS('susierss_ukb_20210324_REF1000_pve005.2.rds')
dat500 = readRDS('../susierss_ukb_20210324_REF500_pve005/susierss_ukb_20210324_REF500_pve005.2.rds')

dat1000 = dat1000$susie %>% filter(method %in% c('susie_rss_refineFALSE_ERNA_ldin_AZFALSE_lambmlelikelihood',
                                                 'susie_rss_refineFALSE_ERNA_ldrefout_AZFALSE_lambmlelikelihood'))
dat500 = dat500$susie %>% filter(method %in% c('susie_rss_refineFALSE_ERNA_ldin_AZFALSE_lambmlelikelihood',
                                                 'susie_rss_refineFALSE_ERNA_ldrefout_AZFALSE_lambmlelikelihood'))

datin = dat1000 %>% filter(method == 'susie_rss_refineFALSE_ERNA_ldin_AZFALSE_lambmlelikelihood')
datref1000 = dat1000 %>% filter(method == 'susie_rss_refineFALSE_ERNA_ldrefout_AZFALSE_lambmlelikelihood')
datref500 = dat500 %>% filter(method == 'susie_rss_refineFALSE_ERNA_ldrefout_AZFALSE_lambmlelikelihood')

datins = sapply(datin$adjustld_med.ldinfo, function(x) x$lamb)
datref1000s = sapply(datref1000$adjustld_med.ldinfo, function(x) x$lamb)
datref500s = sapply(datref500$adjustld_med.ldinfo, function(x) x$lamb)

pdf('compare_s.pdf', width =15, height = 7, pointsize=24)
par(mfrow=c(1,2))
plot(datins, datref1000s, pch=16, main = expression(paste('Estimated ', hat(lambda))),
     xlab = 'in-sample LD', ylab = 'reference LD with 1000 samples')
abline(0,1)
plot(datref1000s, datref500s, pch=16, main = expression(paste('Estimated ', hat(lambda))),
     xlab = 'reference LD with 1000 samples', ylab = 'reference LD with 500 samples')
abline(0,1)
dev.off()

