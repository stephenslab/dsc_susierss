library(ggplot2)
library(cowplot)

# Load the results of the simulations.
null   <- readRDS("../output/susierss_diagnostic_ukb_flipnull.rds")
signal <- readRDS("../output/susierss_diagnostic_ukb_flipsignal.rds")
pdat1  <-
  data.frame(flipped = do.call("c",lapply(null,function (x) x$post$flipped)),
             z       = do.call("c",lapply(null,function (x) x$post$z)),
             logLR   = do.call("c",lapply(null,function (x) x$post$logLR)))
pdat2  <-
  data.frame(flipped = do.call("c",lapply(signal,function (x) x$post$flipped)),
             z       = do.call("c",lapply(signal,function (x) x$post$z)),
             logLR   = do.call("c",lapply(signal,function (x) x$post$logLR)))

# Plot distribution of log-likelihood ratios, separately in the effect
# SNPs and non-effect SNPs that are either allele flips or not allele
# flips.
pdat1 <- transform(pdat1,logLR = ifelse(logLR < -50,NA,logLR))
pdat2 <- transform(pdat2,logLR = ifelse(logLR < -50,NA,logLR))
p1 <- ggplot(subset(pdat1,flipped),aes(x = logLR)) +
  geom_histogram(color = "darkblue",fill = "darkblue",bins = 32) +
  xlim(c(-25,15)) +
  theme_cowplot(font_size = 10) +
  theme(plot.title = element_text(face = "plain",size = 10)) +
  labs(x = "log-likelihood ratio",y = "SNPs",
       title = "non-effect SNPs, flipped allele")
p2 <- ggplot(subset(pdat2,flipped),aes(x = logLR)) +
  geom_histogram(color = "darkblue",fill = "darkblue",bins = 32) +
  xlim(c(-25,15)) +
  theme_cowplot(font_size = 10) +
  theme(plot.title = element_text(face = "plain",size = 10)) +
  labs(x = "log-likelihood ratio",y = "SNPs",
       title = "effect SNPs, flipped allele")
p3 <- ggplot(subset(pdat1,!flipped),aes(x = logLR)) +
  geom_histogram(color = "darkorange",fill = "darkorange",bins = 32) +
  xlim(c(-25,15)) +
  scale_y_log10() +
  theme_cowplot(font_size = 10) +
  theme(plot.title = element_text(face = "plain",size = 10)) +
  labs(x = "log-likelihood ratio",y = "SNPs",
       title = "non-effect SNPs, no flipped allele")
p4 <- ggplot(subset(pdat2,!flipped),aes(x = logLR)) +
  geom_histogram(color = "darkorange",fill = "darkorange",bins = 32) +
  xlim(c(-25,15)) +
  scale_y_log10() +
  theme_cowplot(font_size = 10) +
  theme(plot.title = element_text(face = "plain",size = 10)) +
  labs(x = "log-likelihood ratio",y = "SNPs",
       title = "effect SNPs, no flipped allele")
p <- plot_grid(p1,p2,p3,p4,nrow = 2,ncol = 2,align = "v")
ggsave("allele_flip_diagnostic.eps",p,height = 4,width = 5)
