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
  geom_histogram(color = "black",fill = "black",bins = 32) +
  xlim(c(-25,15)) +
  theme_cowplot(font_size = 10) +
  theme(plot.title = element_text(face = "plain",size = 10)) +
  labs(x = "log-likelihood ratio",y = "SNPs",
       title = "zero effect, flipped allele")
p2 <- ggplot(subset(pdat2,flipped),aes(x = logLR)) +
  geom_histogram(color = "black",fill = "black",bins = 32) +
  xlim(c(-25,15)) +
  theme_cowplot(font_size = 10) +
  theme(plot.title = element_text(face = "plain",size = 10)) +
  labs(x = "log-likelihood ratio",y = "SNPs",
       title = "nonzero effect, flipped allele")
p3 <- ggplot(subset(pdat1,!flipped),aes(x = logLR)) +
  geom_histogram(color = "black",fill = "black",bins = 32) +
  xlim(c(-25,15)) +
  scale_y_continuous(trans = "log10",breaks = 10^seq(0,5)) +
  theme_cowplot(font_size = 10) +
  theme(plot.title = element_text(face = "plain",size = 10)) +
  labs(x = "log-likelihood ratio",y = "SNPs",
       title = "zero effect, no flipped allele")
p4 <- ggplot(subset(pdat2,!flipped),aes(x = logLR)) +
  geom_histogram(color = "black",fill = "black",bins = 32) +
  xlim(c(-25,15)) +
  scale_y_continuous(trans = "log10",breaks = 10^seq(0,5)) +
  theme_cowplot(font_size = 10) +
  theme(plot.title = element_text(face = "plain",size = 10)) +
  labs(x = "log-likelihood ratio",y = "SNPs",
       title = "nonzero effect, no flipped allele")
p5 <- ggplot(subset(pdat1,!flipped & abs(z) > 2),aes(x = logLR)) +
  geom_histogram(color = "black",fill = "black",bins = 32) +
  xlim(c(-25,15)) +
  scale_y_continuous(trans = "log10",breaks = 10^seq(0,5)) +
  theme_cowplot(font_size = 10) +
  theme(plot.title = element_text(face = "plain",size = 10)) +
  labs(x = "log-likelihood ratio",y = "SNPs",
       title = "zero effect, no flipped allele, |z| > 2")
p6 <- ggplot(subset(pdat2,!flipped & abs(z) > 2),aes(x = logLR)) +
  geom_histogram(color = "black",fill = "black",bins = 32) +
  xlim(c(-25,15)) +
  scale_y_continuous(trans = "log10",breaks = 10^seq(0,5)) +
  theme_cowplot(font_size = 10) +
  theme(plot.title = element_text(face = "plain",size = 10)) +
  labs(x = "log-likelihood ratio",y = "SNPs",
       title = "nonzero effect, no flipped allele, |z| > 2")
p <- plot_grid(p1,p2,p3,p4,p5,p6,nrow = 3,ncol = 2,align = "v")
ggsave("allele_flip_diagnostic.eps",p,height = 6,width = 5)
