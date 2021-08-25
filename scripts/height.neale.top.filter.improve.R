library(readr)
library(dplyr)
neale.file <-
  file.path("/gpfs/data/stephens-lab/finemap-uk-biobank/data/summary/neale",
            "50_irnt.gwas.imputed_v3.both_sexes.tsv.gz")

cat("Loading Neale lab association results.\n")
neale <- suppressMessages(read_delim(neale.file,delim = "\t",progress = FALSE))
class(neale) <- "data.frame"
neale        <- neale[c("variant","minor_AF","beta","se","tstat","pval")]
out       <- strsplit(neale$variant,":",fixed = TRUE)
neale$chr <- factor(sapply(out,function (x) x[[1]]))
neale$pos <- as.numeric(sapply(out,function (x) x[[2]]))

# filter MAF
neale = neale %>% filter(minor_AF > 0.01)

# select significant signals
# neale = neale %>% filter(pval < 5e-8)

# filter on chr: delete chr 6 and X
neale = neale %>% filter(! (chr %in% c(6, 'X')))

dat = neale[, c('chr', 'pos', 'pval')]

res = matrix(NA, 0, 5)

for( i in 1:2000){
    idx = which.min(dat$pval)
    tmp = as.numeric(c( as.character(dat$chr[idx]), dat$pos[idx], dat$pval[idx], dat$pos[idx] - 500000, dat$pos[idx] + 500000 ))
    if(i == 1){
        res = rbind(res, tmp)
        dat[which(dat$chr == tmp[1] & dat$pos <= tmp[5] & dat$pos >= tmp[4]),] = NA
    }else{
        # get previous result on the same chrom
        prev = which( res[,1] == tmp[1] )
        if(length(prev) > 0){
            res.sub = res[prev,,drop=FALSE]
            if(any(res.sub[,4] <= tmp[4] & res.sub[,5] >= tmp[4]) | any(res.sub[,4] <= tmp[5] & res.sub[,5] >= tmp[5]) ){
                dat[idx,] = NA
            }else{
                res = rbind(res, tmp)
                dat[which(dat$chr == tmp[1] & dat$pos <= tmp[5] & dat$pos >= tmp[4]),] = NA
            }
        }else{
            res = rbind(res, tmp)
            dat[which(dat$chr == tmp[1] & dat$pos <= tmp[5] & dat$pos >= tmp[4]),] = NA
        }
    }
}
res[,1] = as.numeric(res[,1])
colnames(res) = c('chr', 'pos', 'pval', 'start', 'end')

res.sub = res[,c(1,4,5,2)]

write.table(res, file='../output/height.top2000.filter.wpvalue.txt', quote = FALSE, row.names = FALSE)
write.table(res.sub, file='../output/height.top2000.filter.txt', quote = FALSE, row.names = FALSE)

