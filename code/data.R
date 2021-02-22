library(data.table)
library(Matrix)
library(readr)
library(digest)
# get unique seed for this genotype
str_to_int = function(x){
  h = digest(x, algo = "xxhash32")
  xx = strsplit(tolower(h), "")[[1L]]
  pos = match(xx, c(0L:9L, letters[1L:6L]))
  sum((pos - 1L) * 4^(rev(seq_along(xx) - 1)))
}
seed = str_to_int(dataset)

# read genotypes
geno <- fread(paste0(dataset, '.raw.gz'),sep = "\t", 
              header = TRUE, stringsAsFactors = FALSE)
class(geno) <- "data.frame"
# Extract the genotypes.
X <- as(as.matrix(geno[-(1:6)]),'dgCMatrix')

# Get subset of X (individuals)
n = nrow(X)
in_sample = sort(sample(1:n, GWASsample))
X.sample = X[in_sample,]
if(REFsample > 0){
  if(GWASsample == n){ # random choose individuals
    ref_sample = sample(1:n, REFsample)
  }else{ # choose individuals different from samples
    ref_sample_out = sort(sample(setdiff(1:n, in_sample), REFsample))
    ref_sample_in = sort(sample(in_sample, REFsample))
  }
  X.ref.in = X[ref_sample_in,]
  X.ref.out = X[ref_sample_out,]
}else{
  X.ref.in = X.ref.out = NA
}

rm(X)

# Remove invariant SNPs
sample.idx = apply(X.sample, 2, var, na.rm=TRUE) != 0
if (all(!is.na(X.ref.in))) {
  ref.idx.in = apply(X.ref.in, 2, var, na.rm=TRUE) != 0
  ref.idx.out = apply(X.ref.out, 2, var, na.rm=TRUE) != 0
} else {
  ref.idx.in = ref.idx.out = 1
}
choose.idx = which(sample.idx * ref.idx.in * ref.idx.out == 1)
X.sample = X.sample[, choose.idx]
if (all(!is.na(X.ref.in))){
  X.ref.in = X.ref.in[, choose.idx]
  X.ref.out = X.ref.out[, choose.idx]
}

# Compute MAF
maf.sample = apply(X.sample, 2, function(x) sum(x)/(2*length(x)))
maf.sample = pmin(maf.sample, 1-maf.sample)
if (all(!is.na(X.ref.in))) {
    maf.ref.in = apply(X.ref.in, 2, function(x) sum(x)/(2*length(x)))
    maf.ref.in = pmin(maf.ref.in, 1-maf.ref.in)
    
    maf.ref.out = apply(X.ref.out, 2, function(x) sum(x)/(2*length(x)))
    maf.ref.out = pmin(maf.ref.out, 1-maf.ref.out)
} else {
    maf.ref.in = maf.ref.out = NA
}

# Filter MAF (UKB genotypes already have MAF > 0.01)
if (maf_thresh > 0) {
    sample.idx = maf.sample > maf_thresh
    if (all(!is.na(X.ref.in))) {
        ref.idx.in = maf.ref.in > maf_thresh
        ref.idx.out = maf.ref.out > maf_thresh
    } else {
        ref.idx.in = ref.idx.out = 1
    }
    overlap.idx = which(sample.idx*ref.idx.in*ref.idx.out == 1)
    X.sample = X.sample[, overlap.idx]
    maf.sample = maf.sample[overlap.idx]
    if (all(!is.na(X.ref.in))) {
        X.ref.out = X.ref.out[, overlap.idx]
        maf.ref.out = maf.ref.out[overlap.idx]
        X.ref.in = X.ref.in[, overlap.idx]
        maf.ref.in = maf.ref.in[overlap.idx]
    }
}else{
  overlap.idx = 1:length(choose.idx)
}

# subset SNPs
snps = fread(paste0(dataset, '.pvar'))
signal_pos = gsub('^.*height.chr\\d*.', '', dataset)
pos = max(which(snps$POS[choose.idx[overlap.idx]] <= as.integer(signal_pos)))
X.idx = get_genotype(subset, ncol(X.sample), pos)
X.sample = X.sample[, X.idx]
maf.sample = maf.sample[X.idx]
if(all(!is.na(X.ref.in))){
  X.ref.in = X.ref.in[,X.idx]
  maf.ref.in = maf.ref.in[X.idx]
  X.ref.out = X.ref.out[,X.idx]
  maf.ref.out = maf.ref.out[X.idx]
}

X.sample = as.matrix(center_scale(X.sample))
X.ref.in = as.matrix(center_scale(X.ref.in))
X.ref.out = as.matrix(center_scale(X.ref.out))

r.sample = cor(as.matrix(X.sample))

if (all(!is.na(X.ref.in))) {
  r.ref.in = cor(X.ref.in)
  r.ref.out = cor(X.ref.out)
} else {
  r.ref.in = r.ref.out = NA
}

if(all(!is.na(X.ref.in))){
  r.refin.2dist = Matrix::norm(r.sample - r.ref.in, type='2')
  r.refout.2dist = Matrix::norm(r.sample - r.ref.out, type='2')
  r.refin.Mdist = max(abs(r.sample - r.ref.in))
  r.refout.Mdist = max(abs(r.sample - r.ref.out))
}else{
  r.refin.2dist = r.refin.Mdist = r.refout.2dist = r.refout.Mdist = NA
}

ind = fread(paste0(dataset, '.psam'))

write.table(ind[in_sample,c(1,2)], in_sample_id_file, quote=F, col.names=F, row.names=F)
write.table(gsub('_[A-Z]$','',colnames(X.sample)), snps_id_file, quote=F, col.names=F, row.names=F)
write.table(r.sample, ld_sample_file, quote=F, col.names=F, row.names=F)
write.table(r.ref.in, ld_refin_file, quote=F, col.names=F, row.names=F)
write.table(r.ref.out, ld_refout_file, quote=F, col.names=F, row.names=F)



