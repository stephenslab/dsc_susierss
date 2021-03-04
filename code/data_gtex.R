data = readRDS(dataset)
n = nrow(data$X)
X.all = data$X

if (subsample<1) {
    in_sample = sample(1:n, ceiling(n*subsample))
    X.sample = X.all[in_sample,]
    X.out = X.all[-in_sample,]
} else {
    X.sample = X.all
    X.out = NA
}
# Remove invariant sites
sample.index = apply(X.sample, 2, var, na.rm=TRUE) != 0
if (all(!is.na(X.out))) {
    out.index = apply(X.out, 2, var, na.rm=TRUE) != 0
} else {
    out.index = 1
}
choose.index = as.logical(sample.index * out.index)
X.sample = X.sample[, choose.index]
if (all(!is.na(X.out))) X.out = X.out[, choose.index]
# Apply MAF filter
maf.sample = apply(X.sample, 2, function(x) sum(x)/(2*length(x)))
maf.sample = pmin(maf.sample, 1-maf.sample)
if (all(!is.na(X.out))) {
    maf.out = apply(X.out, 2, function(x) sum(x)/(2*length(x)))
    maf.out = pmin(maf.out, 1-maf.out)
} else {
    maf.out = NA
}
if (maf_thresh > 0) {
    sample.idx = maf.sample > maf_thresh
    if (all(!is.na(X.out))) {
        out.idx = maf.out > maf_thresh
    } else {
        out.idx = 1
    }
    overlap.idx = as.logical(sample.idx*out.idx)
    X.sample = X.sample[, overlap.idx]
    maf.sample = maf.sample[overlap.idx]
    if (all(!is.na(X.out))) {
        X.out = X.out[, overlap.idx]
        maf.out = maf.out[overlap.idx]
    }
}

# get SNPs
idx = get_genotype(subset, ncol(X.sample))
X.sample = X.sample[,idx]
maf.sample = maf.sample[idx]
if (all(!is.na(X.out))){
  X.out = X.out[,idx]
  maf.out = maf.out[idx]
}

# Center & scale data
X.sample = center_scale(X.sample)
if (all(!is.na(X.out))) X.out = center_scale(X.out)
# Get LD (correlations)
r.sample = cor(X.sample)
if (all(!is.na(X.out))) {
    r.out = cor(X.out)
} else {
    r.out = NA
}

if(all(!is.na(X.out))){
  r.out.2dist = Matrix::norm(r.sample - r.out, type='2')
  r.out.Mdist = max(abs(r.sample - r.out))
}else{
  r.out.2dist = r.out.Mdist = NA
}

write.table(r.sample,ld_sample_file,quote=F,col.names=F,row.names=F)
write.table(r.out,ld_out_file,quote=F,col.names=F,row.names=F)
