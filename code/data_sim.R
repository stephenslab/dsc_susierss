# Get subset of X
n = nrow(X)
X.idx = get_genotype(subset, ncol(X), pos)
X.all = X[, X.idx]
in_sample = sample(1:n, GWASsample)
X.sample = X.all[in_sample,]
if(REFsample > 0){
  if(GWASsample == n){ # random choose individuals
    ref_sample = sample(1:n, REFsample)
  }else{ # choose individuals different from samples
    ref_sample = sample(setdiff(1:n, in_sample), REFsample)
  }
  X.ref = X.all[ref_sample,]
}else{
  X.ref = NA
}

# Remove invariant sites
sample.index = apply(X.sample, 2, var, na.rm=TRUE) != 0
if (all(!is.na(X.ref))) {
    ref.index = apply(X.ref, 2, var, na.rm=TRUE) != 0
} else {
    ref.index = 1
}
choose.index = as.logical(sample.index * ref.index)
X.sample = X.sample[, choose.index]
if (all(!is.na(X.ref))) X.ref = X.ref[, choose.index]
# # Apply MAF filter
# maf.sample = apply(X.sample, 2, function(x) sum(x)/(2*length(x)))
# maf.sample = pmin(maf.sample, 1-maf.sample)
# if (!is.na(X.ref)) {
#     maf.ref = apply(X.ref, 2, function(x) sum(x)/(2*length(x)))
#     maf.ref = pmin(maf.ref, 1-maf.ref)
# } else {
#     maf.ref = NA
# }
# if (maf_thresh > 0) {
#     sample.idx = maf.sample > maf_thresh
#     if (!is.na(X.ref)) {
#         ref.idx = maf.ref > maf_thresh
#     } else {
#         ref.idx = 1
#     }
#     overlap.idx = as.logical(sample.idx*ref.idx)
#     X.sample = X.sample[, overlap.idx]
#     maf.sample = maf.sample[overlap.idx]
#     if (!is.na(X.ref)) {
#         X.ref = X.ref[, overlap.idx]
#         maf.ref = maf.ref[overlap.idx]
#     }
# }

# Center & scale data
X.sample = center_scale(X.sample)
if (all(!is.na(X.ref))) X.ref = center_scale(X.ref)
# Get LD (correlations)
if(GWASsample == n){
  XtX = as.matrix(fread(XtX_full))
  XtX = XtX[X.idx, X.idx]
  r.sample = cov2cor(XtX)
}else{
  r.sample = cor(X.sample)  
}
if (all(!is.na(X.ref))) {
    r.ref = cor(X.ref)
} else {
    r.ref = NA
}
write.table(r.sample, ld_sample_file, quote=F, col.names=F, row.names=F)
write.table(r.ref, ld_ref_file, quote=F, col.names=F, row.names=F)
