library(data.table)
r = as.matrix(fread(ld[['in_sample']]))

diag(r) = 0

rnum_0.9 = apply(r, 1, function(x) sum(abs(x) > 0.9))

rnum_0.99 = apply(r, 1, function(x) sum(abs(x) > 0.99))

X_names = sapply(strsplit(colnames(X_sample), '_'), function(x) x[[1]])
info = data.table::fread(paste0(X_file, '.pvar'))
bp_length = diff(range(info$POS[match(X_names, info$ID)]))
