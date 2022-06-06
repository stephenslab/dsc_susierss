#' @title Compare finemap fits to truth
#' @param sets a list of susie CS info from finemap fit
#' @param pip probability for p variables
#' @param true_coef true regression coefficients
#' @return total the number of total CS
#' @return valid the number of CS that captures a true signal
#' @return size an array of size of CS
#' @return top the number of CS whose highest PIP is the true causal
finemap_scores = function(cs, pip, true_coef) {
  if (is.null(dim(true_coef))) beta_idx = which(true_coef!=0)
  else beta_idx = which(apply(true_coef, 1, sum) != 0)
  
  if (is.null(cs)) {
    size = 0
    total = 0
    valid = 0
  } else {
    size = length(cs)
    total = 1
    valid = sum(beta_idx%in%cs)
  }
  return(list(total=total, valid=valid, size=size, signal_pip = pip[beta_idx]))
}

finemap_scores_multiple = function(res, truth) {
  total = valid = size = 0
  signal_pip = list()
  pip = list()
  for (r in 1:length(res)) {
    set = res[[r]]$set
    snps = res[[r]]$snp
    n = sum(cumsum(set$config_prob) < 0.95) + 1
    if(n > nrow(set)){
      set = NULL
    }else{
      if (class(set$config) == "character") {
        set = apply(set[1:n,], 1, function(config) as.numeric(strsplit(config[2], ",")[[1]]))
        set = unique(as.vector(unlist(set)))
      } else {
        set = unique(set$config)
      }
    }
    snps = snps[order(as.numeric(snps$snp)),]
    
    out = finemap_scores(set, snps$snp_prob, truth[,r])
    total = total + out$total
    valid = valid + out$valid
    size = size + out$size
    signal_pip[[r]] = out$signal_pip
    pip[[r]] = snps$snp_prob
  }
  return(list(total=total, valid=valid, size=size, signal_pip = do.call(cbind,signal_pip), pip = do.call(cbind,pip)))
}

finemap_v1.4_scores = function(cs, pure, pip, true_coef) {
  if (is.null(dim(true_coef))) beta_idx = which(true_coef!=0)
  else beta_idx = which(apply(true_coef, 1, sum) != 0)
  
  if (is.null(cs)) {
    size = 0
    total = 0
    purity = 0
    avgr2 = 0
    purity_pure = 0
    avgr2_pure = 0
    total_pure = 0
    size_pure = 0
  } else {
    purity = as.vector(pure[,1])
    avgr2 = as.vector(pure[,2])^2
    total = length(cs)
    size = sapply(cs,length)
    pureidx = which(purity >= 0.5)
    if(length(pureidx) > 0){
      purity_pure = purity[pureidx]
      avgr2_pure = avgr2[pureidx]
      cs_pure = cs[pureidx]
      total_pure = length(cs_pure)
      size_pure = sapply(cs_pure,length)
    }else{
      purity_pure = 0
      avgr2_pure = 0
      total_pure = 0
      size_pure = 0
    }
  }
  valid = 0
  if (total > 0) {
    for (i in 1:total){
      if (any(cs[[i]]%in%beta_idx)) valid=valid+1
    }
  }
  valid_pure = 0
  if (total_pure > 0) {
    for (i in 1:total_pure){
      if (any(cs_pure[[i]]%in%beta_idx)) valid_pure=valid_pure+1
    }
  }
  return(list(total=total, valid=valid, size=size, purity=purity, avgr2=avgr2, 
              total_pure=total_pure, valid_pure=valid_pure, size_pure=size_pure, 
              purity_pure=purity_pure, avgr2_pure=avgr2_pure, 
              signal_pip = pip[beta_idx]))
}

finemap_v1.4_scores_multiple = function(res, truth) {
  total = valid = total_pure = valid_pure = vector() 
  size = size_pure = signal_pip = purity = avgr2 = purity_pure = avgr2_pure = list()
  pip = list()
  for (r in 1:length(res)) {
    set = res[[r]]$set
    pure = res[[r]]$purity
    snps = res[[r]]$snp
    snps = snps[order(as.numeric(snps$snp)),]
    out = finemap_v1.4_scores(set, pure, snps$snp_prob, truth[,r])
    pip[[r]] = snps$snp_prob
    total[r] = out$total
    valid[r] = out$valid
    size[[r]] = out$size
    purity[[r]] = out$purity
    avgr2[[r]] = out$avgr2
    total_pure[r] = out$total_pure
    valid_pure[r] = out$valid_pure
    size_pure[[r]] = out$size_pure
    purity_pure[[r]] = out$purity_pure
    avgr2_pure[[r]] = out$avgr2_pure
    signal_pip[[r]] = out$signal_pip
  }
  return(list(total=total, valid=valid, size=size, purity=purity, avgr2=avgr2,
              total_pure=total_pure, valid_pure=valid_pure, size_pure=size_pure, 
              purity_pure=purity_pure, avgr2_pure=avgr2_pure,
              signal_pip = do.call(cbind,signal_pip), pip = do.call(cbind,pip)))
}

