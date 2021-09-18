library(dplyr)
#' @title Check if produced confidence sets have overlaps
#' @param cs a list a susie confidence sets from susie fit
#' @return a boolean 1 if overlap, 0 otherwise
check_overlap = function(cs) {
  if (length(cs) == 0) {
    return(0)
  } else {
    overlaps = 0
    for (i in 1:length(cs)) {
      for (j in 1:i) {
        if (i == j) next
        overlaps = overlaps + length(intersect(cs[[i]], cs[[j]]))
      }
    }
    return(overlaps)
  }
}


#' @title Compare dap fits to truth
#' @param sets a list of susie CS info from susie fit
#' @param pip probability for p variables
#' @param true_coef true regression coefficients
#' @return total the number of total CS
#' @return valid the number of CS that captures a true signal
#' @return size an array of size of CS
#' @return angr2 an array of average R^2 of CS
#' @return top the number of CS whose highest PIP is the true causal
dap_scores = function(sets, pip, true_coef, ld) {
  if (is.null(dim(true_coef))) beta_idx = which(true_coef!=0)
  else beta_idx = which(apply(true_coef, 1, sum) != 0)
  
  if(is.null(sets)){
    return(list(total_cluster=NA, valid_cluster=NA, 
                size_cluster=NA, purity_cluster=NA, 
                avgr2_cluster=NA, top_cluster=NA,
                has_overlap_cluster=NA, 
                total_cs=NA, valid_cs=NA, 
                size_cs=NA, purity_cs = NA, 
                avgr2_cs=NA, top_cs=NA,
                has_overlap_cs=NA, 
                signal_pip = NA))
  }
  
  if(nrow(sets) > 0){
    snp_clusters = pip %>% filter(cluster %in% sets$cluster)
    clusters = lapply(sort(unique(snp_clusters$cluster)), function(i) snp_clusters$snp[snp_clusters$cluster == i])
    
    ## construct 95% CS
    cs = lapply(sort(unique(snp_clusters$cluster)), 
                function(i){
                  id = which(pip$cluster == i)
                  n = sum(cumsum(sort(pip$snp_prob[id],decreasing = TRUE)) < 0.95) +1
                  o = order(pip$snp_prob[id], decreasing = TRUE)
                  pip$snp[id[o[1:n]]]
                })
    size_cluster = as.vector(sets$size)
    total_cluster = nrow(sets)
    purity_cluster = sapply(clusters, function(x){
      value = abs(ld[x, x])
      min(value)
    })
    avgr2_cluster = sapply(clusters, function(x){
      value = abs(ld[x, x])
      (mean(value))^2
    })
    
    size_cs = sapply(cs,length)
    total_cs = length(cs)
    purity_cs = sapply(cs, function(x){
      value = abs(ld[x, x])
      min(value)
    })
    avgr2_cs = sapply(cs, function(x){
      value = abs(ld[x, x])
      (mean(value))^2
    })
  }else{
    total_cluster=0
    size_cluster=0
    purity_cluster=0
    avgr2_cluster=0

    total_cs=0
    size_cs=0
    purity_cs =0
    avgr2_cs=0
  }

  valid_cluster = 0
  if (total_cluster > 0) {
    for (i in 1:total_cluster){
      if (any(clusters[[i]]%in%beta_idx)) valid_cluster=valid_cluster+1
    }
  }
  valid_cs = 0
  if(total_cs > 0){
    for (i in 1:total_cs){
      if (any(cs[[i]]%in%beta_idx)) valid_cs=valid_cs+1
    }
  }
  return(list(total_cluster=total_cluster, valid_cluster=valid_cluster, 
              size_cluster=size_cluster, purity_cluster=purity_cluster,
              avgr2_cluster=avgr2_cluster,
              total_cs=total_cs, valid_cs=valid_cs,
              size_cs=size_cs, purity_cs=purity_cs,
              avgr2_cs=avgr2_cs, signal_pip = pip$snp_prob[beta_idx]))
}

dap_scores_multiple = function(res, truth, ld, set_threshold = 0.95) {
  total_cluster = valid_cluster = vector()
  total_cs = valid_cs = overlap_cs = vector()
  signal_pip = size_cluster = avgr2_cluster = purity_cluster = list()
  size_cs = avgr2_cs = purity_cs = list()
  pip = list()
  for (r in 1:length(res)) {
    set = res[[r]]$sets
    snps = res[[r]]$pip
    snps = snps[order(as.numeric(snps$snp)),]
    set = set[which(set$cluster_prob >= set_threshold),]
    out = dap_scores(set, snps, truth[,r], ld)
    total_cluster[r] = out$total_cluster
    valid_cluster[r] = out$valid_cluster
    size_cluster[[r]] = out$size_cluster
    avgr2_cluster[[r]] = out$avgr2_cluster
    purity_cluster[[r]] = out$purity_cluster
    
    total_cs[r] = out$total_cs
    valid_cs[r] = out$valid_cs
    size_cs[[r]] = out$size_cs
    avgr2_cs[[r]] = out$avgr2_cs
    purity_cs[[r]] = out$purity_cs
    
    signal_pip[[r]] = out$signal_pip
    pip[[r]] = snps$snp_prob
  }
  return(list(total_cluster=total_cluster, valid_cluster=valid_cluster, 
              size_cluster=size_cluster, avgr2_cluster=avgr2_cluster, 
              purity_cluster=purity_cluster,
              total_cs=total_cs, valid_cs=valid_cs, 
              size_cs=size_cs, avgr2_cs=avgr2_cs, 
              purity_cs=purity_cs,
              signal_pip = do.call(cbind, signal_pip), pip = do.call(cbind, pip)))
}
