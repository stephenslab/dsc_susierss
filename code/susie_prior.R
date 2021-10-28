susie_prior = function(fit, true_coef){
  if(is.null(fit$sets)){
    return(list(valid = c(), V_cs = c(), V = fit$V))
  }
  if (is.null(dim(true_coef))) beta_idx = which(true_coef!=0)
  else beta_idx = which(apply(true_coef, 1, sum) != 0)
  
  cs = fit$sets$cs
  if(length(cs) != 0){
    valid = numeric(length(cs))
    cs_idx = as.numeric(gsub('L', '', names(cs)))
    V_cs = fit$V[cs_idx]
    for (i in 1:length(cs)){
      if (any(cs[[i]]%in%beta_idx)){
        valid[i] = 1
      }
    }
  }else{
    valid = V_cs = c()
  }
  return(list(valid = valid, V_cs = V_cs, V = fit$V))
}

susie_prior_multiple = function(fits, true_coef){
  V_cs = valid = V = list()
  for (r in 1:length(fits)) {
    out = susie_prior(fits[[r]], true_coef[,r])
    V_cs[[r]] = out$V_cs
    valid[[r]] = out$valid
    V[[r]] = out$V
  }
  return(list(valid = valid, V_cs = V_cs, V = V))
}

