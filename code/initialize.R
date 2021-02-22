library(glmnet)
init_susie_true = function(true_coef) {
  p = nrow(true_coef)
  s = list()
  for (r in 1:ncol(true_coef)) {
    beta_idx = which(true_coef[,r] != 0)
    beta_val = true_coef[,r][beta_idx]
    if(length(beta_idx) == 0){
      s[[r]] = NA
    }else{
      s[[r]] = susieR::susie_init_coef(beta_idx, beta_val, p)
    }
  }
  return(s)
}

init_lasso = function(X, Y, L){
  p = ncol(X)
  s = list()
  for (r in 1:ncol(Y)){
    fit.lasso = glmnet::glmnet(X, Y[,r], family="gaussian", alpha=1, dfmax = L)
    lasso.b = fit.lasso$beta[,max(which(fit.lasso$df <= L))]
    beta_idx = which(lasso.b != 0)
    if(length(beta_idx) == 0){
      s[[r]] = NA
    }else{
      sumstat = susieR:::univariate_regression(X[, beta_idx], Y[,r])
      s[[r]] = susieR::susie_init_coef(beta_idx, sumstat$betahat, p)
    }    
  }
  return(s)
}

init_susie_rss_true = function(true_coef, n) {
  p = nrow(true_coef)
  s = list()
  for (r in 1:ncol(true_coef)) {
    beta_idx = which(true_coef[,r] != 0)
    beta_val = true_coef[,r][beta_idx] * sqrt(n)
    if(length(beta_idx) == 0){
      s[[r]] = NA
    }else{
      s[[r]] = susieR::susie_init_coef(beta_idx, beta_val, p)
    }
  }
  return(s)
}

init_rss_lasso = function(Z, R, L){
  p = nrow(Z)
  s = list()
  eigenR = eigen(R, symmetric = T)
  eigenR$values[abs(eigenR$values) < 1e-08] = 0
  P = t(eigenR$vectors[,eigenR$values!=0]) * eigenR$values[eigenR$values!=0]^(-0.5)

  for (r in 1:ncol(Z)){
    fit.lasso = glmnet::glmnet(t(eigenR$vectors[,eigenR$values!=0]) * eigenR$values[eigenR$values!=0]^(0.5), P%*%Z[,r], family="gaussian", alpha=1, dfmax = L)
    lasso.b = fit.lasso$beta[,max(which(fit.lasso$df <= L))]
    beta_idx = which(lasso.b != 0)
    if(length(beta_idx) == 0){
      s[[r]] = NA
    }else{
      s[[r]] = susieR::susie_init_coef(beta_idx, Z[beta_idx, r], p)
    }
  }
  return(s)
}
