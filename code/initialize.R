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
    fit.lassocv = glmnet::cv.glmnet(X, Y[,r], family="gaussian", alpha=1)
    lassocv.b = as.vector(coef(fit.lassocv, s = "lambda.min"))[-1]
    beta_idx <- sort(abs(lassocv.b), index.return=TRUE, decreasing=TRUE)$ix[1:L]
    beta_idx = beta_idx[lassocv.b[beta_idx]!=0]
    # fit.lasso = glmnet::glmnet(X, Y[,r], family="gaussian", alpha=1, dfmax = L)
    # lasso.b = fit.lasso$beta[,max(which(fit.lasso$df <= L))]
    # beta_idx = which(lasso.b != 0)
    if(length(beta_idx) == 0){
      s[[r]] = NA
    }else{
      s[[r]] = susieR::susie_init_coef(beta_idx, lassocv.b[beta_idx], p)
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
  X = t(eigenR$vectors[,eigenR$values!=0]) * eigenR$values[eigenR$values!=0]^(0.5)
  for (r in 1:ncol(Z)){
    fit.lassocv = glmnet::cv.glmnet(X, P%*%Z[,r], family="gaussian", alpha=1, standardize = FALSE, intercept = FALSE)
    lassocv.b = as.vector(coef(fit.lassocv, s = "lambda.min"))
    beta_idx <- sort(abs(lassocv.b), index.return=TRUE, decreasing=TRUE)$ix[1:L]
    beta_idx = beta_idx[lassocv.b[beta_idx]!=0]
    # fit.lasso = glmnet::glmnet(X, P%*%Z[,r], family="gaussian", alpha=1, dfmax = L)
    # lasso.b = fit.lasso$beta[,max(which(fit.lasso$df <= L))]
    # beta_idx = which(lasso.b != 0)
    if(length(beta_idx) == 0){
      s[[r]] = NA
    }else{
      s[[r]] = susieR::susie_init_coef(beta_idx, lassocv.b[beta_idx, r], p)
    }
  }
  return(s)
}
