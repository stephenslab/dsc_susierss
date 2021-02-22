library(susieR)
fitted <- list()
posterior <- list()

fitted <- list()
posterior <- list()
for (r in 1:ncol(Y)) {
  if (prior_var == 'auto') {
      fitted[[r]] <- susie_auto(X,Y[,r],L_max=100,tol=1e-3)
  } else if (prior_var == 0) {
      if(is.na(s_init)){
        fitted[[r]] <- susie(X,Y[,r],L=maxL,
                             max_iter=maxI,
                             estimate_residual_variance=estimate_residual_variance,
                             estimate_prior_variance=TRUE,
                             null_weight=null_weight,
                             tol=1e-3, s_init = NULL)
      }else{
        fitted[[r]] <- susie(X,Y[,r],L=maxL,
                             max_iter=maxI,
                             estimate_residual_variance=estimate_residual_variance,
                             estimate_prior_variance=TRUE,
                             null_weight=null_weight,
                             tol=1e-3, s_init = s_init[[r]])
      }
  } else {
      if(is.na(s_init)){
        fitted[[r]] <- susie(X,Y[,r],L=maxL,
                             max_iter=maxI,
                             estimate_residual_variance=estimate_residual_variance,
                             scaled_prior_variance=prior_var,
                             null_weight=null_weight,
                             tol=1e-3, s_init = NULL)
      }else{
        fitted[[r]] <- susie(X,Y[,r],L=maxL,
                             max_iter=maxI,
                             estimate_residual_variance=estimate_residual_variance,
                             scaled_prior_variance=prior_var,
                             null_weight=null_weight,
                             tol=1e-3, s_init = s_init[[r]])
      }
  }
  posterior[[r]] <- summary(fitted[[r]])
}
