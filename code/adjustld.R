library(data.table)
z = sumstats$bhat/sumstats$shat;
r = as.matrix(fread(ld[[ldmethod]]));

if(addz){
  if(ldmethod == 'refin_sample' || ldmethod == 'refout_sample'){
    if (is.null(N_ref)) stop("Cannot use addz out sample LD when N_out is not available (NULL)")
    r = (r*(N_ref-1) + tcrossprod(z))/N_ref;
    if(rcor){
      r = cov2cor(r);
    }
    r = (r + t(r))/2;
  }else{
    r = (r*(n-1) + tcrossprod(z))/n;
    if(rcor){
      r = cov2cor(r);
    }
    r = (r + t(r))/2;
  }
}

if(lamb == 'estimate'){
  eigenR = eigen(r, symmetric = T)
  colspace = which(eigenR$values > 1e-8)
  if(length(colspace) == length(z)){
    lamb = 0
  }else{
    znull = crossprod(eigenR$vectors[,-colspace], z) # U2^T z
    lamb = sum(znull^2)/length(znull)
  }
}

if(lamb == 'mlelikelihood'){
  eigenR = eigen(r, symmetric = T)
  eigenR$values[eigenR$values <= 1e-8] = 0
  negloglikelihood = function(s, z, eigenld){
    0.5 * sum(log((1-s)*eigenld$values+s)) + 
      0.5 * sum(z * eigenld$vectors %*% ((t(eigenld$vectors) * (1/((1-s)*eigenld$values + s))) %*% z))
  }
  lamb = optim(0.5, fn=negloglikelihood,
               z=z, eigenld=eigenR,
               method = 'Brent', lower=0, upper=1)$par
}

r = (1-lamb) * r + lamb * diag(length(z))

write.table(r,ldfile,quote=F,col.names=F,row.names=F)


