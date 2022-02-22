library(data.table)
z = sumstats$bhat/sumstats$shat;

if(ldmethod == 'in_sample'){
  ldfile = ldn
}else if(ldmethod == 'refout_sample'){
  ldfile = ld[[ldmethod]]
}

ld_info = list(ldfile = ldfile, ldmethod = ldmethod, lamb = 0)