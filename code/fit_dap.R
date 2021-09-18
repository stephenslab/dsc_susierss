#' PAINTOR I/O
library(data.table)
write_dap_z <- function(Z, prefix) {
  write.table(Z, paste0(prefix, '.z'), quote = F, row.names=T, col.names=F, sep=' ')
  return(paste0(prefix, '.z'))
}

run_dap_z <- function(Z, LD_file, args = "", prefix="data"){
  zfile = write_dap_z(Z, prefix)
  print(zfile)
  cmd = paste0("code/linux/dap-g -d_z ", zfile, ' -d_ld ', LD_file, ' ',
               '-o ', prefix, '.result', ' --output_all ', 
               '-t 1 ',
               args)
  print(cmd)
  write("Running shell command:", stderr())
  write(cmd, stderr())
  out <- system(cmd)
  print(out)
  # dscrutils::run_cmd(cmd, quit_on_error = FALSE)
  if(!file.exists(paste0(prefix, '.result'))){
    stop("Cannot find post file")
  }
  
  # get PIPs
  cmd = paste0('grep "((" ', prefix, '.result > ', prefix, '.pip')
  dscrutils::run_cmd(cmd)
  
  # get clusters
  cmd = paste0('grep "{" ', prefix, '.result > ', prefix, '.cluster')
  dscrutils::run_cmd(cmd)
  
  pips = fread(paste0(prefix, '.pip'))
  pips = pips[,2:5]
  colnames(pips) = c('snp', 'snp_prob', 'snp_log10bf', 'cluster')
  pips = pips[order(pips$snp),]
    
  clusters = fread(paste0(prefix, '.cluster'))
  if(nrow(clusters) != 0){
    clusters = clusters[,c(2,4,6,7)]
    colnames(clusters) = c('cluster', 'size', 'cluster_prob', 'cluster_avg_r2')
    clusters$cluster = gsub('\\{', '', clusters$cluster)
    clusters$cluster = gsub('\\}', '', clusters$cluster)
    clusters$cluster = as.numeric(clusters$cluster)
  }
  return(list(pip = pips, sets = clusters))
}

finemap_dap <- function(Z, LD_file, args, prefix, parallel = FALSE) {
  if (is.null(dim(Z))) {
    Z = matrix(ncol=1,Z)
  }
  res = list()
  for(r in 1:ncol(Z)){
    res[[r]] = run_dap_z(Z[,r], LD_file, args, paste0(prefix, '_condition_', r))
  }
  return(res)
}
