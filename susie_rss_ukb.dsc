#!/usr/bin/env dsc

%include modules/data
%include modules/simulate
%include modules/get_sumstats
%include modules/fit
%include modules/evaluate

DSC:
  define:
    method_susie: susie_suff, susie_rss, susie_rss_lambda
  run:
    default: small_data * sim_gaussian * get_sumstats * method_susie * score_susie
  exec_path: code
  global:
    data_file: data/ukb_genotypes_files.txt
    GWAS_sample: 50000
    REF_sample: 1000
    n_dataset: 200
  output: output/susierss_ukb
