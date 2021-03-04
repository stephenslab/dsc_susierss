#!/usr/bin/env dsc

%include modules/data
%include modules/simulate
%include modules/get_sumstats
%include modules/fit
%include modules/evaluate

DSC:
  define:
    method_susie: susie_suff, susie_rss, susie_rss_lambda, susie_rss_scale, susie_rss_lambda_scale
    method_caviar: caviar
    method_finemap: finemap, finemap_scale
    method_finemapv4: finemapv4, finemapv4_scale
  run:
    default: small_data * sim_gaussian * get_sumstats * (method_susie * score_susie, method_caviar * score_caviar, method_finemapv4 * score_finemapv4, method_finemap * score_finemap)
    test: small_data * sim_gaussian * get_sumstats * finemap * score_finemap
  exec_path: code
  global:
    data_file: data/ukb_genotypes_files.txt
    GWAS_sample: 50000
    REF_sample: 1000
    n_dataset: 200
    # unused
    prop_samples: 0.5
  output: output/susierss_ukb
