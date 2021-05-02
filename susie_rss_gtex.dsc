#!/usr/bin/env dsc

%include modules/data
%include modules/simulate
%include modules/get_sumstats
%include modules/fit
%include modules/evaluate

DSC:
  define:
    method_susie: susie_suff, susie_rss
    method_finemapv4: finemapv4, finemapv4L4
  run:
    default: small_data_gtex * lm_pve02 * get_sumstats * (adjustld_gtex, adjustld_addz_gtex) * ((susie_suff, susie_rss) * score_susie, method_finemapv4 * score_finemapv4)
  exec_path: code
  replicate: 2
  global:
    data_file: data/gtex-manifest.txt
    prop_samples: 0.5
    ## unused
    GWAS_sample: 50000
    REF_sample: 1000
    n_dataset: 200
  output: output/susierss_gtex

