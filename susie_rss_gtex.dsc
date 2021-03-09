#!/usr/bin/env dsc

%include modules/data
%include modules/simulate
%include modules/get_sumstats
%include modules/fit
%include modules/evaluate

DSC:
  define:
    method_susie: susie_suff_gtex, susie_suff_addz_gtex, susie_rss_gtex, susie_rss_addz_gtex, susie_rss_suff_gtex, susie_rss_suff_addz_gtex
  run:
    default: small_data_gtex * lm_pve02 * get_sumstats * (method_susie * score_susie, finemapv4_gtex * score_finemapv4, finemap_gtex * score_finemap)
  exec_path: code
  global:
    data_file: data/gtex-manifest.txt
    prop_samples: 0.5
    ## unused
    GWAS_sample: 50000
    REF_sample: 1000
    n_dataset: 200
  output: output/susierss_gtex

