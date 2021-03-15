#!/usr/bin/env dsc

%include modules/data
%include modules/simulate
%include modules/get_sumstats
%include modules/fit
%include modules/evaluate

DSC:
  define:
    method_susie: susie_suff, susie_suff_addz, susie_rss, susie_rss_addz, susie_rss_suff, susie_rss_suff_addz, susie_rss_lambda, susie_rss_lambda_addz
    method_susie_3steps: susie_suff_3steps, susie_suff_3steps_addz, susie_rss_3steps, susie_rss_3steps_addz, susie_rss_suff_3steps, susie_rss_suff_3steps_addz, susie_rss_lambda_3steps, susie_rss_lambda_3steps_addz
    method_caviar: caviar
    method_finemap: finemap
    method_finemapv4: finemapv4, finemapv4L4
  run:
    default: small_data * sim_gaussian * get_sumstats * ((method_susie, method_susie_3steps) * score_susie, method_caviar * score_caviar, method_finemapv4 * score_finemapv4, method_finemap * score_finemap)
    fmv4: small_data * sim_gaussian * get_sumstats * method_finemapv4 * score_finemapv4
    rss: small_data * sim_gaussian * get_sumstats * (susie_suff, susie_suff_addz, susie_rss, susie_rss_addz, susie_rss_suff, susie_rss_suff_addz) * score_susie
    init: small_data * sim_gaussian * get_sumstats * (susie_init, susie_suff_init, susie_suff_3steps) * score_susie
  exec_path: code
  global:
    data_file: data/ukb_genotypes_files.txt
    GWAS_sample: 50000
    REF_sample: 1000
    n_dataset: 200
    # unused
    prop_samples: 0.5
  output: output/susierss_ukb
