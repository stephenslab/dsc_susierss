#!/usr/bin/env dsc

%include modules/data
%include modules/simulate
%include modules/get_sumstats
%include modules/fit
%include modules/evaluate

DSC:
  define:
    method_susie: susie_rss, susie_rss_Ltrue
    method_caviar: caviar
    method_finemap: finemap
    method_finemapv4: finemapv4, finemapv4L4
  run:
    default: small_data * sim_gaussian * get_sumstats * ((adjustld, adjustld_addz) * (method_susie * score_susie, finemap * score_finemap, method_finemapv4 * score_finemapv4, caviar * score_caviar))
    summaryLD: small_data * summarize_ld
    dap: small_data * sim_gaussian * get_sumstats * adjustld * (dap_z * score_dap)
    susierssprior: small_data * sim_gaussian * get_sumstats * adjustld * method_susie * score_susie_prior
    pve: small_data * sim_gaussian_pve * get_sumstats * adjustld_lamb0 * ((susie_rss_n, susie_rss_Ltrue_n) * score_susie, method_finemapv4 * score_finemapv4, caviar * score_caviar)
    pve_n: small_data * sim_gaussian_pve_n * get_sumstats_n * adjustld_n_lamb0 * ((susie_rss_n, susie_rss_Ltrue_n) * score_susie, (finemapv4_n, finemapv4L4_n) * score_finemapv4, caviar * score_caviar, dap_z * score_dap)
  exec_path: code
  global:
    data_file: data/ukb_genotypes_files.txt
    GWAS_sample: 50000
    # REF_sample: 500
    REF_sample: 1000
    n_dataset: 200
    # unused
    prop_samples: 0.5
  output: output/susierss_ukb
