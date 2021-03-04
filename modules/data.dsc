# Modules to provide data
# Real or simulated

full_data: sim_utils.R + data.R
  tag: "full"
  dataset: Shell{head -${n_dataset} ${data_file}}
  subset: NULL
  maf_thresh: 0
  GWASsample: ${GWAS_sample}
  REFsample: ${REF_sample}
  in_sample_id_file: file(.txt)
  snps_id_file: file(.txt)
  ld_sample_file: file(sample.ld)
  ld_refin_file: file(refin.ld)
  ld_refout_file: file(refout.ld)
  $X_sample: X.sample
  $N_sample: nrow(X.sample)
  $N_ref: nrow(X.ref.in)
  $maf: list(in_sample=maf.sample, refin_sample=maf.ref.in, refout_sample=maf.ref.out)
  $ld: list(in_sample=ld_sample_file, refin_sample=ld_refin_file, refout_sample=ld_refout_file)
  $r_refin_2dist: r.refin.2dist
  $r_refout_2dist: r.refout.2dist
  $r_refin_Mdist: r.refin.Mdist
  $r_refout_Mdist: r.refout.Mdist
  $seed: seed
  $X_file: dataset
  $sample_file: in_sample_id_file
  $snp_file: snps_id_file

lite_data(full_data):
  tag: "2k"
  subset: 2000

small_data(full_data):
  tag: "1k"
  maf_thresh: 0
  subset: 1000

tiny_data(full_data):
  tag: "300"
  maf_thresh: 0
  subset: 300

summarize_ld: lib_regression_simulator.py + \
                regression_simulator.py + \
                Python(res = summarize_LD(X, ld_file['in_sample'], ld_plot))
  X: $X_sample
  ld_file: $ld
  $ld_plot: file(png)
  $top_idx: res

full_data_gtex: sim_utils.R + data_gtex.R
  tag: "full"
  dataset: Shell{head -250 ${data_file}}
  subset: NULL
  subsample: ${prop_samples}
  ld_sample_file: file(sample.ld)
  ld_out_file: file(out.ld)
  maf_thresh: 0.05
  $X_sample: X.sample
  $X_out: X.out
  $Y: data$Y
  $N_sample: nrow(X.sample)
  $N_ref: nrow(X.out)
  $maf: list(in_sample=maf.sample, out_sample=maf.out)
  $meta: data$meta
  $ld: list(in_sample=ld_sample_file, out_sample=ld_out_file)
  $r_refin_2dist: NA
  $r_refout_2dist: r.out.2dist
  $r_refin_Mdist: NA
  $r_refout_Mdist: r.out.Mdist
  $X_file: ''
  $sample_file: ''
  $snp_file: ''

small_data_gtex(full_data_gtex):
  tag: "1k"
  subset: 1000
  