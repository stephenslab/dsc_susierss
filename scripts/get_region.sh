#!/bin/bash

regionchr="$1"
regionfrom="$2"
regionto="$3"
regionname="$4"

module load gcc/6.2.0 R/3.5.0

zcat /gpfs/data/stephens-lab/finemap-uk-biobank/data/raw/height.csv.gz | awk 'BEGIN{FS=","} {print $1 OFS $1}' | tail -n +2 > ../output/height.id.txt

zcat /gpfs/data/pierce-lab/uk-biobank-genotypes/ukb_mfi_chr${regionchr}_v3.txt.gz | awk '($8 < 0.9){print $2}' > ../output/ukb_mfi_chr${regionchr}_v3_exclude_id.txt

plink2 --sample /gpfs/data/xhe-lab/uk-biobank/data/genotypes/ukb27386_imp_v3_s487324.sample \
--bgen /gpfs/data/pierce-lab/uk-biobank-genotypes/ukb_imp_chr${regionchr}_v3.bgen \
--chr ${regionchr} --from-bp ${regionfrom} --to-bp ${regionto} \
--keep ../output/height.id.txt \
--exclude ../output/ukb_mfi_chr${regionchr}_v3_exclude_id.txt \
--maf 0.01 minor \
--snps-only --max-alleles 2 --rm-dup exclude-all --make-pgen --threads 8 --memory 38000 \
--out ../output/height.chr${regionchr}.${regionname} > ../output/height.chr${regionchr}.${regionname}.plink2.log

plink2 --pfile ../output/height.chr${regionchr}.${regionname} --threads 8 --export A --out ../output/height.chr${regionchr}.${regionname}

gzip ../output/height.chr${regionchr}.${regionname}.raw

plink2 --pfile ../output/height.chr${regionchr}.${regionname} --freq --out ../output/height.chr${regionchr}.${regionname}

plink2 --pfile ../output/height.chr${regionchr}.${regionname} --recode bgen-1.2 --out ../output/height.chr${regionchr}.${regionname}

