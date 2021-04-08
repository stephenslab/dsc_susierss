#!/bin/bash

#SBATCH --time=24:00:00
#SBATCH --partition=mstephens
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --mem=40G

# cd ~/GitHub/dsc_susierss

# Rscript results/susierss_ukb_20210324_REF500_pve005/query.R

# Rscript pip_extract.R

# Rscript ROC.R

Rscript pip_cali.R

