#!/bin/bash

#PBS -N UKB_region
#PBS -S /bin/bash
#PBS -l mem=20gb
#PBS -l walltime=10:00:00 
#PBS -l nodes=1:ppn=10
#PBS -t 0-199

# For reproducibility, add version numbers to "module load" calls.
module load gcc/6.2.0 R/3.5.0

cd /gpfs/data/stephens-lab/finemap-uk-biobank/data/raw/UKB_height_top/code/

linenum=$((${PBS_ARRAYID}+2))
echo ${linenum}
line=$(cat ../output/height.top2000.filter.txt | head -n ${linenum} | tail -n 1)
echo ${line}
regionchr=$(echo ${line} | awk '{print $1;}')
regionfrom=$(echo ${line} | awk '{print $2;}')
regionto=$(echo ${line} | awk '{print $3;}')
regionname=$(echo ${line} | awk '{print $4;}')

echo ${regionchr}
echo ${regionname}
./get_region.sh ${regionchr} ${regionfrom} ${regionto} ${regionname}

