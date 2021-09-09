Steps taken to extract UK Biobank genotype data:

1. Use the R script [get_pheno.R](get_pheno.R) obtain a list of sample
   ids. (The height and covariate data obtained from running this
   script are not used in our simulations.) This step creates a new
   CSV file, `height.csv`, containing the phenotype and covariate
   data.

2. Extract regions based on height GWAS result from Neale lab. Run R
   script
   [height.neale.top.filter.improve.R](height.neale.top.filter.improve.R).

3. Extract genotypes for each region. Run [submit.sh](submit.sh).
