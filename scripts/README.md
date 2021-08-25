1. Prepare individual id. Run R script [get_pheno.R](get_pheno.R) to prepare a CSV file containing the phenotype and covariate data from the UK Biobank source files. For height, this step creates a new CSV file, height.csv, containing the phenotype and covariate data.

2. Extract regions based on height GWAS result from Neale lab. Run R script [height.neale.top.filter.improve.R](height.neale.top.filter.improve.R).

3. Extract genotype for each region. Run [submit.sh](submit.sh).

