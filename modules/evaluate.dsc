# Modules to evaluate various methods
# for finemapping

# Module input
# ============
# $fit: see fit.dsc
# $result: see fit.dsc

# Module output
# =============
# ? an object or plot for diagnosis

score_susie: susie_scores.R + R(sc = susie_scores_multiple($(fitted), $(meta)$true_coef))
    $total: sc$total
    $valid: sc$valid
    $size: sc$size
    $purity: sc$purity
    $avgr2: sc$avgr2
    $top: sc$top
    $objective: sc$objective
    $converged: sc$converged
    $niter: sc$niter
    $overlap: sc$overlap
    $signal_pip: sc$signal_pip
    $pip: sc$pip
    $sigma2: sc$sigma2
    $lamb: sc$lambda

score_dap: dap_scores.R + R(ld = as.matrix(data.table::fread($(ldinfo)$ldfile));
                            sc = dap_scores_multiple($(posterior), $(meta)$true_coef, ld))
    $total_cluster: sc$total_cluster
    $valid_cluster: sc$valid_cluster
    $size_cluster: sc$size_cluster
    $avgr2_cluster: sc$avgr2_cluster
    $purity_cluster: sc$purity_cluster
    $total_cs: sc$total_cs
    $valid_cs: sc$valid_cs
    $size_cs: sc$size_cs
    $avgr2_cs: sc$avgr2_cs
    $purity_cs: sc$purity_cs
    $signal_pip: sc$signal_pip
    $pip: sc$pip

score_finemap: finemap_scores.R + R(sc = finemap_scores_multiple($(posterior), $(meta)$true_coef))
    $total: sc$total
    $valid: sc$valid
    $size: sc$size
    $signal_pip: sc$signal_pip
    $pip: sc$pip
    
score_finemapv3: finemap_scores.R + R(sc = finemap_v1.3.1_scores_multiple($(posterior), $(meta)$true_coef))
    $total: sc$total
    $valid: sc$valid
    $size: sc$size
    $signal_pip: sc$signal_pip
    $pip: sc$pip

score_finemapv4: finemap_scores.R + R(sc = finemap_v1.4_scores_multiple($(posterior), $(meta)$true_coef))
    $total: sc$total
    $valid: sc$valid
    $size: sc$size
    $purity: sc$purity
    $avgr2: sc$avgr2
    $total_pure: sc$total_pure
    $valid_pure: sc$valid_pure
    $size_pure: sc$size_pure
    $purity_pure: sc$purity_pure
    $avgr2_pure: sc$avgr2_pure
    $signal_pip: sc$signal_pip
    $pip: sc$pip
    
score_caviar: caviar_scores.R + R(sc = caviar_scores_multiple($(posterior), $(meta)$true_coef))
    $total: sc$total
    $valid: sc$valid
    $size: sc$size
    $signal_pip: sc$signal_pip
    $pip: sc$pip


    
