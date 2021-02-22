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

score_dap: dap_scores.R + R(sc = dap_scores_multiple($(posterior), $(meta)$true_coef))
    $total: sc$total
    $valid: sc$valid
    $size: sc$size
    $avgr2: sc$avgr2
    $top: sc$top
    $overlap: sc$overlap
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
    
score_caviar: caviar_scores.R + R(sc = caviar_scores_multiple($(posterior), $(meta)$true_coef))
    $total: sc$total
    $valid: sc$valid
    $size: sc$size
    $signal_pip: sc$signal_pip
    $pip: sc$pip

