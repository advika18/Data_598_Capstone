#' Calculates the Continuous Ranked Probability Score (CRPS)
#' 
#' @description 
#' The calc_crps() calculates the continuous ranked probability score of
#'   forecasted sample paths.
#'
#' @param actual_path Actual path vector the forecast paths are measured against.
#' @param sample_paths Forecasted sample path matrix.
#' 

calc_crps = function(actual_path, sample_paths) {
  
  # Compute accuracy term
  acc_diff <- t(apply(sample_paths, 1, function(x) x - actual_path))
  acc_norm <- apply(acc_diff, 1, function(x) norm(x, "2"))
  acc_term <- mean(acc_norm)
  
  # Compute dispersion term
  combinations <- expand.grid(rep(list(1:nrow(sample_paths)), 2))
  dis_diff <- t(apply(combinations, 1,
                      function(x) sample_paths[x[1], ] - sample_paths[x[2], ]))
  dis_norm <- apply(dis_diff, 1, function(x) norm(x, "2"))
  dis_term <- mean(dis_norm)
  
  # Compute CRPS
  crps = acc_term - 1/2 * dis_term
  
  return(crps)
  
}
