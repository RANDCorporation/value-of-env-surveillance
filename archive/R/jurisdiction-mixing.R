#
#
#
# # Approaches for inter-jurisdiction mixing matrices
#
# # 1 - Decomposed mixing matrix (similar to current PBM's approach, but without NPI weights)
#
# # First, we have a
#
# names.jurisdictions <- c("A", "B", "C", "D", "E")
# n.jurisdictions <- length(names.jurisdictions)
#
# names.modes <- c("Home", "Work", "Other")
# n.modes <- length(names.modes)
# # Following the same notation, k is the normalized contact rates across modes, by jurisdictions
# # k can be thought of the intensity of contacts across these mixing modes for each jurisdiction
#
# # For generality,
# k.mat <- matrix(data = c(0,0.5,0.5, 0.7,0.2,0.1), byrow = T, nrow = n.jurisdictions, ncol = n.modes, dimnames = list(names.jurisdictions, names.modes))
#
# # stop if rows are not normalized to 1
# stopifnot(!all((rowSums(k.mat) - 1)^2 > 1e-10))
#
# # now, for each mode, we have a mode-specific mixing matrix:
# home_mixing <-  diag(nrow = n.jurisdictions, ncol = n.jurisdictions, names = names.jurisdictions)
#
# # Work mixing needs to be an input from the spreadsheet:
# perc_work_home_county <- 0.9
#
# work_mixing <- diag(x = perc_work_home_county, nrow = n.jurisdictions, ncol = n.jurisdictions)
#
# work_mixing[work_mixing != perc_work_home_county] <- (1-perc_work_home_county)/(n.jurisdictions-1)
#
# rownames(work_mixing) <- names.jurisdictions
# colnames(work_mixing) <- names.jurisdictions
#
# # Other mixing:
#
# perc_other_home_county <- 0.7
#
# other_mixing <- diag(x = perc_other_home_county, nrow = n.jurisdictions, ncol = n.jurisdictions)
#
# other_mixing[other_mixing != perc_other_home_county] <- (1-perc_other_home_county)/(n.jurisdictions-1)
#
# rownames(other_mixing) <- names.jurisdictions
# colnames(other_mixing) <- names.jurisdictions
#
# # Concatenate all mixing mdoes into one array (order matters!):
# M <- array(c(home_mixing, work_mixing, other_mixing),dim = c(n.jurisdictions, n.jurisdictions, n.modes), dimnames = list(names.jurisdictions, names.jurisdictions, names.modes))
#
# # Overall mixing matrix is the sum of element-wise multiplications:
# mixing_matrix <- matrix(data = 0, nrow = n.jurisdictions, ncol = n.jurisdictions,dimnames = list(names.jurisdictions, names.jurisdictions))
#
# for(m in names.modes) {
#
#   mixing_matrix <- mixing_matrix + k.mat[,m] * M[,,m]
#
# }
#
# mixing_matrix
