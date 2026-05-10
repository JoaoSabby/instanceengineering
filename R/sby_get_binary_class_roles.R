

#' Identificar classes minoritaria e majoritaria
#' @noRd
sby_get_binary_class_roles <- function(sby_target_factor) {
  sby_class_counts <- table(sby_target_factor)
  if (length(sby_class_counts) != 2L) {
    sby_over_under_abort("'sby_target_vector' deve ser binario")
  }
  if (sby_class_counts[[1L]] == sby_class_counts[[2L]]) {
    sby_over_under_abort("As rotinas de sampling requerem classes desbalanceadas")
  }

  list(
    sby_class_counts = sby_class_counts,
    sby_minority_label = names(sby_class_counts)[which.min(sby_class_counts)],
    sby_majority_label = names(sby_class_counts)[which.max(sby_class_counts)]
  )
}

####
## Fim
#
