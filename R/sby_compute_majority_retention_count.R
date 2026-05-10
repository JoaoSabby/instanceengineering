
#' Calcular quantidade retida da maioria
#' @noRd
sby_compute_majority_retention_count <- function(sby_target_factor, sby_under_ratio) {
  if (!is.numeric(sby_under_ratio) || length(sby_under_ratio) != 1L || is.na(sby_under_ratio) || sby_under_ratio <= 0 || sby_under_ratio > 1) {
    sby_over_under_abort("'sby_under_ratio' deve estar no intervalo (0, 1]")
  }

  sby_class_roles <- sby_get_binary_class_roles(sby_target_factor)
  sby_majority_count <- as.integer(sby_class_roles$sby_class_counts[sby_class_roles$sby_majority_label])
  sby_retained_count <- floor(sby_majority_count * sby_under_ratio)

  if (sby_retained_count < 1L) {
    sby_over_under_abort("'sby_under_ratio' reteve zero linhas")
  }

  sby_retained_count
}

####
## Fim
#
