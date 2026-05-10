
#' Remover o proprio ponto de uma matriz de vizinhos de forma robusta
#' @noRd
sby_drop_self_neighbor_index <- function(sby_neighbor_index, sby_self_index, sby_desired_k) {
  sby_out <- matrix(NA_integer_, nrow = nrow(sby_neighbor_index), ncol = sby_desired_k)
  for (i in seq_len(nrow(sby_neighbor_index))) {
    if (i %% 1024L == 1L) {
      sby_over_under_check_user_interrupt()
    }
    sby_candidates <- sby_neighbor_index[i, ]
    sby_candidates <- sby_candidates[!is.na(sby_candidates) & sby_candidates != sby_self_index[[i]]]
    if (length(sby_candidates) < sby_desired_k) {
      sby_over_under_abort("Nao foi possivel remover o proprio ponto mantendo vizinhos suficientes")
    }
    sby_out[i, ] <- sby_candidates[seq_len(sby_desired_k)]
  }
  sby_out
}

####
## Fim
#
