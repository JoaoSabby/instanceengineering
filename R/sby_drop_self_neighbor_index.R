
#' Remover o proprio ponto de uma matriz de vizinhos de forma robusta
#' @noRd
<<<<<<< HEAD
sby_drop_self_neighbor_index <- function(sby_neighbor_index, sby_self_index, sby_desired_k) {
  sby_out <- matrix(NA_integer_, nrow = nrow(sby_neighbor_index), ncol = sby_desired_k)
  for (i in seq_len(nrow(sby_neighbor_index))) {
    if (i %% 1024L == 1L) {
=======
# Executa instrucao do fluxo preservado
sby_drop_self_neighbor_index <- function(sby_neighbor_index, sby_self_index, sby_desired_k) {
  # Executa instrucao do fluxo preservado
  sby_out <- matrix(NA_integer_, nrow = nrow(sby_neighbor_index), ncol = sby_desired_k)
  # Executa instrucao do fluxo preservado
  for (i in seq_len(nrow(sby_neighbor_index))) {
    # Executa instrucao do fluxo preservado
    if (i %% 1024L == 1L) {
      # Executa instrucao do fluxo preservado
>>>>>>> origin/main
      sby_over_under_check_user_interrupt()
    # Executa instrucao do fluxo preservado
    }
    # Executa instrucao do fluxo preservado
    sby_candidates <- sby_neighbor_index[i, ]
    # Executa instrucao do fluxo preservado
    sby_candidates <- sby_candidates[!is.na(sby_candidates) & sby_candidates != sby_self_index[[i]]]
<<<<<<< HEAD
    if (length(sby_candidates) < sby_desired_k) {
=======
    # Executa instrucao do fluxo preservado
    if (length(sby_candidates) < sby_desired_k) {
      # Executa instrucao do fluxo preservado
>>>>>>> origin/main
      sby_over_under_abort("Nao foi possivel remover o proprio ponto mantendo vizinhos suficientes")
    # Executa instrucao do fluxo preservado
    }
    # Executa instrucao do fluxo preservado
    sby_out[i, ] <- sby_candidates[seq_len(sby_desired_k)]
  # Executa instrucao do fluxo preservado
  }
  # Executa instrucao do fluxo preservado
  sby_out
# Executa instrucao do fluxo preservado
}

####
## Fim
#
