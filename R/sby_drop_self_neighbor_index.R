#' Remover o proprio ponto de uma matriz de vizinhos de forma robusta
#'
#' @details
#' A funcao implementa uma unidade interna do fluxo de balanceamento com contrato de entrada explicito e retorno controlado
#' A documentacao descreve a intencao operacional para apoiar manutencao, auditoria e revisao tecnica do pacote
#'
#' @param sby_neighbor_index Matriz de indices de vizinhos candidatos
#' @param sby_self_index Vetor com indices das proprias linhas
#' @param sby_desired_k Numero desejado de vizinhos apos a remocao
#'
#' @return Matriz de indices de vizinhos sem o proprio ponto
#' @noRd
sby_drop_self_neighbor_index <- function(sby_neighbor_index, sby_self_index, sby_desired_k){
  # Inicializa matriz de saida com dimensoes esperadas
  sby_out <- matrix(
    data = NA_integer_,
    nrow = nrow(sby_neighbor_index),
    ncol = sby_desired_k
  )

  # Processa cada linha de vizinhos removendo o proprio ponto
  for(i in seq_len(nrow(sby_neighbor_index))){
    # Verifica interrupcao periodicamente durante processamento das linhas
    if(i %% 1024L == 1L){

      # Executa ponto cooperativo de interrupcao
      sby_over_under_check_user_interrupt()
    }

    # Filtra candidatos validos diferentes do indice da propria linha
    sby_candidates <- sby_neighbor_index[i, ]
    sby_candidates <- sby_candidates[!is.na(sby_candidates) & sby_candidates != sby_self_index[[i]]]

    # Verifica se ha candidatos suficientes apos a remocao do proprio ponto
    if(length(sby_candidates) < sby_desired_k){

      # Aborta quando nao e possivel manter a quantidade desejada de vizinhos
      sby_over_under_abort(
        sby_message = "Nao foi possivel remover o proprio ponto mantendo vizinhos suficientes"
      )
    }

    # Preenche a linha de saida com os primeiros vizinhos validos
    sby_out[i, ] <- sby_candidates[seq_len(sby_desired_k)]
  }

  # Retorna matriz de vizinhos sem autorreferencias
  return(sby_out)
}
####
## Fim
#
