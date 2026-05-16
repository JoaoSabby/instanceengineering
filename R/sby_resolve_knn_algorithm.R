#' Resolver algoritmo KNN automatico quando solicitado
#'
#' @details
#' A funcao implementa uma unidade interna do fluxo de balanceamento com contrato de entrada explicito e retorno controlado.
#' A resolucao considera o engine efetivo para que `auto` produza algoritmos compativeis com FNN ou BiocNeighbors.
#'
#' @param sby_knn_algorithm Algoritmo KNN informado pelo chamador
#' @param sby_predictor_column_count Quantidade de colunas preditoras
#' @param sby_knn_engine Engine KNN resolvido
#'
#' @return Nome do algoritmo KNN resolvido
#' @noRd
sby_resolve_knn_algorithm <- function(sby_knn_algorithm, sby_predictor_column_count, sby_knn_engine){
  
  # Retorna algoritmo explicito quando modo automatico nao foi solicitado
  if(!identical(
    x = sby_knn_algorithm,
    y = "auto"
  )){

    # Mantem a escolha explicita do chamador
    return(sby_knn_algorithm)
  }

  # RcppHNSW gerencia seu algoritmo internamente
  if(identical(
    x = sby_knn_engine,
    y = "RcppHNSW"
  )){

    # Informa ao usuario que o engine selecionado gerencia o algoritmo internamente
    sby_adanear_inform(
      sby_message = paste0(
        "KNN automático: sby_knn_algorithm = \"auto\" com sby_knn_engine = \"RcppHNSW\". ",
        "Justificativa: RcppHNSW controla internamente a estratégia HNSW, então o pacote ",
        "mantém o algoritmo como auto porque algoritmos externos não são aplicados nessa rota."
      )
    )

    # Mantem marcador automatico porque o engine ignora algoritmos externos
    return("auto")
  }

  # Seleciona algoritmo FNN por dimensionalidade quando o modo automatico e usado
  if(identical(
    x = sby_knn_engine,
    y = "FNN"
  )){

    # Usa busca bruta para dimensionalidade mais alta
    if(sby_predictor_column_count > 15L){

      # Informa ao usuario a selecao automatica e a justificativa da decisao
      sby_adanear_inform(
        sby_message = paste0(
          "KNN automático: sby_knn_algorithm = \"brute\" com sby_knn_engine = \"FNN\". ",
          "Justificativa: os dados têm ", sby_predictor_column_count,
          " colunas preditoras, acima do limite de 15; nessa dimensionalidade, ",
          "a busca bruta tende a ser mais estável que estruturas de árvore."
        )
      )

      return("brute")
    }

    # Informa ao usuario a selecao automatica e a justificativa da decisao
    sby_adanear_inform(
      sby_message = paste0(
        "KNN automático: sby_knn_algorithm = \"kd_tree\" com sby_knn_engine = \"FNN\". ",
        "Justificativa: os dados têm ", sby_predictor_column_count,
        " colunas preditoras, até o limite de 15; nessa dimensionalidade, ",
        "kd_tree costuma acelerar consultas exatas de vizinhos."
      )
    )

    # Usa kd-tree para dimensionalidade mais baixa
    return("kd_tree")
  }

  # Seleciona algoritmo BiocNeighbors por dimensionalidade quando o modo automatico e usado
  if(sby_predictor_column_count > 15L){

    # Informa ao usuario a selecao automatica e a justificativa da decisao
    sby_adanear_inform(
      sby_message = paste0(
        "KNN automático: sby_knn_algorithm = \"Exhaustive\" com sby_knn_engine = \"BiocNeighbors\". ",
        "Justificativa: os dados têm ", sby_predictor_column_count,
        " colunas preditoras, acima do limite de 15; a busca exaustiva evita perda ",
        "de eficiência das árvores em alta dimensionalidade e mantém resultado exato."
      )
    )

    return("Exhaustive")
  }

  # Informa ao usuario a selecao automatica e a justificativa da decisao
  sby_adanear_inform(
    sby_message = paste0(
      "KNN automático: sby_knn_algorithm = \"Kmknn\" com sby_knn_engine = \"BiocNeighbors\". ",
      "Justificativa: os dados têm ", sby_predictor_column_count,
      " colunas preditoras, até o limite de 15; Kmknn é o caminho exato padrão ",
      "do BiocNeighbors para baixa dimensionalidade."
    )
  )

  # Usa Kmknn como caminho exato padrao em menor dimensionalidade
  return("Kmknn")
}
####
## Fim
#
