
#' Restaurar tipos numericos inferidos
#' @noRd
sby_restore_numeric_column_types <- function(sby_x_matrix, sby_type_info, sby_as_data_frame = TRUE){
  sby_x_matrix <- sby_over_under_as_numeric_matrix(sby_x_matrix)
  if(NCOL(sby_x_matrix) != nrow(sby_type_info)){
    sby_over_under_abort("Inconsistencia entre numero de colunas e sby_type_info")
  }

  for(j in seq_len(NCOL(sby_x_matrix))){
    if(j %% 64L == 1L){
      sby_over_under_check_user_interrupt()
    }
    sby_inferred_type <- sby_type_info$sby_inferred_type[[j]]
    if(identical(sby_inferred_type, "binary")){
      sby_x_matrix[, j] <- ifelse(sby_x_matrix[, j] >= 0.5, 1, 0)
    } else if(identical(sby_inferred_type, "integer")){
      sby_x_matrix[, j] <- round(sby_x_matrix[, j])
    }
  }

  if(!sby_as_data_frame){
    return(sby_x_matrix)
  }

  sby_out <- as.data.frame(sby_x_matrix, stringsAsFactors = FALSE)
  names(sby_out) <- sby_type_info$sby_column_name

  for(j in seq_len(NCOL(sby_x_matrix))){
    if(j %% 64L == 1L){
      sby_over_under_check_user_interrupt()
    }
    sby_inferred_type <- sby_type_info$sby_inferred_type[[j]]
    if(identical(sby_inferred_type, "binary") || identical(sby_inferred_type, "integer")){
      sby_out[[j]] <- as.integer(sby_out[[j]])
    } else {
      sby_out[[j]] <- as.numeric(sby_out[[j]])
    }
  }

  sby_out
}
