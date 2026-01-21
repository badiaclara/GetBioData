#' Remove marked duplicates
#'
#' This function filters the dataframe to keep only 'Original' records
#' identified by dedup_label and removes the status column.
#'
#' @param df Dataframe returned by dedup_label.
#' @return A cleaned tibble containing only unique records.
#' @export
kill_dedup <- function(df) {

  # 1. Verificação de segurança: a coluna status_dedup existe?
  if (!"status_dedup" %in% names(df)) {
    warning("A coluna 'status_dedup' não foi encontrada. Certifique-se de rodar dedup_label() antes.")
    return(df)
  }

  message("-> Removendo duplicatas da base de dados...")

  n_antes <- nrow(df)

  # 2. Filtragem e limpeza
  df_limpo <- df |>
    dplyr::filter(status_dedup == "Original") |>
    dplyr::select(-status_dedup)

  n_depois <- nrow(df_limpo)
  n_removidos <- n_antes - n_depois

  message(paste0("   (ok) Limpeza concluída. ", n_removidos, " registros removidos."))
  message(paste0("   Total de registros únicos restantes: ", n_depois))

  return(tibble::as_tibble(df_limpo))
}
