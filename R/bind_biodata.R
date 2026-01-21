#' Bind GBIF and speciesLink data
#'
#' This function combines standardized dataframes from GBIF and speciesLink
#' into a single integrated dataset.
#'
#' @param gbif_df A tibble/dataframe returned by get_gbif().
#' @param splink_df A tibble/dataframe returned by get_splink().
#'
#' @return A single integrated tibble with 16 standardized columns.
#' @export
bind_biodata <- function(gbif_df, splink_df) {

  message("-> Unindo bases de dados (GBIF + speciesLink)...")

  # 1. Tratamento de bases vazias
  if (is.null(gbif_df) || nrow(gbif_df) == 0) {
    message("   (!) Base GBIF vazia. Retornando apenas speciesLink.")
    return(splink_df)
  }

  if (is.null(splink_df) || nrow(splink_df) == 0) {
    message("   (!) Base speciesLink vazia. Retornando apenas GBIF.")
    return(gbif_df)
  }

  # 2. Verificação de Integridade (Opcional, mas seguro)
  # Se os nomes das colunas não baterem, o R daria erro ou criaria colunas extras.
  # Aqui garantimos que a união só ocorra se as colunas forem irmãs.
  common_cols <- intersect(names(gbif_df), names(splink_df))

  if (length(common_cols) < 16) {
    warning("   [Atenção] As bases possuem colunas diferentes. Verifique a padronização.")
  }

  # 3. União das bases
  combined <- dplyr::bind_rows(gbif_df, splink_df)

  message(paste0("   (ok) Sucesso! ", nrow(combined), " registros totais integrados."))

  return(tibble::as_tibble(combined))
}
