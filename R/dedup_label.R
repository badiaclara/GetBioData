#' Deduplicate Biodiversity Data
#'
#' Identifies duplicates based on collector, number, and coordinates.
#'
#' @param df Combined dataframe from bind_biodata.
#' @return A dataframe with a new 'status_dedup' column.
#' @export
dedup_label <- function(df) {

  message("-> Iniciando deduplicação...")

  if (nrow(df) == 0) return(df)

  # 1. Normalização temporária para comparação
  # Criamos uma chave baseada em Coletor, Número e Localização
  df_clean <- df |>
    dplyr::mutate(
      tmp_coletor = stringr::str_to_lower(COLETOR),
      tmp_lat = round(LATITUDE, 3),
      tmp_lon = round(LONGITUDE, 3)
    )

  # 2. Identificação de Duplicatas
  # Marcamos como duplicado se tiver o mesmo Coletor, Número, Ano e Coordenadas
  df <- df_clean |>
    dplyr::group_by(tmp_coletor, NÚMERO_COLETOR, ANO, tmp_lat, tmp_lon) |>
    dplyr::mutate(
      status_dedup = ifelse(dplyr::row_number() == 1, "Original", "Duplicado")
    ) |>
    dplyr::ungroup() |>
    # Remove as colunas temporárias
    dplyr::select(-starts_with("tmp_"))

  n_dups <- sum(df$status_dedup == "Duplicado")
  message(paste0("   (ok) Processo concluído. ", n_dups, " duplicatas marcadas."))

  return(df)
}
