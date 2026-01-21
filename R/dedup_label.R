#' Deduplicate Biodiversity Data
#'
#' Identifies duplicates based on collector, date, and coordinates.
#' It is flexible regarding column names to avoid errors.
#'
#' @param df Combined dataframe from bind_biodata.
#' @return A dataframe with a new 'status_dedup' column.
#' @export
dedup_label <- function(df) {

  message("-> Iniciando deduplicação...")

  if (nrow(df) == 0) return(df)

  # 1. Normalização e Criação de Chaves Temporárias
  # Usamos as colunas que confirmamos que existem no seu names(dados_unidos)
  df_prep <- df |>
    dplyr::mutate(
      # Normaliza coletor para evitar erro por maiúsculas/minúsculas
      tmp_coletor = stringr::str_to_lower(as.character(COLETOR)),
      # Arredonda coordenadas para captar registros com precisão levemente diferente
      tmp_lat = round(as.numeric(LATITUDE), 3),
      tmp_lon = round(as.numeric(LONGITUDE), 3)
    )

  # 2. Lógica de Data (Usa ANO se existir, senão usa DATA_COLETA)
  if ("ANO" %in% names(df_prep)) {
    df_prep$tmp_data <- as.character(df_prep$ANO)
  } else if ("DATA_COLETA" %in% names(df_prep)) {
    df_prep$tmp_data <- as.character(df_prep$DATA_COLETA)
  } else {
    df_prep$tmp_data <- "Sem Data"
  }

  # 3. Lógica de Identificador (Usa NÚMERO_COLETOR se existir, senão usa CATÁLOGO)
  if ("NÚMERO_COLETOR" %in% names(df_prep)) {
    df_prep$tmp_id <- as.character(df_prep$NÚMERO_COLETOR)
  } else if ("CATÁLOGO" %in% names(df_prep)) {
    df_prep$tmp_id <- as.character(df_prep$CATÁLOGO)
  } else {
    df_prep$tmp_id <- "Sem ID"
  }

  # 4. Processo de Deduplicação
  # Agrupamos pelas chaves temporárias para marcar o que é repetido
  df_final <- df_prep |>
    dplyr::group_by(tmp_coletor, tmp_data, tmp_id, tmp_lat, tmp_lon) |>
    dplyr::mutate(
      status_dedup = ifelse(dplyr::row_number() == 1, "Original", "Duplicado")
    ) |>
    dplyr::ungroup() |>
    # Remove as colunas auxiliares
    dplyr::select(-starts_with("tmp_"))

  n_dups <- sum(df_final$status_dedup == "Duplicado", na.rm = TRUE)
  message(paste0("   (ok) Processo concluído. ", n_dups, " duplicatas marcadas."))

  return(df_final)
}
