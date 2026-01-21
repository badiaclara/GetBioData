#' Bind and Harmonize Biodiversity Data
#'
#' @param data_list List of dataframes (e.g., from GBIF and SPLink)
#' @return A unified and standardized tibble.
#' @export
bind_biodata <- function(data_list) {
  data_list <- data_list[sapply(data_list, function(x) !is.null(x) && nrow(x) > 0)]
  if (length(data_list) == 0) return(dplyr::tibble())

  template <- dplyr::tibble(
    OCCURRENCE_ID = character(), CATÁLOGO = character(), ESPÉCIE = character(),
    FAMÍLIA = character(), GÊNERO = character(), INSTITUIÇÃO = character(),
    LATITUDE = numeric(), LONGITUDE = numeric(), DATA_COLETA = character(),
    COLETOR = character(), PAÍS = character(), ESTADO = character(),
    MUNICÍPIO = character(), DETERMINADOR = character(),
    DATA_DETERMINACAO = character(), FONTE = character()
  )

  padronizado <- lapply(data_list, function(df) {
    df <- df |> dplyr::mutate(dplyr::across(dplyr::where(is.logical), as.character))
    dplyr::bind_rows(template, df)
  }) |>
    dplyr::bind_rows() |>
    dplyr::mutate(
      LATITUDE = as.numeric(LATITUDE),
      LONGITUDE = as.numeric(LONGITUDE)
    )

  return(padronizado)
}
