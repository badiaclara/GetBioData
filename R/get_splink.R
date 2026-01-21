#' Download occurrence data from speciesLink
#'
#' @param species Character vector of species names
#' @param api_key speciesLink API key (default: Sys.getenv("SPLINK_API_KEY"))
#' @return Tibble of occurrence records standardized to 16 columns.
#' @export
get_splink <- function(species, api_key = Sys.getenv("SPLINK_API_KEY")) {
  if (api_key == "") {
    stop("\n[GetBioData] ERRO: Chave speciesLink não encontrada!\n",
         "1. Pegue sua chave em: https://api.splink.org.br/\n",
         "2. Use: Sys.setenv(SPLINK_API_KEY = 'SUA_CHAVE')", call. = FALSE)
  }

  message(paste0("-> Consultando speciesLink para: ", species, "..."))
  base_url <- "https://api.splink.org.br/records/format/json/"
  full_url <- paste0(base_url, "scientificname/", gsub(" ", "%20", species), "/apikey/", api_key)

  tryCatch({
    res <- httr::GET(full_url, httr::timeout(30))
    if (httr::status_code(res) == 401) stop("Chave API inválida.")

    cont <- httr::content(res, as = "text", encoding = "UTF-8")
    data <- jsonlite::fromJSON(cont, flatten = TRUE)

    if (is.null(data$records) || length(data$records) == 0) {
      message("   (!) Nenhum registro no speciesLink.")
      return(data.frame())
    }

    df <- data$records
    # Padronização das 16 colunas
    df_clean <- df %>%
      dplyr::mutate(
        ESPÉCIE = as.character(scientificName),
        FAMÍLIA = if("family" %in% names(.)) as.character(family) else NA,
        GÊNERO = if("genus" %in% names(.)) as.character(genus) else NA,
        LATITUDE = as.numeric(decimalLatitude),
        LONGITUDE = as.numeric(decimalLongitude),
        PAÍS = if("country" %in% names(.)) as.character(country) else NA,
        ESTADO = if("stateProvince" %in% names(.)) as.character(stateProvince) else NA,
        MUNICÍPIO = if("county" %in% names(.)) as.character(county) else NA,
        COLETOR = if("recordedBy" %in% names(.)) as.character(recordedBy) else NA,
        DATA_COLETA = paste(year, month, day, sep="-"),
        DETERMINADOR = if("identifiedBy" %in% names(.)) as.character(identifiedBy) else NA,
        DATA_DETERMINACAO = if("dateIdentified" %in% names(.)) as.character(dateIdentified) else NA,
        CATÁLOGO = if("catalogNumber" %in% names(.)) as.character(catalogNumber) else NA,
        INSTITUIÇÃO = if("institutionCode" %in% names(.)) as.character(institutionCode) else NA,
        OCCURRENCE_ID = if("occurrenceID" %in% names(.)) as.character(occurrenceID) else NA,
        FONTE = "speciesLink"
      ) %>%
      dplyr::select(ESPÉCIE, FAMÍLIA, GÊNERO, LATITUDE, LONGITUDE, PAÍS, ESTADO, MUNICÍPIO,
                    COLETOR, DATA_COLETA, DETERMINADOR, DATA_DETERMINACAO, CATÁLOGO,
                    INSTITUIÇÃO, OCCURRENCE_ID, FONTE)

    message(paste0("   (ok) ", nrow(df_clean), " registros do speciesLink."))
    return(df_clean)
  }, error = function(e) {
    message(paste0("   (X) Erro no speciesLink: ", e$message))
    return(data.frame())
  })
}
