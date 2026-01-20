#' Download occurrence data from GBIF
#'
#' @param species Character vector with scientific names.
#' @param limit Maximum records per species (default = 100000).
#' @return Tibble of occurrence records standardized to 16 columns.
#' @export
get_gbif <- function(species, limit = 100000) {
  results <- lapply(species, function(spp) {
    cat("Downloading GBIF data for:", spp, "...\n")
    gbif_raw <- rgbif::occ_search(scientificName = spp, limit = limit,
                                  hasCoordinate = TRUE, hasGeospatialIssue = FALSE)
    df <- gbif_raw$data
    if (is.null(df) || nrow(df) == 0) return(dplyr::tibble())

    cols <- names(df)

    df |>
      dplyr::mutate(
        ESPÉCIE = scientificName,
        FAMÍLIA = if("family" %in% cols) family else NA_character_,
        GÊNERO  = if("genus" %in% cols) genus else NA_character_,
        LATITUDE = as.numeric(decimalLatitude),
        LONGITUDE = as.numeric(decimalLongitude),
        COLETOR = if("recordedBy" %in% cols) recordedBy else NA_character_,
        DATA_COLETA = as.character(eventDate),
        PAÍS = if("countryCode" %in% cols) countryCode else NA_character_,
        ESTADO = if("stateProvince" %in% cols) stateProvince else NA_character_,
        MUNICÍPIO = if("municipality" %in% cols) municipality else NA_character_,
        DETERMINADOR = if("identifiedBy" %in% cols) identifiedBy else NA_character_,
        DATA_DETERMINACAO = if("dateIdentified" %in% cols) as.character(dateIdentified) else NA_character_,
        CATÁLOGO = if("catalogNumber" %in% cols) as.character(catalogNumber) else NA_character_,
        INSTITUIÇÃO = if("institutionCode" %in% cols) institutionCode else NA_character_,
        OCCURRENCE_ID = if("occurrenceID" %in% cols) as.character(occurrenceID) else as.character(key),
        FONTE = "GBIF"
      ) |>
      dplyr::select(ESPÉCIE, FAMÍLIA, GÊNERO, LATITUDE, LONGITUDE, COLETOR,
                    DATA_COLETA, PAÍS, ESTADO, MUNICÍPIO, DETERMINADOR,
                    DATA_DETERMINACAO, CATÁLOGO, INSTITUIÇÃO, OCCURRENCE_ID, FONTE)
  })
  dplyr::bind_rows(results)
}
