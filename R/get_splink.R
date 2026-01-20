#' Download occurrence data from speciesLink
#'
#' @param species Character vector of species names
#' @param api_key speciesLink API key (default: Sys.getenv("SPLINK_API_KEY"))
#' @return Tibble of occurrence records standardized to 16 columns.
#' @export
get_splink <- function(species, api_key = Sys.getenv("SPLINK_API_KEY")) {
  if (api_key == "") return(dplyr::tibble())

  results <- lapply(species, function(spp) {
    cat("Downloading speciesLink data for:", spp, "...\n")
    url <- paste0("https://specieslink.net/ws/1.0/search?scientificname=",
                  utils::URLencode(spp), "&apikey=", api_key, "&limit=100000")

    resp <- tryCatch(httr::GET(url, httr::timeout(60)), error = function(e) NULL)
    if (is.null(resp) || httr::status_code(resp) != 200) return(dplyr::tibble())

    data <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"))
    if (is.null(data$features) || length(data$features) == 0) return(dplyr::tibble())

    df_props <- as.data.frame(data$features$properties)
    df_geom  <- as.data.frame(data$features$geometry)

    names(df_props) <- lower_names <- tolower(names(df_props))

    df_props$long_clean <- sapply(df_geom$coordinates, function(x) if(length(x) >= 1) as.numeric(x[1]) else NA)
    df_props$lat_clean  <- sapply(df_geom$coordinates, function(x) if(length(x) >= 2) as.numeric(x[2]) else NA)

    df_props |>
      dplyr::mutate(
        ESPÉCIE = if("scientificname" %in% lower_names) scientificname else spp,
        FAMÍLIA = if("family" %in% lower_names) family else NA_character_,
        GÊNERO  = if("genus" %in% lower_names) genus else NA_character_,
        LATITUDE = lat_clean, LONGITUDE = long_clean,
        COLETOR = if("recordedby" %in% lower_names) recordedby else NA_character_,
        DATA_COLETA = paste0(
          if("yearcollected" %in% lower_names) yearcollected else "", "-",
          if("monthcollected" %in% lower_names) monthcollected else "", "-",
          if("daycollected" %in% lower_names) daycollected else ""
        ),
        PAÍS = if("country" %in% lower_names) country else NA_character_,
        ESTADO = if("stateprovince" %in% lower_names) stateprovince else NA_character_,
        MUNICÍPIO = if("county" %in% lower_names) county else NA_character_,
        DETERMINADOR = if("identifiedby" %in% lower_names) identifiedby else NA_character_,
        DATA_DETERMINACAO = if("yearidentified" %in% lower_names) as.character(yearidentified) else NA_character_,
        CATÁLOGO = if("catalognumber" %in% lower_names) as.character(catalognumber) else NA_character_,
        INSTITUIÇÃO = if("institutioncode" %in% lower_names) institutioncode else NA_character_,
        OCCURRENCE_ID = if("occurrenceid" %in% lower_names) as.character(occurrenceid) else CATÁLOGO,
        FONTE = "SPLink"
      ) |>
      dplyr::select(ESPÉCIE, FAMÍLIA, GÊNERO, LATITUDE, LONGITUDE, COLETOR,
                    DATA_COLETA, PAÍS, ESTADO, MUNICÍPIO, DETERMINADOR,
                    DATA_DETERMINACAO, CATÁLOGO, INSTITUIÇÃO, OCCURRENCE_ID, FONTE)
  })
  dplyr::bind_rows(results)
}
