#' Download occurrence data from speciesLink
#'
#' @param species Character. Scientific name (e.g., "Araucaria angustifolia").
#' @param limit Integer. Maximum number of records to download (default 5000).
#' @param api_key Character. Your speciesLink API key. If NULL, will use Sys.getenv("SPLINK_API_KEY").
#'
#' @return A tibble with exactly 16 standardized columns.
#' @export
get_splink <- function(species, limit = 5000, api_key = NULL) {

  if (is.null(api_key)) {
    api_key <- Sys.getenv("SPLINK_API_KEY")
    if (api_key == "") stop("Chave API não encontrada.")
  }

  message(paste0("-> Consultando speciesLink: ", species, "..."))

  species_url <- utils::URLencode(species)
  url_splink <- paste0("https://specieslink.net/ws/1.0/search?scientificname=",
                       species_url, "&apikey=", api_key, "&limit=", limit)

  tryCatch({
    response <- httr::GET(url_splink,
                          httr::config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE),
                          httr::timeout(120))

    cont <- httr::content(response, "text", encoding = "UTF-8")
    data <- jsonlite::fromJSON(cont)

    if (is.null(data$features) || length(data$features) == 0) return(data.frame())

    df_props <- as.data.frame(data$features$properties)
    df_geom  <- as.data.frame(data$features$geometry)

    # Coordenadas
    lat_vec <- sapply(df_geom$coordinates, function(x) if(length(x)==2) as.numeric(x[2]) else NA_real_)
    lon_vec <- sapply(df_geom$coordinates, function(x) if(length(x)==2) as.numeric(x[1]) else NA_real_)

    # Criando o DF com nomes EXATOS para bater com o GBIF
    df_final <- data.frame(
      ESPÉCIE = as.character(species),
      FAMÍLIA = if("family" %in% names(df_props)) as.character(df_props$family) else NA_character_,
      GÊNERO = if("genus" %in% names(df_props)) as.character(df_props$genus) else NA_character_,
      LATITUDE = lat_vec,
      LONGITUDE = lon_vec,
      PAÍS = if("country" %in% names(df_props)) as.character(df_props$country) else NA_character_,
      ESTADO = if("stateprovince" %in% names(df_props)) as.character(df_props$stateprovince) else NA_character_,
      MUNICÍPIO = if("county" %in% names(df_props)) as.character(df_props$county) else NA_character_,
      COLETOR = if("recordedby" %in% names(df_props)) as.character(df_props$recordedby) else NA_character_,
      DATA_COLETA = paste(
        if("yearcollected" %in% names(df_props)) df_props$yearcollected else "NA",
        if("monthcollected" %in% names(df_props)) df_props$monthcollected else "01",
        if("daycollected" %in% names(df_props)) df_props$daycollected else "01",
        sep="-"
      ),
      DETERMINADOR = if("identifiedby" %in% names(df_props)) as.character(df_props$identifiedby) else NA_character_,
      DATA_DETERMINACAO = if("yearidentified" %in% names(df_props)) as.character(df_props$yearidentified) else NA_character_,
      CATÁLOGO = if("catalognumber" %in% names(df_props)) as.character(df_props$catalognumber) else NA_character_,
      INSTITUIÇÃO = if("institutioncode" %in% names(df_props)) as.character(df_props$institutioncode) else NA_character_,
      OCCURRENCE_ID = if("occurrenceid" %in% names(df_props)) as.character(df_props$occurrenceid) else NA_character_,
      FONTE = "speciesLink",
      stringsAsFactors = FALSE
    )

    message(paste0("   (ok) ", nrow(df_final), " registros padronizados."))
    return(tibble::as_tibble(df_final))

  }, error = function(e) {
    message("Erro: ", e$message)
    return(data.frame())
  })
}
