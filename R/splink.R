#' Download occurrence data from speciesLink for a given species
#'
#' This function downloads occurrence records from speciesLink for a specific species,
#' handling coordinates, dates, collectors, and other metadata for downstream deduplication.
#'
#' @param species Character. Scientific name (e.g., "Araucaria angustifolia").
#'                Can include subspecies using "ssp." or "subsp.".
#' @param limit Integer. Maximum number of records to download (default 5000).
#' @param api_key Character. Your speciesLink API key. If NULL, will use Sys.getenv("SPLINK_API_KEY").
#'
#' @return A tibble with taxonomic, geographic, temporal, and collector metadata.
#' @export
get_splink <- function(species, limit = 5000, api_key = NULL) {

  if (is.null(api_key)) {
    api_key <- Sys.getenv("SPLINK_API_KEY")
    if (api_key == "") stop("SPLINK_API_KEY not found. Set it via Sys.setenv() or pass api_key argument.")
  }

  # Encode species for URL
  species_url <- utils::URLencode(species)

  # Build URL
  url_splink <- paste0(
    "https://specieslink.net/ws/1.0/search?scientificname=",
    species_url,
    "&apikey=", api_key,
    "&limit=", limit
  )

  # GET request with SSL disabled
  response <- httr::GET(
    url_splink,
    httr::config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE),
    httr::timeout(120)
  )

  if (httr::status_code(response) != 200) {
    stop("speciesLink request failed (HTTP ", httr::status_code(response), ").")
  }

  data <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))

  if (is.null(data$features) || length(data$features) == 0) {
    stop("No records returned from speciesLink for the provided name.")
  }

  # Convert properties and geometry to tibble
  df_props <- tibble::as_tibble(as.data.frame(data$features$properties))
  df_geom  <- tibble::as_tibble(as.data.frame(data$features$geometry))

  # Ensure critical columns exist
  colunas_criticas <- c(
    "occurrenceid", "catalognumber", "recordnumber",
    "scientificname", "kingdom", "phylum", "class",
    "order", "family", "genus", "recordedby",
    "identifiedby", "yearidentified", "collectioncode",
    "institutioncode", "basisofrecord", "geodeticdatum",
    "coordinateprecision", "country", "stateprovince",
    "county", "locality", "verbatimlocality",
    "yearcollected", "monthcollected", "daycollected"
  )
  for (col in colunas_criticas) {
    if (!col %in% names(df_props)) df_props[[col]] <- NA
  }

  # Extract coordinates
  df_props$LATITUDE <- NA_real_
  df_props$LONGITUDE <- NA_real_
  for (i in seq_len(nrow(df_geom))) {
    coord_vec <- df_geom$coordinates[[i]]
    if (length(coord_vec) == 2 && !all(coord_vec == 0)) {
      df_props$LONGITUDE[i] <- as.numeric(coord_vec[1])
      df_props$LATITUDE[i]  <- as.numeric(coord_vec[2])
    }
  }

  # Construct collection date
  df_props$DATA_COLETA <- NA_character_
  df_props$ANO <- NA_integer_
  df_props$MÊS <- NA_integer_
  df_props$DIA <- NA_integer_
  for (i in seq_len(nrow(df_props))) {
    year  <- df_props$yearcollected[i]
    month <- df_props$monthcollected[i]
    day   <- df_props$daycollected[i]
    if (!is.na(year)) {
      m <- if (is.na(month)) 1 else as.integer(month)
      d <- if (is.na(day)) 1 else as.integer(day)
      df_props$ANO[i] <- as.integer(year)
      df_props$MÊS[i] <- m
      df_props$DIA[i] <- d
      df_props$DATA_COLETA[i] <- paste0(year, "-", sprintf("%02d", m), "-", sprintf("%02d", d))
    }
  }

  # Create final tibble with all metadata
  splink_df <- dplyr::mutate(df_props,
                             ID_SPLink = as.character(catalognumber),
                             OCCURRENCE_ID = as.character(occurrenceid),
                             CATÁLOGO = as.character(catalognumber),
                             ESPÉCIE = scientificname,
                             REINO = kingdom,
                             FILO = phylum,
                             CLASSE = class,
                             ORDEM = order,
                             FAMÍLIA = family,
                             GÊNERO = genus,
                             LATITUDE = LATITUDE,
                             LONGITUDE = LONGITUDE,
                             PRECISÃO_METROS = as.numeric(coordinateprecision),
                             DATUM = geodeticdatum,
                             PAÍS = country,
                             ESTADO = stateprovince,
                             MUNICÍPIO = county,
                             LOCALIDADE = locality,
                             LOCALIDADE_ORIGINAL = verbatimlocality,
                             DATA_COLETA = DATA_COLETA,
                             ANO = ANO,
                             MÊS = MÊS,
                             DIA = DIA,
                             COLETOR = recordedby,
                             NÚMERO_COLETOR = recordnumber,
                             DETERMINADOR = identifiedby,
                             DATA_DETERMINAÇÃO = as.character(yearidentified),
                             INSTITUIÇÃO = institutioncode,
                             COLEÇÃO = collectioncode,
                             TIPO_REGISTRO = basisofrecord,
                             STATUS_OCORRÊNCIA = NA_character_,
                             DATASET_SPLink = "SPLink",
                             COLEÇÃO_CÓDIGO = as.character(collectionid),
                             DATA_ACESSO = Sys.Date(),
                             FONTE = "SPLink"
  )

  # Select columns in desired order
  splink_df <- dplyr::select(splink_df,
                             ID_SPLink, OCCURRENCE_ID, CATÁLOGO,
                             ESPÉCIE, REINO, FILO, CLASSE, ORDEM, FAMÍLIA, GÊNERO,
                             LATITUDE, LONGITUDE, PRECISÃO_METROS, DATUM,
                             PAÍS, ESTADO, MUNICÍPIO, LOCALIDADE, LOCALIDADE_ORIGINAL,
                             DATA_COLETA, ANO, MÊS, DIA,
                             COLETOR, NÚMERO_COLETOR, DETERMINADOR, DATA_DETERMINAÇÃO,
                             INSTITUIÇÃO, COLEÇÃO,
                             TIPO_REGISTRO, STATUS_OCORRÊNCIA,
                             DATASET_SPLink, COLEÇÃO_CÓDIGO, DATA_ACESSO, FONTE
  )

  return(splink_df)
}
