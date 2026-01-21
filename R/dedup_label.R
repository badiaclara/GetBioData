#' Label duplicate biodiversity occurrence data
#'
#' @param df Tibble with occurrence data
#' @return Tibble with added DUP_GROUP and DUP_CRITERIA columns.
#' @export
dedup_label <- function(df) {
  t_start <- Sys.time()
  cat("\n=== Iniciando DEDUPLICAÇÃO ===\n")

  df_proc <- df |>
    dplyr::mutate(
      key_inst_cat = paste0(INSTITUIÇÃO, "_", CATÁLOGO),
      key_occ_id   = as.character(OCCURRENCE_ID),
      key_col_evt  = paste0(COLETOR, "_", DATA_COLETA),
      key_geo_date = paste0(LATITUDE, "_", LONGITUDE, "_", DATA_COLETA),
      DUP_GROUP = NA_integer_,
      DUP_CRITERIA = NA_character_,
      FLAG_POTENTIAL_DUP = FALSE
    )

  current_group_id <- 0

  assign_dups <- function(d, key_col, criteria_name) {
    valid_keys <- !is.na(d[[key_col]]) & d[[key_col]] != "" & !grepl("NA_NA", d[[key_col]])
    potential_dups <- d[[key_col]][valid_keys & is.na(d$DUP_GROUP)]

    if (length(potential_dups) > 0) {
      dup_keys <- unique(potential_dups[duplicated(potential_dups)])
      if (length(dup_keys) > 0) {
        idx_to_update <- which(d[[key_col]] %in% dup_keys & is.na(d$DUP_GROUP))
        match_map <- setNames(seq_along(dup_keys) + current_group_id, dup_keys)
        d$DUP_GROUP[idx_to_update] <- match_map[d[[key_col]][idx_to_update]]
        d$DUP_CRITERIA[idx_to_update] <- criteria_name
        d$FLAG_POTENTIAL_DUP[idx_to_update] <- TRUE
        current_group_id <<- max(d$DUP_GROUP, na.rm = TRUE)
      }
    }
    return(d)
  }

  cat("-> Cruzando Instituição + Catálogo...\n")
  df_proc <- assign_dups(df_proc, "key_inst_cat", "institution_catalog")
  cat("-> Cruzando Occurrence ID...\n")
  df_proc <- assign_dups(df_proc, "key_occ_id", "occurrence_id")
  cat("-> Cruzando Coletor + Data...\n")
  df_proc <- assign_dups(df_proc, "key_col_evt", "collector_date")
  cat("-> Cruzando Coordenadas + Data...\n")
  df_proc <- assign_dups(df_proc, "key_geo_date", "coords_exact_date")

  t_end <- Sys.time()
  cat("Tempo de execução:", round(as.numeric(difftime(t_end, t_start, units = "secs")), 2), "s\n")
  return(df_proc |> dplyr::select(-dplyr::starts_with("key_")))
}
