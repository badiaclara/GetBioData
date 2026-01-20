#' Full biodiversity workflow: GBIF + speciesLink + deduplication
#'
#' @param species Character vector of scientific names.
#' @param export_path Character. Directory to save outputs.
#' @return A list with two tibbles: [[1]] Labeled data, [[2]] Clean data.
#' @export
run_workflow <- function(species, export_path = getwd()) {
  results_labeled <- list()
  results_clean   <- list()

  for (sp in species) {
    cat("\n", strrep("=", 50), "\n")
    cat("  PROCESSANDO: ", toupper(sp), "\n")
    cat(strrep("=", 50), "\n")

    cat("-> Capturando dados GBIF e SPLink...\n")
    df_gbif   <- tryCatch(get_gbif(sp), error = function(e) dplyr::tibble())
    df_splink <- tryCatch(get_splink(sp), error = function(e) dplyr::tibble())

    combined_raw <- dplyr::bind_rows(df_gbif, df_splink)
    if (nrow(combined_raw) == 0) {
      cat("! Nenhum dado encontrado para:", sp, "\n")
      next
    }

    cat("-> Executando motor de deduplicação...\n")
    df_labeled <- dedup_label(combined_raw)
    df_clean <- kill_dedup(df_labeled)

    sp_safe <- gsub(" ", "_", sp)
    writexl::write_xlsx(df_labeled, file.path(export_path, paste0("GetBioData_LABEL_", sp_safe, ".xlsx")))
    writexl::write_xlsx(df_clean, file.path(export_path, paste0("GetBioData_KILL_", sp_safe, ".xlsx")))

    cat("-> OK: Gerados arquivos LABEL e KILL para", sp, "\n")
    results_labeled[[sp]] <- df_labeled
    results_clean[[sp]]   <- df_clean
  }

  return(list(labeled = dplyr::bind_rows(results_labeled), clean = dplyr::bind_rows(results_clean)))
}
