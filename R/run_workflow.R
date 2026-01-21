#' Full biodiversity workflow: GBIF + speciesLink + deduplication
#'
#' @param species Character vector of scientific names.
#' @param export_path Character. Directory to save outputs.
#' @return A list with two tibbles: [[1]] Labeled data, [[2]] Clean data.
#' @export
run_workflow <- function(species, export_path = getwd()) {
  if (!dir.exists(export_path)) stop("O caminho de exportação não existe!")

  results_list <- list()

  for (sp in species) {
    message(paste0("\n=== Iniciando Processamento: ", sp, " ==="))

    d1 <- get_gbif(sp)
    d2 <- get_splink(sp)

    combined <- dplyr::bind_rows(d1, d2)

    if (nrow(combined) == 0) {
      message(paste0("(!) Sem dados para ", sp, ". Pulando..."))
      next
    }

    message("-> Executando deduplicação...")
    labeled <- dedup_label(combined)
    clean <- kill_dedup(labeled)

    # Exportação
    sp_safe <- gsub(" ", "_", sp)
    writexl::write_xlsx(labeled, file.path(export_path, paste0("GetBioData_LABEL_", sp_safe, ".xlsx")))
    writexl::write_xlsx(clean, file.path(export_path, paste0("GetBioData_KILL_", sp_safe, ".xlsx")))

    message(paste0("✔ Concluído! Arquivos salvos em: ", export_path))

    results_list[[sp]] <- list(labeled = labeled, clean = clean)
  }

  return(invisible(results_list))
}
