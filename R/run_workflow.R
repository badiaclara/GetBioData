#' Full biodiversity workflow: GBIF + speciesLink + deduplication
#'
#' This function automates the entire process: downloading data from both sources,
#' merging them into 16 standardized columns, and labeling/removing duplicates.
#'
#' @param species Character vector of scientific names.
#' @param export_path Character. Directory to save outputs (defaults to current directory).
#' @return A list with two tibbles per species: labeled data and clean data.
#' @export
run_workflow <- function(species, export_path = getwd()) {

  if (!dir.exists(export_path)) {
    stop("O caminho de exportação não existe! Verifique o parâmetro export_path.")
  }

  results_list <- list()

  for (sp in species) {
    message(paste0("\n==============================================="))
    message(paste0("   INICIANDO PROCESSAMENTO: ", sp))
    message(paste0("==============================================="))

    # 1. Coleta (Usa as funções internas já padronizadas)
    d1 <- get_gbif(sp)
    d2 <- get_splink(sp)

    # 2. União Refatorada (Garante as 16 colunas e trata erros)
    combined <- bind_biodata(d1, d2)

    if (nrow(combined) == 0) {
      message(paste0("(!) Sem dados retornados em nenhuma base para: ", sp))
      next
    }

    # 3. Deduplicação e Limpeza
    labeled <- dedup_label(combined)
    clean   <- kill_dedup(labeled)

    # 4. Exportação (Usa o nome da espécie para o arquivo)
    sp_safe <- gsub(" ", "_", sp)

    file_label <- file.path(export_path, paste0("GetBioData_LABEL_", sp_safe, ".xlsx"))
    file_kill  <- file.path(export_path, paste0("GetBioData_KILL_", sp_safe, ".xlsx"))

    writexl::write_xlsx(labeled, file_label)
    writexl::write_xlsx(clean, file_kill)

    message(paste0("✔ Concluído! Arquivos salvos em: ", export_path))
    message(paste0("   Registros originais: ", nrow(labeled)))
    message(paste0("   Registros pós-dedup: ", nrow(clean)))

    # Armazena na lista de resultados para uso no R
    results_list[[sp]] <- list(labeled = labeled, clean = clean)
  }

  return(invisible(results_list))
}
