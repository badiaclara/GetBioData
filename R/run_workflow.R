#' Full biodiversity workflow: GBIF + speciesLink + deduplication
#'
#' Automates the entire process: downloading, merging into standardized columns,
#' labeling duplicates, and cleaning the final dataset.
#'
#' @param species Character vector of scientific names.
#' @param export_path Character. Directory to save outputs (defaults to current directory).
#' @return A list with two tibbles per species: [[labeled]] and [[clean]].
#' @export
run_workflow <- function(species, export_path = getwd()) {

  if (!dir.exists(export_path)) {
    stop("O caminho de exportação não existe! Verifique o parâmetro export_path.")
  }

  results_list <- list()

  for (sp in species) {
    message(paste0("\n", paste(rep("=", 50), collapse = "")))
    message(paste0("   PROCESSANDO: ", sp))
    message(paste(rep("=", 50), collapse = ""))

    # 1. Coleta e Padronização (As 16 colunas)
    d1 <- get_gbif(sp)
    d2 <- get_splink(sp)

    # 2. União Robusta
    combined <- bind_biodata(d1, d2)

    if (nrow(combined) == 0) {
      message(paste0("(!) Nenhum dado encontrado para: ", sp))
      next
    }

    # 3. Inteligência de Dados: Deduplicação
    # Agora usa a lógica flexível (Coletor, Ano/Data, Coordenadas)
    labeled <- dedup_label(combined)

    # 4. Limpeza Final
    # Remove as duplicatas e a coluna de status para o arquivo final
    clean <- kill_dedup(labeled)

    # 5. Exportação para Excel
    # Nomeamos os arquivos de forma organizada
    sp_safe <- gsub(" ", "_", sp)

    file_label <- file.path(export_path, paste0("GetBioData_COMPLETO_", sp_safe, ".xlsx"))
    file_kill  <- file.path(export_path, paste0("GetBioData_LIMPO_", sp_safe, ".xlsx"))

    # Salvando os arquivos
    writexl::write_xlsx(labeled, file_label)
    writexl::write_xlsx(clean, file_kill)

    message(paste0("\n✔ Sucesso para ", sp, "!"))
    message(paste0("   -> Arquivo com duplicatas marcadas salvo."))
    message(paste0("   -> Arquivo limpo (", nrow(clean), " registros) salvo."))

    # Salva na lista para retorno no R
    results_list[[sp]] <- list(labeled = labeled, clean = clean)
  }

  message("\n===============================================")
  message("   WORKFLOW CONCLUÍDO COM SUCESSO!")
  message("===============================================")

  return(invisible(results_list))
}
