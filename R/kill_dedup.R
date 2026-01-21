#' Remove duplicates based on dedup_label flags
#'
#' @param df Dataframe from dedup_label
#' @return Clean dataframe
#' @export
kill_dedup <- function(df) {

  if (!"FLAG_POTENTIAL_DUP" %in% names(df)) return(df)

  df |>
    dplyr::group_by(DUP_GROUP) |>
    # Mantém se DUP_GROUP é NA (não é duplicata) OU se é o 1º do grupo de duplicatas
    dplyr::filter(is.na(DUP_GROUP) | dplyr::row_number() == 1) |>
    dplyr::ungroup() |>
    dplyr::select(-DUP_GROUP, -DUP_CRITERIA, -FLAG_POTENTIAL_DUP)
}
