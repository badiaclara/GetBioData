#' Remove duplicates based on dedup_label flags
#'
#' @param df Dataframe from dedup_label
#' @return Clean dataframe
#' @export
kill_dedup <- function(df) {
  if (!"status_dedup" %in% names(df)) return(df)

  df_limpo <- df |>
    dplyr::filter(status_dedup == "Original") |>
    dplyr::select(-status_dedup)

  return(df_limpo)
}

