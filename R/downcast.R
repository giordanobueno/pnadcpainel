#' Downcast PNADc data types
#'
#' Converte colunas numericas/categoricas dos microdados da PNADc para inteiros de 32 bits
#' para otimizar o uso da memoria RAM durante o processamento.
#'
#' @param df Data frame / tibble contendo microdados da PNADc.
#' @return Data frame com colunas categoricas/inteiras convertidas para tipo integer.
#' @keywords internal
downcast_pnadc <- function(df) {
  colunas_int <- c(
    "V2007", "V2008", "V20081", "V20082",
    "V2001", "V2005", "V2009",
    "VD3004", "V3001",
    "VD4001", "VD4002", "VD4009", "VD4010",
    "V5001A", "V5002A",
    "S01013", "S01006", "S01010",
    "Ano", "Trimestre", "UF"
  )
  cols_presentes <- intersect(colunas_int, names(df))
  if (length(cols_presentes) > 0) {
    df <- dplyr::mutate(df, dplyr::across(dplyr::all_of(cols_presentes), as.integer))
  }
  return(df)
}
