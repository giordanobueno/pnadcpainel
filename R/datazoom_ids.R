#' Criar Identificadores Longitudinais (Metodologia Data Zoom)
#'
#' Aplica a metodologia desenvolvida pelo Data Zoom (PUC-Rio) para construcao
#' dos identificadores de domicilio (`id_dom`) e individuo (`id_ind`) a partir das
#' colunas primarias da PNAD Continua.
#'
#' @param dados Um data frame contendo os microdados brutos da PNADc. Deve conter
#'   as colunas \code{UPA}, \code{V1008}, \code{V1014}, \code{V2008}, \code{V20081},
#'   \code{V20082}, \code{V2007} e \code{UF}.
#'
#' @return Um data frame / tibble com as colunas \code{id_dom} e \code{id_ind}
#'   adicionadas, e as colunas auxiliares de data (\code{V2008}, \code{V20081}, \code{V20082})
#'   removidas para economia de memoria RAM.
#'
#' @importFrom dplyr filter mutate select
#' @importFrom stringr str_pad
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' \dontrun{
#' dados_id <- criar_ids_datazoom(dados_pnadc)
#' }
criar_ids_datazoom <- function(dados) {
  cols_req <- c("UPA", "V1008", "V1014", "V2008", "V20081", "V20082", "V2007", "UF")
  faltantes <- setdiff(cols_req, names(dados))
  if (length(faltantes) > 0) {
    stop("Colunas obrigatorias ausentes para criar IDs Data Zoom: ", paste(faltantes, collapse = ", "))
  }

  dados %>%
    dplyr::filter(
      .data$V2008 != 99L,
      .data$V20081 != 99L,
      .data$V20082 != 9999L,
      !is.na(.data$V2007)
    ) %>%
    dplyr::mutate(
      dia    = stringr::str_pad(.data$V2008, 2L, pad = "0"),
      mes    = stringr::str_pad(.data$V20081, 2L, pad = "0"),
      ano    = as.character(.data$V20082),
      sexo   = as.character(.data$V2007),
      uf     = as.character(.data$UF),
      id_dom = paste0(.data$UPA, .data$V1008, .data$V1014),
      id_ind = paste0(.data$id_dom, .data$dia, .data$mes, .data$ano, .data$sexo, .data$uf)
    ) %>%
    dplyr::select(-"dia", -"mes", -"ano", -"sexo", -"uf", -"V2008", -"V20081", -"V20082")
}
