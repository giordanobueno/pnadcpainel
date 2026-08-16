#' Wrapper Interno de Download da PNADc com Suporte a Mocks e Retry
#'
#' @param year Ano de referencia.
#' @param quarter Trimestre (1-4) para dados trimestrais, ou NULL.
#' @param interview Entrevista (1-5) para dados de Visita, ou NULL.
#' @param vars Vetor de variaveis a baixar.
#' @param design Logico.
#' @param labels Logico.
#' @param verbose Logico.
#'
#' @return Data frame com os microdados baixados.
#' @keywords internal
get_pnadc_internal <- function(year,
                               quarter = NULL,
                               interview = NULL,
                               vars = NULL,
                               design = FALSE,
                               labels = FALSE,
                               verbose = TRUE) {
  # Suporte a provedor mock durante testes automatizados
  mock_provider <- getOption("pnadcpainel.mock_provider", default = NULL)
  if (!is.null(mock_provider) && is.function(mock_provider)) {
    return(mock_provider(
      year      = year,
      quarter   = quarter,
      interview = interview,
      vars      = vars,
      design    = design,
      labels    = labels
    ))
  }

  rotulo <- if (!is.null(quarter)) {
    sprintf("Download Trimestre %d/%d", quarter, year)
  } else if (!is.null(interview)) {
    sprintf("Download Visita %d/%d", interview, year)
  } else {
    sprintf("Download PNADc %d", year)
  }

  executar_com_retry(
    expr = function() {
      PNADcIBGE::get_pnadc(
        year      = year,
        quarter   = quarter,
        interview = interview,
        vars      = vars,
        design    = design,
        labels    = labels
      )
    },
    max_tentativas = 3L,
    delay_inicial  = 1,
    verbose        = verbose,
    rotulo         = rotulo
  )
}
