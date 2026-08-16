#' Diagnosticar Preenchimento de Colunas no Painel
#'
#' Calcula metricas de disponibilidade (linhas preenchidas vs. ausentes) para cada
#' coluna do painel gerado.
#'
#' @param painel Tibble/Data frame contendo o painel cruzado.
#' @param colunas Vetor de nomes de colunas a diagnosticar. Se NULL, diagnostica todas as colunas.
#'
#' @return Um tibble contendo as colunas: \code{variavel}, \code{total_linhas},
#'   \code{com_dado}, \code{sem_dado}, \code{pct_disponivel}.
#' @importFrom dplyr select group_by summarise arrange desc mutate n tibble
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#' @export
diagnosticar_painel <- function(painel, colunas = NULL) {
  if (is.null(colunas)) {
    colunas <- names(painel)
  } else {
    colunas <- intersect(colunas, names(painel))
  }

  if (length(colunas) == 0L) {
    stop("Nenhuma coluna valida fornecida para diagnostico.", call. = FALSE)
  }

  if (nrow(painel) == 0L) {
    return(dplyr::tibble(
      variavel       = colunas,
      total_linhas   = 0L,
      com_dado       = 0L,
      sem_dado       = 0L,
      pct_disponivel = 0.0
    ))
  }

  diag_tb <- painel %>%
    dplyr::select(dplyr::all_of(colunas)) %>%
    tidyr::pivot_longer(cols = dplyr::everything(), names_to = "variavel", values_to = "valor") %>%
    dplyr::group_by(.data$variavel) %>%
    dplyr::summarise(
      total_linhas   = dplyr::n(),
      com_dado       = sum(!is.na(.data$valor)),
      sem_dado       = sum(is.na(.data$valor)),
      pct_disponivel = if (dplyr::n() > 0L) round((sum(!is.na(.data$valor)) / dplyr::n()) * 100, 2) else 0.0,
      .groups = "drop"
    ) %>%
    dplyr::arrange(.data$pct_disponivel, .data$variavel)

  diag_tb
}

#' Imprimir Mensagem Padronizada de Diagnostico de Perda de Dados
#'
#' Emite uma mensagem estruturada no console descrevendo a perda de dados resultante
#' do cruzamento e balanceamento temporal entre a base trimestral e a Visita 1.
#'
#' @param diagnostico Tibble gerado por \code{diagnosticar_painel}.
#' @param painel_antes Data frame do painel antes do balanceamento.
#' @param painel_depois Data frame do painel apos o balanceamento (ou igual se sem balancear).
#' @param ano Ano de referencia.
#'
#' @return A string formatada da mensagem (invisivelmente).
#' @export
mensagem_diagnostico <- function(diagnostico, painel_antes, painel_depois, ano) {
  n_antes <- if (!is.null(painel_antes)) nrow(painel_antes) else 0L
  n_depois <- if (!is.null(painel_depois)) nrow(painel_depois) else 0L
  perda_abs <- n_antes - n_depois
  perda_pct <- if (n_antes > 0L) round((perda_abs / n_antes) * 100, 2) else 0.0

  if (!is.null(diagnostico) && nrow(diagnostico) > 0L) {
    var_critica_row <- diagnostico[1L, ]
    var_critica <- var_critica_row$variavel
    pct_ausente_critica <- round(100 - var_critica_row$pct_disponivel, 2)
  } else {
    var_critica <- "Nenhuma"
    pct_ausente_critica <- 0.0
  }

  msg <- sprintf(
    paste0(
      "\n>>> Diagn\u00f3stico do painel PNADc - ano %d\n",
      "Linhas antes do cruzamento (base trimestral): %s\n",
      "Linhas ap\u00f3s cruzamento + balanceamento:       %s\n",
      "Perda total: %s linhas (%.2f%%)\n",
      "Vari\u00e1vel com maior perda antes do balanceamento: %s - %.2f%% de dados ausentes\n",
      "Motivo: descompasso temporal entre a base trimestral (Ano/Trimestre corrente) ",
      "e a base de Visita 1 (entrevista espec\u00edfica, ano corrente + ano anterior).\n"
    ),
    ano,
    format(n_antes, big.mark = ".", decimal.mark = ","),
    format(n_depois, big.mark = ".", decimal.mark = ","),
    format(perda_abs, big.mark = ".", decimal.mark = ","),
    perda_pct,
    var_critica,
    pct_ausente_critica
  )

  message(msg)
  invisible(msg)
}
