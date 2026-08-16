#' Executar Funcao com Repeticao (Retry) e Backoff Exponencial
#'
#' @param expr Expressao ou funcao a ser executada.
#' @param max_tentativas Numero maximo de tentativas (padrao 3).
#' @param delay_inicial Delay inicial em segundos (padrao 1).
#' @param verbose Logico. Se TRUE, exibe mensagens de progresso.
#' @param rotulo String identificadora da operacao para log.
#'
#' @return O resultado da execucao de \code{expr}.
#' @keywords internal
executar_com_retry <- function(expr,
                               max_tentativas = 3L,
                               delay_inicial = 1,
                               verbose = TRUE,
                               rotulo = "Requisicao") {
  tentativa <- 1L
  delay <- delay_inicial

  while (tentativa <= max_tentativas) {
    inicio <- Sys.time()
    res <- tryCatch(
      {
        val <- expr()
        if (verbose && tentativa > 1L) {
          duracao <- round(as.numeric(difftime(Sys.time(), inicio, units = "secs")), 2)
          message(sprintf(
            ">>> %s bem-sucedida na tentativa %d/%d (%.2fs)",
            rotulo, tentativa, max_tentativas, duracao
          ))
        }
        return(val)
      },
      error = function(e) {
        duracao <- round(as.numeric(difftime(Sys.time(), inicio, units = "secs")), 2)
        if (tentativa < max_tentativas) {
          if (verbose) {
            message(sprintf(
              ">>> %s falhou na tentativa %d/%d (%.2fs). Erro: %s. Tentando em %.1fs...",
              rotulo, tentativa, max_tentativas, duracao, e$message, delay
            ))
          }
          Sys.sleep(delay)
          delay <<- delay * 2
          return(NULL)
        } else {
          stop(sprintf(
            "%s falhou apos %d tentativas. Erro final: %s",
            rotulo, max_tentativas, e$message
          ), call. = FALSE)
        }
      }
    )
    if (!is.null(res)) {
      return(res)
    }
    tentativa <- tentativa + 1L
  }
}
