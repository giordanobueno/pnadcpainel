#' Baixar e consolidar dados de Visita 1 para domicilios
#'
#' Baixa os microdados de entrevista 1 (Visita 1) para o ano corrente e
#' o ano anterior (se disponivel), consolidando as informacoes no nivel do domicilio (`id_dom`).
#'
#' @param ano Ano corrente de referencia (inteiro).
#' @param vars_visita Vetor de variaveis de habitacao a serem baixadas.
#' @param verbose Logico. Se TRUE, exibe mensagens informativas.
#'
#' @return Um data frame / tibble contendo uma linha por domicilio (`id_dom`)
#'   com as primeiras respostas nao-NA de cada variavel de habitacao.
#' @importFrom dplyr mutate select group_by summarise across all_of bind_rows
#' @importFrom rlang .data
#' @export
consolidar_base_habitacao <- function(ano, vars_visita = vars_visita_default, verbose = TRUE) {
  if (!"package:PNADcIBGE" %in% search()) {
    suppressPackageStartupMessages(requireNamespace("PNADcIBGE", quietly = TRUE))
    try(attachNamespace("PNADcIBGE"), quietly = TRUE)
  }

  dados_casa_lista <- list()

  # 1. Ano corrente
  if (verbose) {
    message(">>> Baixando Habitacao ", ano, " (Visita 1)...")
  }
  casa_corrente <- tryCatch(
    {
      df <- PNADcIBGE::get_pnadc(
        year      = ano,
        interview = 1,
        vars      = vars_visita,
        design    = FALSE,
        labels    = FALSE
      )
      downcast_pnadc(df)
    },
    error = function(e) {
      stop("Falha ao baixar dados de Visita 1 para o ano ", ano, ": ", e$message)
    }
  )
  dados_casa_lista[[1]] <- casa_corrente

  # 2. Ano anterior (caso de borda: ex 2012)
  ano_anterior <- ano - 1
  casa_anterior <- tryCatch(
    {
      if (verbose) {
        message(">>> Baixando Habitacao ", ano_anterior, " (Visita 1)...")
      }
      df <- PNADcIBGE::get_pnadc(
        year      = ano_anterior,
        interview = 1,
        vars      = vars_visita,
        design    = FALSE,
        labels    = FALSE
      )
      downcast_pnadc(df)
    },
    error = function(e) {
      warning(
        "Nao foi possivel baixar dados de Visita 1 para o ano anterior (", ano_anterior, "). ",
        "A consolidacao sera realizada apenas com os dados de ", ano, ". Erro: ", e$message,
        call. = FALSE
      )
      NULL
    }
  )

  if (!is.null(casa_anterior)) {
    dados_casa_lista[[2]] <- casa_anterior
  }

  dados_casa_total <- dplyr::bind_rows(dados_casa_lista)
  rm(dados_casa_lista, casa_corrente, casa_anterior)
  gc()

  if (verbose) {
    message(">>> Consolidando Base de Habitacao...")
  }

  chaves <- c("UPA", "V1008", "V1014", "Ano", "UF")
  vars_hab_especificas <- setdiff(vars_visita, chaves)

  base_habitacao <- dados_casa_total %>%
    dplyr::mutate(id_dom = paste0(.data$UPA, .data$V1008, .data$V1014)) %>%
    dplyr::select(dplyr::all_of(c("id_dom", vars_hab_especificas))) %>%
    dplyr::group_by(.data$id_dom) %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(vars_hab_especificas),
        ~ {
          v <- stats::na.omit(.x)
          if (length(v) > 0) v[1] else .x[1]
        }
      ),
      .groups = "drop"
    )

  rm(dados_casa_total)
  gc()

  return(base_habitacao)
}
