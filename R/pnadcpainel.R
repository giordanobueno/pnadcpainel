#' Variaveis padrao trimestrais da PNAD Contínua
#'
#' Lista contendo as variaveis padrao da base trimestral utilizadas para
#' construcao do painel de pessoas.
#'
#' @export
vars_tri_default <- c(
  "UPA", "V1008", "V1014", "Ano", "Trimestre", "UF",
  "V2007", "V2008", "V20081", "V20082",
  "V2001", "V2005", "V2009",
  "VD3004", "V3001",
  "VD4001", "VD4002", "VD4009", "VD4020", "VD4010"
)

#' Variaveis padrao de Visita 1 (Domicilio) da PNAD Contínua
#'
#' Lista contendo as variaveis padrao da base de Visita 1 utilizadas para
#' caracterizacao domiciliar e rendimento per capita.
#'
#' @export
vars_visita_default <- c(
  "UPA", "V1008", "V1014", "Ano", "UF",
  "V5001A",
  "VD5002", "V5002A",
  "S01013", "S01006", "S01010"
)

#' Otimizacao de Memoria (Downcasting) para Microdados PNADc
#'
#' Converte colunas numericas e categoricas dos microdados para tipo integer
#' economizando memoria RAM.
#'
#' @param df Data frame contendo microdados da PNADc.
#'
#' @return Data frame com colunas otimizadas para integer.
#' @importFrom dplyr mutate across all_of intersect
#' @export
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
  cols_presentes <- dplyr::intersect(colunas_int, names(df))
  df %>% dplyr::mutate(dplyr::across(dplyr::all_of(cols_presentes), as.integer))
}

#' Criar Identificadores Longitudinais Data Zoom
#'
#' Aplica a metodologia desenvolvida pelo Data Zoom (PUC-Rio) para construcao
#' dos identificadores longitudinais de domicilio (id_dom) e individuo (id_ind).
#'
#' @param dados Data frame contendo os microdados da PNADc.
#'
#' @return Data frame com id_dom e id_ind adicionados.
#' @importFrom dplyr filter mutate select stringr
#' @importFrom rlang .data
#' @export
criar_ids_datazoom <- function(dados) {
  chaves_obrig <- c("UPA", "V1008", "V1014", "V2008", "V20081", "V20082", "V2007", "UF")
  faltantes <- setdiff(chaves_obrig, names(dados))
  if (length(faltantes) > 0L) {
    stop(paste("Colunas obrigatorias ausentes para criar IDs Data Zoom:", paste(faltantes, collapse = ", ")), call. = FALSE)
  }

  dados %>%
    dplyr::filter(
      !is.na(.data$V2008),
      !is.na(.data$V20081),
      !is.na(.data$V20082),
      !is.na(.data$V2007),
      .data$V2008 != 99L,
      .data$V20081 != 99L,
      .data$V20082 != 9999L
    ) %>%
    dplyr::mutate(
      id_dom = paste0(.data$UPA, stringr::str_pad(.data$V1008, width = 2, pad = "0"), .data$V1014),
      id_ind = paste0(
        .data$id_dom,
        stringr::str_pad(.data$V2008,  width = 2, pad = "0"),
        stringr::str_pad(.data$V20081, width = 2, pad = "0"),
        .data$V20082, .data$V2007, .data$UF
      )
    ) %>%
    dplyr::select(-.data$V2008, -.data$V20081, -.data$V20082)
}

#' Diagnosticar Preenchimento de Colunas no Painel
#'
#' @param painel Data frame contendo o painel cruzado.
#' @param colunas Vetor de nomes de colunas a diagnosticar.
#' @return Um tibble com metricas de preenchimento.
#' @export
diagnosticar_painel <- function(painel, colunas = NULL) {
  if (is.null(colunas)) {
    colunas <- names(painel)
  } else {
    colunas <- dplyr::intersect(colunas, names(painel))
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

  painel %>%
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
}

#' Imprimir Mensagem Padronizada de Diagnostico
#'
#' @param diagnostico Tibble gerado por diagnosticar_painel.
#' @param painel_antes Data frame do painel antes do balanceamento.
#' @param painel_depois Data frame do painel apos o balanceamento.
#' @param ano Ano de referencia.
#' @return A string formatada da mensagem.
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

#' Executar Funcao com Repeticao e Backoff Exponencial
#' @keywords internal
executar_com_retry <- function(expr, max_tentativas = 3L, delay_inicial = 1.0, verbose = TRUE, rotulo = "Requisicao") {
  tentativa <- 1L
  delay <- delay_inicial

  while (tentativa <= max_tentativas) {
    inicio <- Sys.time()
    res <- tryCatch(
      list(val = expr(), err = NULL),
      error = function(e) list(val = NULL, err = e)
    )

    if (is.null(res$err)) {
      if (verbose && tentativa > 1L) {
        duracao <- round(as.numeric(difftime(Sys.time(), inicio, units = "secs")), 2)
        message(sprintf(">>> %s bem-sucedida na tentativa %d/%d (%.2fs)", rotulo, tentativa, max_tentativas, duracao))
      }
      return(res$val)
    } else {
      duracao <- round(as.numeric(difftime(Sys.time(), inicio, units = "secs")), 2)
      if (tentativa < max_tentativas) {
        if (verbose) {
          message(sprintf(">>> %s falhou na tentativa %d/%d (%.2fs). Erro: %s. Tentando em %.1fs...",
                          rotulo, tentativa, max_tentativas, duracao, res$err$message, delay))
        }
        Sys.sleep(delay)
        delay <- delay * 2
      } else {
        stop(sprintf("%s falhou apos %d tentativas. Erro final: %s", rotulo, max_tentativas, res$err$message), call. = FALSE)
      }
    }
    tentativa <- tentativa + 1L
  }
}

#' Wrapper Interno com Fallback para get_pnadc
#' @keywords internal
get_pnadc_internal <- function(year, quarter = NULL, interview = NULL, vars = NULL, design = FALSE, labels = FALSE, verbose = TRUE) {
  rotulo <- if (!is.null(quarter)) sprintf("Download Trimestre %d/%d", quarter, year) else sprintf("Download Visita %d/%d", interview, year)
  executar_com_retry(
    expr = function() {
      suppressMessages(library(PNADcIBGE))
      PNADcIBGE::get_pnadc(year = year, quarter = quarter, interview = interview, vars = vars, design = design, labels = labels)
    },
    max_tentativas = 3L, delay_inicial = 1.0, verbose = verbose, rotulo = rotulo
  )
}

#' Baixar e Consolidar os 4 Trimestres de um Ano
#' @keywords internal
baixar_trimestres_pnadc <- function(ano, vars_tri = vars_tri_default, low_memory = FALSE, verbose = TRUE) {
  lista_painel <- vector("list", 4L)
  for (tri in 1:4) {
    if (verbose) message(">>> Processando Trimestre ", tri, " de ", ano, "...")
    dados_brutos <- get_pnadc_internal(year = ano, quarter = tri, vars = vars_tri, design = FALSE, labels = FALSE, verbose = verbose)
    if (is.null(dados_brutos) || nrow(dados_brutos) == 0L) stop(sprintf("Download vazio ou nulo para o Trimestre %d de %d.", tri, ano))
    dados_brutos <- downcast_pnadc(dados_brutos)
    lista_painel[[tri]] <- criar_ids_datazoom(dados_brutos)
  }
  dplyr::bind_rows(lista_painel)
}

#' Consolidar Base de Habitação (Visita 1)
#'
#' @param ano Ano de referencia.
#' @param vars_visita Vetor de variaveis de habitação.
#' @param verbose Logico.
#' @return Data frame consolidado no nivel do domicilio.
#' @export
consolidar_base_habitacao <- function(ano, vars_visita = vars_visita_default, verbose = TRUE) {
  dados_casa_lista <- list()

  if (verbose) message(">>> Baixando Habitacao ", ano, " (Visita 1)...")
  dados_casa_corrente <- tryCatch(
    get_pnadc_internal(year = ano, interview = 1, vars = if (is.character(vars_visita)) vars_visita else NULL, design = FALSE, labels = FALSE, verbose = verbose),
    error = function(e) stop(sprintf("Falha ao baixar dados de Visita 1 para o ano %d: %s", ano, e$message), call. = FALSE)
  )
  dados_casa_lista[[1]] <- downcast_pnadc(dados_casa_corrente)

  ano_anterior <- ano - 1L
  if (verbose) message(">>> Baixando Habitacao ", ano_anterior, " (Visita 1)...")
  tryCatch({
    dados_casa_ant <- get_pnadc_internal(year = ano_anterior, interview = 1, vars = if (is.character(vars_visita)) vars_visita else NULL, design = FALSE, labels = FALSE, verbose = verbose)
    if (!is.null(dados_casa_ant) && nrow(dados_casa_ant) > 0L) {
      dados_casa_lista[[2]] <- downcast_pnadc(dados_casa_ant)
    }
  }, error = function(e) {
    if (verbose) warning(sprintf("Nao foi possivel baixar dados de Visita 1 para o ano anterior (%d). A consolidacao sera realizada apenas com os dados de %d. Erro: %s", ano_anterior, ano, e$message), call. = FALSE)
  })

  dados_casa_total <- dplyr::bind_rows(dados_casa_lista)
  if (verbose) message(">>> Consolidando Base de Habitacao...")

  chaves <- c("UPA", "V1008", "V1014", "Ano", "UF")
  vars_hab_especificas <- if (is.null(vars_visita) || (is.character(vars_visita) && length(vars_visita) == 1L && tolower(vars_visita) %in% c("todas", "all", "tudo"))) {
    setdiff(names(dados_casa_total), chaves)
  } else {
    setdiff(vars_visita, chaves)
  }

  base_habitacao <- dados_casa_total %>%
    dplyr::mutate(id_dom = paste0(.data$UPA, stringr::str_pad(.data$V1008, width = 2, pad = "0"), .data$V1014)) %>%
    dplyr::select(dplyr::all_of(c("id_dom", dplyr::intersect(vars_hab_especificas, names(dados_casa_total))))) %>%
    dplyr::group_by(.data$id_dom) %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::everything(),
        ~ { v <- stats::na.omit(.x); if (length(v) > 0) v[1] else .x[1] }
      ),
      .groups = "drop"
    )

  base_habitacao
}

#' Gerar Painel Consolidado PNAD Continua (Pessoa + Domicilio)
#'
#' @param ano Ano de referencia (inteiro).
#' @param vars_tri Vetor de variaveis trimestrais a baixar.
#' @param vars_visita Vetor de variaveis de Visita 1 a baixar.
#' @param balancear Logico.
#' @param low_memory Logico.
#' @param verbose Logico.
#' @return Um tibble consolidado contendo as informacoes de pessoas e domicilios.
#' @export
gerar_painel_pnadc <- function(ano, vars_tri = NULL, vars_visita = NULL, balancear = TRUE, low_memory = FALSE, verbose = TRUE) {
  ano_atual <- as.integer(format(Sys.Date(), "%Y"))
  if (missing(ano) || is.null(ano) || length(ano) != 1L || any(is.na(ano)) || any(is.nan(ano)) || any(is.infinite(ano)) || !is.numeric(ano)) {
    stop("O argumento 'ano' deve ser um unico numero inteiro valido.", call. = FALSE)
  }
  if (ano != suppressWarnings(as.integer(ano))) {
    stop("O argumento 'ano' deve ser um numero inteiro valido.", call. = FALSE)
  }
  ano <- as.integer(ano)
  if (ano < 2012L || ano > ano_atual) {
    stop(sprintf("Ano invalido: %d. A PNAD Continua esta disponivel entre 2012 e %d.", ano, ano_atual), call. = FALSE)
  }

  chaves_obrig_tri <- c("UPA", "V1008", "V1014", "V2008", "V20081", "V20082", "V2007", "UF", "Ano", "Trimestre")
  vars_tri_proc <- if (is.null(vars_tri)) vars_tri_default else if (is.character(vars_tri) && length(vars_tri) == 1L && tolower(vars_tri) %in% c("all", "todas", "tudo")) NULL else unique(c(chaves_obrig_tri, vars_tri))

  chaves_obrig_visita <- c("UPA", "V1008", "V1014", "Ano", "UF")
  vars_visita_proc <- if (is.null(vars_visita)) vars_visita_default else if (is.character(vars_visita) && length(vars_visita) == 1L && tolower(vars_visita) %in% c("all", "todas", "tudo")) NULL else unique(c(chaves_obrig_visita, vars_visita))

  painel_pessoas <- baixar_trimestres_pnadc(ano = ano, vars_tri = vars_tri_proc, low_memory = low_memory, verbose = verbose)
  base_habitacao <- consolidar_base_habitacao(ano = ano, vars_visita = vars_visita_proc, verbose = verbose)

  if (verbose) message(">>> Realizando o cruzamento final (left_join por id_dom)...")
  painel_cruzado <- dplyr::left_join(painel_pessoas, base_habitacao, by = "id_dom")

  vars_hab_especificas <- if (is.null(vars_visita_proc)) {
    setdiff(names(painel_cruzado), c("id_dom", "id_ind", chaves_obrig_visita, vars_tri_proc))
  } else {
    setdiff(vars_visita_proc, chaves_obrig_visita)
  }
  vars_hab_especificas <- dplyr::intersect(vars_hab_especificas, names(painel_cruzado))

  diag_tb <- diagnosticar_painel(painel_cruzado, colunas = vars_hab_especificas)

  painel_final <- if (balancear && length(vars_hab_especificas) > 0L) {
    painel_cruzado %>% dplyr::filter(dplyr::if_all(dplyr::all_of(vars_hab_especificas), ~ !is.na(.x)))
  } else {
    painel_cruzado
  }

  if (verbose && nrow(diag_tb) > 0L) {
    mensagem_diagnostico(diagnostico = diag_tb, painel_antes = painel_cruzado, painel_depois = painel_final, ano = ano)
  }

  attr(painel_final, "diagnostico") <- diag_tb
  painel_final
}
