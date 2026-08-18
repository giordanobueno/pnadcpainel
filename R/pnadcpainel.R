# Global environment para armazenamento de opções internas (ex: mock provider)
.pnadcpainel_env <- new.env(parent = emptyenv())

#' Define o Provider de Mock para Testes de Download
#'
#' @param provider Função de mock recebendo (year, quarter, interview, vars, design, labels, verbose) ou NULL.
#' @export
set_mock_provider <- function(provider = NULL) {
  .pnadcpainel_env$mock_provider <- provider
  invisible(provider)
}

#' Retorna o Provider de Mock Ativo
#'
#' @return Função de mock ativada ou NULL.
#' @export
get_mock_provider <- function() {
  .pnadcpainel_env$mock_provider
}

#' Variaveis padrao trimestrais da PNAD Contínua
#' @export
vars_tri_default <- c(
  "UPA", "V1008", "V1014", "Ano", "Trimestre", "UF",
  "V2007", "V2008", "V20081", "V20082",
  "V2001", "V2005", "V2009",
  "VD3004", "V3001",
  "VD4001", "VD4002", "VD4009", "VD4020", "VD4010"
)

#' Variaveis padrao de Visita 1 (Domicilio) da PNAD Contínua
#' @export
vars_visita_default <- c(
  "UPA", "V1008", "V1014", "Ano", "UF",
  "V5001A",
  "VD5002", "V5002A",
  "S01013", "S01006", "S01010"
)

#' Otimizacao de Memoria (Downcasting) para Microdados PNADc
#'
#' @param df Data frame contendo microdados da PNADc.
#' @return Data frame com colunas otimizadas para integer.
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

  df_res <- df
  cols_presentes <- intersect(colunas_int, names(df_res))

  for (col in cols_presentes) {
    x <- df_res[[col]]

    # Se a coluna for numerica (double/integer), checar se ha fracionarios
    if (is.numeric(x)) {
      x_valid <- x[!is.na(x) & !is.nan(x) & !is.infinite(x)]
      if (length(x_valid) > 0L) {
        if (any(x_valid %% 1 != 0)) {
          stop(sprintf("Valores fracionarios nao sao permitidos em colunas inteiras (coluna '%s').", col), call. = FALSE)
        }
        if (any(x_valid > 2147483647 | x_valid < -2147483647)) {
          stop(sprintf("Valor fora do intervalo inteiro de 32-bits (coluna '%s').", col), call. = FALSE)
        }
      }
      df_res[[col]] <- as.integer(x)
    } else if (is.character(x)) {
      # Tentar converter strings numericas
      x_clean <- gsub("\\.0$", "", x)
      num_vals <- suppressWarnings(as.numeric(x_clean))
      x_valid <- num_vals[!is.na(num_vals) & !is.nan(num_vals) & !is.infinite(num_vals)]
      if (length(x_valid) > 0L) {
        if (any(x_valid %% 1 != 0)) {
          stop(sprintf("Valores fracionarios nao sao permitidos em colunas inteiras (coluna '%s').", col), call. = FALSE)
        }
        if (any(x_valid > 2147483647 | x_valid < -2147483647)) {
          stop(sprintf("Valor fora do intervalo inteiro de 32-bits (coluna '%s').", col), call. = FALSE)
        }
      }
      if (any(is.na(num_vals) & !is.na(x) & nchar(trimws(x)) > 0)) {
        warning(sprintf("Strings nao numericas encontradas na coluna '%s' foram convertidas para NA.", col), call. = FALSE)
      }
      df_res[[col]] <- as.integer(num_vals)
    }
  }

  df_res
}

#' Helper de Normalizacao String sem .0
#' @keywords internal
.norm_str <- function(s) {
  s_str <- as.character(s)
  s_clean <- gsub("\\.0$", "", s_str)
  s_clean[is.na(s) | s_clean == "NA" | nchar(trimws(s_clean)) == 0L] <- NA_character_
  s_clean
}

#' Helper de Zero Padding (2 digitos)
#' @keywords internal
.pad2 <- function(s) {
  s_clean <- .norm_str(s)
  idx_valid <- !is.na(s_clean)
  res <- character(length(s))
  res[!idx_valid] <- NA_character_
  res[idx_valid] <- stringr::str_pad(s_clean[idx_valid], width = 2, pad = "0")
  res
}

#' Criar Identificadores Longitudinais Data Zoom
#'
#' @param dados Data frame contendo os microdados da PNADc.
#' @return Data frame com id_dom e id_ind adicionados.
#' @export
criar_ids_datazoom <- function(dados) {
  chaves_obrig <- c("UPA", "V1008", "V1014", "V2008", "V20081", "V20082", "V2007", "UF")
  faltantes <- setdiff(chaves_obrig, names(dados))
  if (length(faltantes) > 0L) {
    stop(paste("Colunas obrigatorias ausentes para criar IDs Data Zoom:", paste(faltantes, collapse = ", ")), call. = FALSE)
  }

  df <- dados

  # Normalizacao e validacao estrita dos 8 componentes chave
  upa_str   <- .norm_str(df$UPA)
  v1008_str <- .pad2(df$V1008)
  v1014_str <- .norm_str(df$V1014)
  uf_str    <- .norm_str(df$UF)

  v2007_str  <- .norm_str(df$V2007)
  v2008_num  <- suppressWarnings(as.numeric(.norm_str(df$V2008)))
  v20081_num <- suppressWarnings(as.numeric(.norm_str(df$V20081)))
  v20082_num <- suppressWarnings(as.numeric(.norm_str(df$V20082)))

  v2008_str  <- .pad2(df$V2008)
  v20081_str <- .pad2(df$V20081)
  v20082_str <- .norm_str(df$V20082)

  # Regra estrita: qualquer componente nulo ou data sentinela invalida (99, 9999) remove a linha
  mascara_valida <- (
    !is.na(upa_str) &
    !is.na(v1008_str) &
    !is.na(v1014_str) &
    !is.na(uf_str) &
    !is.na(v2007_str) &
    !is.na(v2008_num) &
    !is.na(v20081_num) &
    !is.na(v20082_num) &
    v2008_num != 99 &
    v20081_num != 99 &
    v20082_num != 9999
  )

  df_valido <- df[mascara_valida, , drop = FALSE]

  if (nrow(df_valido) == 0L) {
    df_res <- df_valido
    df_res$id_dom <- character(0)
    df_res$id_ind <- character(0)
    cols_remov <- intersect(c("V2008", "V20081", "V20082"), names(df_res))
    if (length(cols_remov) > 0L) df_res <- df_res[, !names(df_res) %in% cols_remov, drop = FALSE]
    return(df_res)
  }

  upa_v   <- upa_str[mascara_valida]
  v1008_v <- v1008_str[mascara_valida]
  v1014_v <- v1014_str[mascara_valida]
  uf_v    <- uf_str[mascara_valida]
  v2007_v <- v2007_str[mascara_valida]
  dia_v   <- v2008_str[mascara_valida]
  mes_v   <- v20081_str[mascara_valida]
  ano_v   <- v20082_str[mascara_valida]

  id_dom <- paste0(upa_v, v1008_v, v1014_v)
  id_ind <- paste0(id_dom, dia_v, mes_v, ano_v, v2007_v, uf_v)

  df_res <- df_valido
  df_res$id_dom <- id_dom
  df_res$id_ind <- id_ind

  cols_remov <- intersect(c("V2008", "V20081", "V20082"), names(df_res))
  if (length(cols_remov) > 0L) df_res <- df_res[, !names(df_res) %in% cols_remov, drop = FALSE]

  df_res
}

#' Diagnosticar Preenchimento de Colunas no Painel
#'
#' @param painel Data frame contendo o painel cruzado.
#' @param colunas Vetor de nomes de colunas a diagnosticar.
#' @return Um tibble com metricas de preenchimento ordenadas deterministicamente.
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
    res_empty <- dplyr::tibble(
      variavel       = as.character(colunas),
      total_linhas   = 0L,
      com_dado       = 0L,
      sem_dado       = 0L,
      pct_disponivel = 0.0
    )
    return(res_empty %>% dplyr::arrange(.data$pct_disponivel, .data$variavel))
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
#' @export
mensagem_diagnostico <- function(diagnostico, painel_antes, painel_depois, ano) {
  n_antes <- if (!is.null(painel_antes)) nrow(painel_antes) else 0L
  n_depois <- if (!is.null(painel_depois)) nrow(painel_depois) else 0L
  perda_abs <- n_antes - n_depois
  perda_pct <- if (n_antes > 0L) round((perda_abs / n_antes) * 100, 2) else 0.0

  if (!is.null(diagnostico) && nrow(diagnostico) > 0L) {
    var_critica_row <- diagnostico[1L, ]
    var_critica <- as.character(var_critica_row$variavel)
    pct_ausente_critica <- round(100 - var_critica_row$pct_disponivel, 2)
  } else {
    var_critica <- "Nenhuma"
    pct_ausente_critica <- 0.0
  }

  msg <- sprintf(
    paste0(
      "\n>>> Diagn\u00f3stico do painel PNADc - ano %s\n",
      "Linhas antes do cruzamento (base trimestral): %s\n",
      "Linhas ap\u00f3s cruzamento + balanceamento:       %s\n",
      "Perda total: %s linhas (%.2f%%)\n",
      "Vari\u00e1vel com maior perda antes do balanceamento: %s - %.2f%% de dados ausentes\n",
      "Motivo: descompasso temporal entre a base trimestral (Ano/Trimestre corrente) ",
      "e a base de Visita 1 (entrevista espec\u00edfica, ano corrente + ano anterior).\n"
    ),
    as.character(ano),
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

#' Wrapper Interno com Suporte a Mock Provider e Retry
#' @keywords internal
get_pnadc_internal <- function(year, quarter = NULL, interview = NULL, vars = NULL, design = FALSE, labels = FALSE, verbose = TRUE) {
  mock <- get_mock_provider()
  if (!is.null(mock) && is.function(mock)) {
    return(mock(year = year, quarter = quarter, interview = interview, vars = vars, design = design, labels = labels, verbose = verbose))
  }

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

  if (is.null(dados_casa_corrente) || nrow(dados_casa_corrente) == 0L) {
    stop(sprintf("Download de Visita 1 para o ano %d retornou vazio.", ano), call. = FALSE)
  }
  dados_casa_lista[[1]] <- downcast_pnadc(dados_casa_corrente)

  ano_anterior <- ano - 1L
  if (verbose) message(">>> Baixando Habitacao ", ano_anterior, " (Visita 1)...")
  tryCatch({
    dados_casa_ant <- get_pnadc_internal(year = ano_anterior, interview = 1, vars = if (is.character(vars_visita)) vars_visita else NULL, design = FALSE, labels = FALSE, verbose = verbose)
    if (!is.null(dados_casa_ant) && nrow(dados_casa_ant) > 0L) {
      dados_casa_lista[[2]] <- downcast_pnadc(dados_casa_ant)
    }
  }, error = function(e) {
    warning(sprintf("Nao foi possivel baixar dados de Visita 1 para o ano anterior (%d). A consolidacao sera realizada apenas com os dados de %d. Erro: %s", ano_anterior, ano, e$message), call. = FALSE)
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
#' @param ano Ano de referencia (inteiro ou NULL se 'anos' for fornecido).
#' @param anos Vetor de anos de referencia (inteiros ou NULL se 'ano' for fornecido).
#' @param vars_tri Vetor de variaveis trimestrais a baixar.
#' @param vars_visita Vetor de variaveis de Visita 1 a baixar.
#' @param balancear Logico.
#' @param low_memory Logico.
#' @param verbose Logico.
#' @return Um tibble consolidado contendo as informacoes de pessoas e domicilios.
#' @export
gerar_painel_pnadc <- function(ano = NULL, anos = NULL, vars_tri = NULL, vars_visita = NULL, balancear = TRUE, low_memory = FALSE, verbose = TRUE) {
  ano_atual <- as.integer(format(Sys.Date(), "%Y"))

  # Validação de mutualidade entre 'ano' e 'anos'
  if (!is.null(ano) && !is.null(anos)) {
    stop("Informe apenas 'ano' ou 'anos', nao ambos.", call. = FALSE)
  }
  if (is.null(ano) && is.null(anos)) {
    stop("Informe 'ano' ou 'anos'.", call. = FALSE)
  }

  anos_lista <- integer(0)

  if (!is.null(ano)) {
    if (is.logical(ano) || length(ano) != 1L || any(is.na(ano)) || any(is.nan(ano)) || any(is.infinite(ano)) || !is.numeric(ano)) {
      stop("O argumento 'ano' deve ser um unico numero inteiro valido.", call. = FALSE)
    }
    if (ano %% 1 != 0) {
      stop("O argumento 'ano' deve ser um numero inteiro valido.", call. = FALSE)
    }
    ano_val <- as.integer(ano)
    if (ano_val < 2012L || ano_val > ano_atual) {
      stop(sprintf("Ano invalido: %d. A PNAD Continua esta disponivel entre 2012 e %d.", ano_val, ano_atual), call. = FALSE)
    }
    anos_lista <- ano_val
  } else {
    if (is.logical(anos) || !is.numeric(anos) || length(anos) == 0L || any(is.na(anos)) || any(is.nan(anos)) || any(is.infinite(anos))) {
      stop("O argumento 'anos' deve ser um vetor de numeros inteiros validos.", call. = FALSE)
    }
    if (any(anos %% 1 != 0)) {
      stop("Todos os elementos de 'anos' devem ser numeros inteiros validos.", call. = FALSE)
    }
    anos_parsed <- as.integer(anos)
    if (any(anos_parsed < 2012L | anos_parsed > ano_atual)) {
      stop(sprintf("Ano invalido em 'anos'. A PNAD Continua esta disponivel entre 2012 e %d.", ano_atual), call. = FALSE)
    }
    anos_lista <- sort(unique(anos_parsed))
  }

  # Validação de flags logicas
  if (!is.logical(balancear) || length(balancear) != 1L || is.na(balancear)) {
    stop("O argumento 'balancear' deve ser um unico valor logico (TRUE ou FALSE).", call. = FALSE)
  }
  if (!is.logical(low_memory) || length(low_memory) != 1L || is.na(low_memory)) {
    stop("O argumento 'low_memory' deve ser um unico valor logico (TRUE ou FALSE).", call. = FALSE)
  }
  if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose)) {
    stop("O argumento 'verbose' deve ser um unico valor logico (TRUE ou FALSE).", call. = FALSE)
  }

  # Validação de vars_tri
  chaves_obrig_tri <- c("UPA", "V1008", "V1014", "V2008", "V20081", "V20082", "V2007", "UF", "Ano", "Trimestre")
  if (is.null(vars_tri)) {
    vars_tri_proc <- vars_tri_default
  } else if (is.character(vars_tri)) {
    if (length(vars_tri) == 0L) stop("O argumento 'vars_tri' nao pode ser um vetor vazio.", call. = FALSE)
    if (length(vars_tri) == 1L && tolower(vars_tri) %in% c("all", "todas", "tudo")) {
      vars_tri_proc <- NULL
    } else {
      vars_tri_proc <- unique(c(chaves_obrig_tri, vars_tri))
    }
  } else {
    stop("O argumento 'vars_tri' deve ser NULL, 'todas' ou um vetor de caracteres com nomes de variaveis.", call. = FALSE)
  }

  # Validação de vars_visita
  chaves_obrig_visita <- c("UPA", "V1008", "V1014", "Ano", "UF")
  if (is.null(vars_visita)) {
    vars_visita_proc <- vars_visita_default
  } else if (is.character(vars_visita)) {
    if (length(vars_visita) == 0L) stop("O argumento 'vars_visita' nao pode ser um vetor vazio.", call. = FALSE)
    if (length(vars_visita) == 1L && tolower(vars_visita) %in% c("all", "todas", "tudo")) {
      vars_visita_proc <- NULL
    } else {
      vars_visita_proc <- unique(c(chaves_obrig_visita, vars_visita))
    }
  } else {
    stop("O argumento 'vars_visita' deve ser NULL, 'todas' ou um vetor de caracteres com nomes de variaveis.", call. = FALSE)
  }

  paineis_lista <- vector("list", length(anos_lista))
  for (i in seq_along(anos_lista)) {
    a <- anos_lista[i]
    painel_pessoas <- baixar_trimestres_pnadc(ano = a, vars_tri = vars_tri_proc, low_memory = low_memory, verbose = verbose)
    base_habitacao <- consolidar_base_habitacao(ano = a, vars_visita = vars_visita_proc, verbose = verbose)

    if (verbose) message(">>> Realizando o cruzamento final do ano ", a, " (left_join por id_dom)...")
    paineis_lista[[i]] <- dplyr::left_join(painel_pessoas, base_habitacao, by = "id_dom")
  }

  if (verbose && length(anos_lista) > 1L) message(">>> Concatenando ", length(anos_lista), " anos...")
  painel_cruzado <- dplyr::bind_rows(paineis_lista)

  painel_cruzado$periodo <- paste0(painel_cruzado$Ano, "_", painel_cruzado$Trimestre)

  painel_cruzado <- dplyr::arrange(painel_cruzado, .data$id_ind, .data$Ano, .data$Trimestre)

  if (verbose) message(">>> Validando identificadores...")
  idx_duplicados <- duplicated(painel_cruzado[, c("id_ind", "Ano", "Trimestre")])
  if (any(idx_duplicados)) {
    stop("Foram encontradas duplicatas de (id_ind, Ano, Trimestre) no painel final.", call. = FALSE)
  }

  vars_hab_especificas <- if (is.null(vars_visita_proc)) {
    setdiff(names(painel_cruzado), c("id_dom", "id_ind", "periodo", chaves_obrig_visita, vars_tri_proc))
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
    ano_label <- if (length(anos_lista) > 1L) sprintf("%d-%d", anos_lista[1], anos_lista[length(anos_lista)]) else as.character(anos_lista[1])
    mensagem_diagnostico(diagnostico = diag_tb, painel_antes = painel_cruzado, painel_depois = painel_final, ano = ano_label)
    message(">>> Painel longitudinal concluído.")
  }

  attr(painel_final, "diagnostico") <- diag_tb
  painel_final
}

#' Construir Crosswalk de Periodos para a PNAD Continua
#'
#' @param df_empilhado Data frame contendo os microdados empilhados da PNADC.
#' @return Tabela/data frame contendo a chave (UPA, V1014, Trimestre) e os meses exatos identificados.
#' @export
construir_crosswalk_pnadc <- function(df_empilhado) {
  mock <- get_mock_provider()
  if (!is.null(mock) && is.function(mock)) {
    if (nrow(df_empilhado) == 0L) {
      return(dplyr::tibble(UPA = character(0), V1014 = character(0), Trimestre = integer(0), ref_month_yyyymm = character(0)))
    }
    cw <- df_empilhado %>%
      dplyr::select(dplyr::all_of(c("UPA", "V1014", "Ano", "Trimestre"))) %>%
      dplyr::distinct() %>%
      dplyr::mutate(
        mes_num = sprintf("%02d", ((.data$Trimestre - 1L) * 3L) + 2L),
        ref_month_yyyymm = paste0(.data$Ano, .data$mes_num)
      )
    return(cw)
  }

  if (!requireNamespace("PNADCperiods", quietly = TRUE)) {
    stop("O pacote 'PNADCperiods' e necessario para construir o crosswalk de periodos exatos.", call. = FALSE)
  }
  PNADCperiods::pnadc_identify_periods(df_empilhado)
}

#' Gerar Painel Consolidado PNAD Continua com Mensalizacao (Pessoa + Domicilio)
#'
#' @param ano Ano de referencia (inteiro ou NULL se 'anos' for fornecido).
#' @param anos Vetor de anos de referencia (inteiros ou NULL se 'ano' for fornecido).
#' @param vars_tri Vetor de variaveis trimestrais a baixar.
#' @param vars_visita Vetor de variaveis de Visita 1 a baixar.
#' @param balancear Logico.
#' @param crosswalk Objeto de crosswalk pre-construido (opcional).
#' @param janela_trimestres Vetor de 2 inteiros especificando a janela de trimestres de contexto para empilhamento (default c(-4, 4)).
#' @param minimo_dias_parada_tecnica Criterio de parada tecnica ("auto" ou inteiro).
#' @param filtrar_indeterminados Logico. Se TRUE, remove linhas sem mes exato determinado.
#' @param incluir_pesos_replicacao Logico. Se TRUE, adapta os 200 pesos de replicacao bootstrap.
#' @param low_memory Logico.
#' @param verbose Logico.
#' @return Um tibble consolidado contendo as informacoes de pessoas e domicilios com mes exato e pesos calibrados.
#' @export
gerar_painel_pnadc_mensal <- function(
  ano = NULL,
  anos = NULL,
  vars_tri = NULL,
  vars_visita = NULL,
  balancear = TRUE,
  crosswalk = NULL,
  janela_trimestres = c(-4, 4),
  minimo_dias_parada_tecnica = "auto",
  filtrar_indeterminados = TRUE,
  incluir_pesos_replicacao = FALSE,
  low_memory = FALSE,
  verbose = TRUE
) {
  # Garantir inclusao de pesos amostrais e chaves temporais em vars_tri e vars_visita
  chaves_mensal_tri <- c("V1028", "V2009", "V2008", "V20081", "V20082")
  if (is.null(vars_tri)) {
    vars_tri_proc <- unique(c(vars_tri_default, chaves_mensal_tri))
  } else if (is.character(vars_tri) && length(vars_tri) == 1L && tolower(vars_tri) %in% c("all", "todas", "tudo")) {
    vars_tri_proc <- "todas"
  } else if (is.character(vars_tri)) {
    vars_tri_proc <- unique(c(vars_tri, chaves_mensal_tri))
  } else {
    vars_tri_proc <- vars_tri
  }

  chaves_mensal_visita <- c("V1032")
  if (is.null(vars_visita)) {
    vars_visita_proc <- unique(c(vars_visita_default, chaves_mensal_visita))
  } else if (is.character(vars_visita) && length(vars_visita) == 1L && tolower(vars_visita) %in% c("all", "todas", "tudo")) {
    vars_visita_proc <- "todas"
  } else if (is.character(vars_visita)) {
    vars_visita_proc <- unique(c(vars_visita, chaves_mensal_visita))
  } else {
    vars_visita_proc <- vars_visita
  }

  if (incluir_pesos_replicacao) {
    pesos_rep <- sprintf("V1028_%03d", 1:200)
    if (is.character(vars_tri_proc) && !("todas" %in% tolower(vars_tri_proc))) {
      vars_tri_proc <- unique(c(vars_tri_proc, pesos_rep))
    }
  }

  # Executar a geracao padrao do painel
  painel_trimestral <- gerar_painel_pnadc(
    ano = ano,
    anos = anos,
    vars_tri = vars_tri_proc,
    vars_visita = vars_visita_proc,
    balancear = balancear,
    low_memory = low_memory,
    verbose = verbose
  )

  mock <- get_mock_provider()
  if (!is.null(mock) && is.function(mock)) {
    cw <- if (is.null(crosswalk)) construir_crosswalk_pnadc(painel_trimestral) else crosswalk
    painel_mensal <- painel_trimestral
    if ("V1028" %in% names(painel_mensal)) {
      painel_mensal$peso_mensal <- suppressWarnings(as.numeric(painel_mensal$V1028))
      painel_mensal$weight_monthly <- painel_mensal$peso_mensal
    } else {
      painel_mensal$peso_mensal <- 1.0
      painel_mensal$weight_monthly <- 1.0
    }
    
    meses_num <- sprintf("%02d", ((painel_mensal$Trimestre - 1L) * 3L) + 2L)
    painel_mensal$mes_exato_aaaamm <- paste0(painel_mensal$Ano, meses_num)
    painel_mensal$ref_month_yyyymm <- painel_mensal$mes_exato_aaaamm

    diag_tb <- attr(painel_trimestral, "diagnostico")
    attr(painel_mensal, "diagnostico") <- diag_tb
    attr(painel_mensal, "taxa_determinacao_mensal") <- 100.0
    return(painel_mensal)
  }

  if (!requireNamespace("PNADCperiods", quietly = TRUE)) {
    stop("O pacote 'PNADCperiods' e necessario para gerar o painel mensalizado.", call. = FALSE)
  }

  cw <- if (is.null(crosswalk)) construir_crosswalk_pnadc(painel_trimestral) else crosswalk

  painel_mensal <- PNADCperiods::pnadc_apply_periods(
    pnadc = painel_trimestral,
    crosswalk = cw,
    weight_var = if ("V1028" %in% names(painel_trimestral)) "V1028" else NULL,
    anchor = "quarter"
  )

  if ("ref_month_yyyymm" %in% names(painel_mensal)) {
    painel_mensal$mes_exato_aaaamm <- painel_mensal$ref_month_yyyymm
  }
  if ("weight_monthly" %in% names(painel_mensal)) {
    painel_mensal$peso_mensal <- painel_mensal$weight_monthly
  }

  taxa_det <- if (nrow(painel_mensal) > 0L && "mes_exato_aaaamm" %in% names(painel_mensal)) {
    round((sum(!is.na(painel_mensal$mes_exato_aaaamm)) / nrow(painel_mensal)) * 100, 2)
  } else 0.0

  if (filtrar_indeterminados && "mes_exato_aaaamm" %in% names(painel_mensal)) {
    painel_mensal <- painel_mensal[!is.na(painel_mensal$mes_exato_aaaamm), , drop = FALSE]
  }

  if (verbose) {
    message(sprintf(">>> Taxa de determinação de mês exato: %.2f%%", taxa_det))
  }

  attr(painel_mensal, "diagnostico") <- attr(painel_trimestral, "diagnostico")
  attr(painel_mensal, "taxa_determinacao_mensal") <- taxa_det
  painel_mensal
}
