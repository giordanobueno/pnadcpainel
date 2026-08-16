#' Gerar Painel Consolidado PNAD Continua (Pessoa + Domicilio)
#'
#' Baixa e cruza a base trimestral de pessoas com a base de Visita 1 de domicilios
#' da PNAD Continua para um determinado ano, identificando domicilios e individuos
#' com a metodologia Data Zoom (PUC-Rio).
#'
#' @param ano Ano de referencia (inteiro). Deve estar entre 2012 e o ano atual.
#' @param vars_tri Vetor de variaveis trimestrais a baixar. Se NULL (padrao),
#'   utiliza \code{vars_tri_default}. Se \code{"todas"}, \code{"all"} ou \code{"tudo"},
#'   baixa todas as colunas da PNADc trimestral.
#' @param vars_visita Vetor de variaveis de Visita 1 a baixar. Se NULL (padrao),
#'   utiliza \code{vars_visita_default}. Se \code{"todas"}, \code{"all"} ou \code{"tudo"},
#'   baixa todas as colunas de Visita 1.
#' @param balancear Logico. Se TRUE (padrao), remove linhas do painel onde qualquer
#'   variavel oriunda de Visita 1 selecionada esteja com NA, garantindo painel retangular.
#' @param low_memory Logico. Se TRUE, grava intermediarios trimestrais em disco temporario.
#' @param verbose Logico. Se TRUE (padrao), exibe mensagens informativas e de diagnostico.
#'
#' @return Um tibble consolidado contendo as informacoes de pessoas e domicilios.
#'   O objeto possui o atributo \code{"diagnostico"} contendo a tabela de preenchimento.
#' @importFrom dplyr left_join filter if_all all_of
#' @export
#'
#' @examples
#' \dontrun{
#' painel_2023 <- gerar_painel_pnadc(ano = 2023)
#' }
gerar_painel_pnadc <- function(ano,
                               vars_tri = NULL,
                               vars_visita = NULL,
                               balancear = TRUE,
                               low_memory = FALSE,
                               verbose = TRUE) {
  # 1. Validacao estrita de argumentos de entrada
  ano_atual <- as.integer(format(Sys.Date(), "%Y"))
  if (missing(ano) || is.null(ano) || length(ano) != 1L || any(is.na(ano)) ||
      any(is.nan(ano)) || any(is.infinite(ano)) || !is.numeric(ano)) {
    stop("O argumento 'ano' deve ser um unico numero inteiro valido.", call. = FALSE)
  }
  if (ano != suppressWarnings(as.integer(ano))) {
    stop("O argumento 'ano' deve ser um numero inteiro valido.", call. = FALSE)
  }
  ano <- as.integer(ano)
  if (ano < 2012L || ano > ano_atual) {
    stop(sprintf(
      "Ano invalido: %d. A PNAD Continua esta disponivel entre 2012 e %d.",
      ano, ano_atual
    ), call. = FALSE)
  }

  if (!is.logical(balancear) || length(balancear) != 1L || is.na(balancear)) {
    stop("O argumento 'balancear' deve ser um unico valor logico (TRUE ou FALSE).", call. = FALSE)
  }
  if (!is.logical(low_memory) || length(low_memory) != 1L || is.na(low_memory)) {
    stop("O argumento 'low_memory' deve ser um unico valor logico (TRUE ou FALSE).", call. = FALSE)
  }
  if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose)) {
    stop("O argumento 'verbose' deve ser um unico valor logico (TRUE ou FALSE).", call. = FALSE)
  }

  # Tratar e validar vars_tri
  chaves_obrig_tri <- c("UPA", "V1008", "V1014", "V2008", "V20081", "V20082", "V2007", "UF", "Ano", "Trimestre")
  if (is.null(vars_tri)) {
    vars_tri_proc <- vars_tri_default
  } else if (is.character(vars_tri)) {
    if (length(vars_tri) == 0L) {
      stop("O argumento 'vars_tri' nao pode ser um vetor vazio.", call. = FALSE)
    }
    if (length(vars_tri) == 1L && tolower(vars_tri) %in% c("all", "todas", "tudo")) {
      vars_tri_proc <- NULL
    } else {
      vars_tri_proc <- unique(c(chaves_obrig_tri, vars_tri))
    }
  } else {
    msg_err <- "O argumento 'vars_tri' deve ser NULL, 'todas' ou um vetor de caracteres com nomes de variaveis."
    stop(msg_err, call. = FALSE)
  }

  # Tratar e validar vars_visita
  chaves_obrig_visita <- c("UPA", "V1008", "V1014", "Ano", "UF")
  if (is.null(vars_visita)) {
    vars_visita_proc <- vars_visita_default
  } else if (is.character(vars_visita)) {
    if (length(vars_visita) == 0L) {
      stop("O argumento 'vars_visita' nao pode ser um vetor vazio.", call. = FALSE)
    }
    if (length(vars_visita) == 1L && tolower(vars_visita) %in% c("all", "todas", "tudo")) {
      vars_visita_proc <- NULL
    } else {
      vars_visita_proc <- unique(c(chaves_obrig_visita, vars_visita))
    }
  } else {
    msg_err <- "O argumento 'vars_visita' deve ser NULL, 'todas' ou um vetor de caracteres com nomes de variaveis."
    stop(msg_err, call. = FALSE)
  }

  # 2. Processar base trimestral
  painel_pessoas <- baixar_trimestres_pnadc(
    ano = ano,
    vars_tri = vars_tri_proc,
    low_memory = low_memory,
    verbose = verbose
  )

  # 3. Processar base de habitacao (Visita 1)
  base_habitacao <- consolidar_base_habitacao(
    ano = ano,
    vars_visita = vars_visita_proc,
    verbose = verbose
  )

  # 4. Cruzamento (left_join por id_dom)
  if (verbose) {
    message(">>> Realizando o cruzamento final (left_join por id_dom)...")
  }
  painel_cruzado <- dplyr::left_join(painel_pessoas, base_habitacao, by = "id_dom")
  rm(painel_pessoas, base_habitacao)
  gc()

  # 5. Diagnostico de preenchimento
  if (is.null(vars_visita_proc)) {
    cols_excluir <- c("id_dom", "id_ind", chaves_obrig_visita, if (is.null(vars_tri_proc)) NULL else vars_tri_proc)
    vars_hab_especificas <- setdiff(names(painel_cruzado), cols_excluir)
  } else {
    vars_hab_especificas <- setdiff(vars_visita_proc, chaves_obrig_visita)
  }
  vars_hab_especificas <- intersect(vars_hab_especificas, names(painel_cruzado))

  diag_tb <- diagnosticar_painel(painel_cruzado, colunas = vars_hab_especificas)

  # 6. Balanceamento do painel
  if (balancear && length(vars_hab_especificas) > 0L) {
    painel_final <- painel_cruzado %>%
      dplyr::filter(dplyr::if_all(dplyr::all_of(vars_hab_especificas), ~ !is.na(.)))
  } else {
    painel_final <- painel_cruzado
  }

  # Emissao de mensagem de diagnostico
  if (verbose && nrow(diag_tb) > 0L) {
    mensagem_diagnostico(
      diagnostico = diag_tb,
      painel_antes = painel_cruzado,
      painel_depois = painel_final,
      ano = ano
    )
  }

  # Anexar atributo de diagnostico
  attr(painel_final, "diagnostico") <- diag_tb

  painel_final
}
