#' Gerar Painel Consolidado PNAD Continua (Pessoa + Domicilio)
#'
#' Baixa e cruza a base trimestral de pessoas com a base de Visita 1 de domicilios
#' da PNAD Continua para um determinado ano, identificando domicilios e individuos
#' com a metodologia Data Zoom (PUC-Rio).
#'
#' @param ano Ano de referencia (inteiro). Deve estar entre 2012 e o ano atual.
#' @param vars_tri Vetor de variaveis trimestrais a baixar. Se NULL (padrao), utiliza \code{vars_tri_default}. Se \code{"todas"} ou \code{"all"}, baixa todas as colunas da PNADc trimestral.
#' @param vars_visita Vetor de variaveis de Visita 1 a baixar. Se NULL (padrao), utiliza \code{vars_visita_default}. Se \code{"todas"} ou \code{"all"}, baixa todas as colunas de Visita 1.
#' @param balancear Logico. Se TRUE (padrao), remove linhas do painel onde qualquer variavel oriunda de \code{vars_visita} esteja com NA, garantindo um painel balanceado e retangular.
#' @param low_memory Logico. Se TRUE, grava intermediarios trimestrais em disco temporario para economizar memoria RAM.
#' @param verbose Logico. Se TRUE (padrao), exibe mensagens informativas e de diagnostico no console.
#'
#' @return Um tibble consolidado contendo as informacoes de pessoas e domicilios. O objeto possui o atributo \code{"diagnostico"} contendo a tabela de preenchimento.
#' @importFrom dplyr left_join filter if_all all_of
#' @export
#'
#' @examples
#' \dontrun{
#' # Uso padrao (variaveis essenciais)
#' painel_2023 <- gerar_painel_pnadc(ano = 2023)
#'
#' # Selecao customizada de variaveis
#' painel_custom <- gerar_painel_pnadc(
#'   ano = 2023,
#'   vars_tri = c("V2009", "VD4020"),
#'   vars_visita = c("VD5002", "S01006")
#' )
#'
#' # Baixar TODAS as variaveis dos microdados
#' painel_completo <- gerar_painel_pnadc(
#'   ano = 2023,
#'   vars_tri = "todas",
#'   vars_visita = "todas"
#' )
#' }
gerar_painel_pnadc <- function(ano,
                               vars_tri = NULL,
                               vars_visita = NULL,
                               balancear = TRUE,
                               low_memory = FALSE,
                               verbose = TRUE) {
  # Garantir que PNADcIBGE esteja anexado ao search path do R
  if (!"package:PNADcIBGE" %in% search()) {
    suppressPackageStartupMessages(library(PNADcIBGE))
  }

  # 1. Validacao de ano
  ano_atual <- as.integer(format(Sys.Date(), "%Y"))
  if (missing(ano) || is.null(ano) || !is.numeric(ano) || length(ano) != 1 || is.na(ano)) {
    stop("O argumento 'ano' deve ser um unico numero inteiro valido.")
  }
  ano <- as.integer(ano)
  if (ano < 2012 || ano > ano_atual) {
    stop(sprintf("Ano invalido: %d. A PNAD Continua esta disponivel entre 2012 e %d.", ano, ano_atual))
  }

  # Tratar vars_tri (garantir chaves de ID Data Zoom)
  chaves_obrig_tri <- c("UPA", "V1008", "V1014", "V2008", "V20081", "V20082", "V2007", "UF", "Ano", "Trimestre")
  if (is.null(vars_tri)) {
    vars_tri_proc <- vars_tri_default
  } else if (is.character(vars_tri) && length(vars_tri) == 1 && tolower(vars_tri) %in% c("all", "todas", "tudo")) {
    vars_tri_proc <- NULL # NULL indica para get_pnadc baixar todas as colunas
  } else {
    vars_tri_proc <- unique(c(chaves_obrig_tri, vars_tri))
  }

  # Tratar vars_visita (garantir chaves de id_dom)
  chaves_obrig_visita <- c("UPA", "V1008", "V1014", "Ano", "UF")
  if (is.null(vars_visita)) {
    vars_visita_proc <- vars_visita_default
  } else if (is.character(vars_visita) && length(vars_visita) == 1 && tolower(vars_visita) %in% c("all", "todas", "tudo")) {
    vars_visita_proc <- NULL # NULL indica para get_pnadc baixar todas as colunas
  } else {
    vars_visita_proc <- unique(c(chaves_obrig_visita, vars_visita))
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
  vars_hab_especificas <- setdiff(names(painel_cruzado), names(painel_cruzado)[1:ncol(painel_cruzado)])
  if (is.null(vars_visita_proc)) {
    # Todas as colunas vindas de Visita 1 exceto id_dom e chaves
    vars_hab_especificas <- setdiff(names(painel_cruzado), c(names(vars_tri_default), "id_dom", "id_ind", chaves_obrig_visita))
  } else {
    vars_hab_especificas <- setdiff(vars_visita_proc, chaves_obrig_visita)
  }
  vars_hab_especificas <- intersect(vars_hab_especificas, names(painel_cruzado))

  diag_tb <- diagnosticar_painel(painel_cruzado, colunas = vars_hab_especificas)

  # 6. Balanceamento do painel
  if (balancear && length(vars_hab_especificas) > 0) {
    painel_final <- painel_cruzado %>%
      dplyr::filter(dplyr::if_all(dplyr::all_of(vars_hab_especificas), ~ !is.na(.)))
  } else {
    painel_final <- painel_cruzado
  }

  # Emissao de mensagem de diagnostico
  if (verbose && nrow(diag_tb) > 0) {
    mensagem_diagnostico(
      diagnostico = diag_tb,
      painel_antes = painel_cruzado,
      painel_depois = painel_final,
      ano = ano
    )
  }

  # Anexar atributo de diagnostico
  attr(painel_final, "diagnostico") <- diag_tb

  return(painel_final)
}
