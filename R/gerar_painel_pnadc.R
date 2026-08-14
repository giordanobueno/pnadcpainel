#' Gerar Painel Consolidado PNAD Continua (Pessoa + Domicilio)
#'
#' Baixa e cruza a base trimestral de pessoas com a base de Visita 1 de domicilios
#' da PNAD Continua para um determinado ano, identificando domicilios e individuos
#' com a metodologia Data Zoom (PUC-Rio).
#'
#' @param ano Ano de referencia (inteiro). Deve estar entre 2012 e o ano atual.
#' @param vars_tri Vetor opcional de variaveis trimestrais a baixar. Se NULL, utiliza \code{vars_tri_default}.
#' @param vars_visita Vetor opcional de variaveis de Visita 1 a baixar. Se NULL, utiliza \code{vars_visita_default}.
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
#' painel_2023 <- gerar_painel_pnadc(ano = 2023)
#' diag <- attr(painel_2023, "diagnostico")
#' print(diag)
#' }
gerar_painel_pnadc <- function(ano,
                               vars_tri = NULL,
                               vars_visita = NULL,
                               balancear = TRUE,
                               low_memory = FALSE,
                               verbose = TRUE) {
  # 1. Validacao de ano
  ano_atual <- as.integer(format(Sys.Date(), "%Y"))
  if (missing(ano) || is.null(ano) || !is.numeric(ano) || length(ano) != 1 || is.na(ano)) {
    stop("O argumento 'ano' deve ser um unico numero inteiro valido.")
  }
  ano <- as.integer(ano)
  if (ano < 2012 || ano > ano_atual) {
    stop(sprintf("Ano invalido: %d. A PNAD Continua esta disponivel entre 2012 e %d.", ano, ano_atual))
  }

  # Defaults
  if (is.null(vars_tri)) {
    vars_tri <- vars_tri_default
  }
  if (is.null(vars_visita)) {
    vars_visita <- vars_visita_default
  }

  # 2. Processar base trimestral
  painel_pessoas <- baixar_trimestres_pnadc(
    ano = ano,
    vars_tri = vars_tri,
    low_memory = low_memory,
    verbose = verbose
  )

  # 3. Processar base de habitacao (Visita 1)
  base_habitacao <- consolidar_base_habitacao(
    ano = ano,
    vars_visita = vars_visita,
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
  chaves_hab <- c("UPA", "V1008", "V1014", "Ano", "UF")
  vars_hab_especificas <- setdiff(vars_visita, chaves_hab)
  diag_tb <- diagnosticar_painel(painel_cruzado, colunas = vars_hab_especificas)

  # 6. Balanceamento do painel
  if (balancear) {
    painel_final <- painel_cruzado %>%
      dplyr::filter(dplyr::if_all(dplyr::all_of(vars_hab_especificas), ~ !is.na(.)))
  } else {
    painel_final <- painel_cruzado
  }

  # Emissao de mensagem de diagnostico
  if (verbose) {
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
