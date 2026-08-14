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

#' Baixar e consolidar os 4 trimestres de um determinado ano
#'
#' @param ano Ano de referencia (inteiro).
#' @param vars_tri Vetor de nomes de variaveis a serem baixadas.
#' @param low_memory Logico. Se TRUE, salva intermediarios em disco temporario.
#' @param verbose Logico. Se TRUE, exibe mensagens de progresso.
#' @return Data frame consolidado das pessoas nos 4 trimestres.
#' @keywords internal
baixar_trimestres_pnadc <- function(ano, vars_tri = vars_tri_default, low_memory = FALSE, verbose = TRUE) {
  if (!"package:PNADcIBGE" %in% search()) {
    suppressPackageStartupMessages(library(PNADcIBGE))
  }

  lista_painel <- vector("list", 4L)
  temp_files <- character(4L)

  for (tri in 1:4) {
    if (verbose) {
      message(">>> Processando Trimestre ", tri, " de ", ano, "...")
    }

    dados_brutos <- PNADcIBGE::get_pnadc(
      year    = ano,
      quarter = tri,
      vars    = vars_tri,
      design  = FALSE,
      labels  = FALSE
    )

    dados_brutos <- downcast_pnadc(dados_brutos)
    dados_proc <- criar_ids_datazoom(dados_brutos)

    rm(dados_brutos)
    gc()

    if (low_memory) {
      tpath <- file.path(tempdir(), paste0("pnadc_tri_", ano, "_", tri, ".rds"))
      saveRDS(dados_proc, tpath)
      temp_files[tri] <- tpath
      rm(dados_proc)
      gc()
    } else {
      lista_painel[[tri]] <- dados_proc
      rm(dados_proc)
      gc()
    }
  }

  if (low_memory) {
    res_list <- vector("list", 4L)
    for (tri in 1:4) {
      res_list[[tri]] <- readRDS(temp_files[tri])
      unlink(temp_files[tri])
    }
    painel_pessoas <- dplyr::bind_rows(res_list)
    rm(res_list)
    gc()
  } else {
    painel_pessoas <- dplyr::bind_rows(lista_painel)
    rm(lista_painel)
    gc()
  }

  return(painel_pessoas)
}
