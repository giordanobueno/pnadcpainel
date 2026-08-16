source(testthat::test_path("fixtures", "synthetic_pnadc.R"))

test_that("gerar_painel_pnadc executa pipeline completo offline com mock provider", {
  withr::with_options(
    list(pnadcpainel.mock_provider = criar_mock_provider()),
    {
      painel <- gerar_painel_pnadc(ano = 2023, verbose = FALSE)

      expect_s3_class(painel, "data.frame")
      expect_gt(nrow(painel), 0)
      expect_true("id_dom" %in% names(painel))
      expect_true("id_ind" %in% names(painel))
      expect_true(!is.null(attr(painel, "diagnostico")))

      diag <- attr(painel, "diagnostico")
      expect_s3_class(diag, "data.frame")
      expect_true(all(c("variavel", "total_linhas", "com_dado", "sem_dado", "pct_disponivel") %in% names(diag)))
    }
  )
})

test_that("gerar_painel_pnadc aceita vars_tri e vars_visita como 'todas' e 'all'", {
  withr::with_options(
    list(pnadcpainel.mock_provider = criar_mock_provider()),
    {
      painel_todas <- gerar_painel_pnadc(ano = 2023, vars_tri = "todas", vars_visita = "todas", verbose = FALSE)
      expect_s3_class(painel_todas, "data.frame")
      expect_gt(nrow(painel_todas), 0)

      painel_all <- gerar_painel_pnadc(ano = 2023, vars_tri = "all", vars_visita = "all", verbose = FALSE)
      expect_s3_class(painel_all, "data.frame")
      expect_gt(nrow(painel_all), 0)
    }
  )
})

test_that("low_memory = TRUE e FALSE geram resultados semanticamente equivalentes", {
  withr::with_options(
    list(pnadcpainel.mock_provider = criar_mock_provider()),
    {
      p_mem <- gerar_painel_pnadc(ano = 2023, low_memory = FALSE, verbose = FALSE)
      p_low <- gerar_painel_pnadc(ano = 2023, low_memory = TRUE, verbose = FALSE)

      expect_equal(nrow(p_mem), nrow(p_low))
      expect_equal(names(p_mem), names(p_low))
      expect_equal(p_mem$id_ind, p_low$id_ind)
    }
  )
})

test_that("falha do ano anterior gera warning nao fatal e falha do corrente gera erro", {
  mock_falha_anterior <- function(year, quarter = NULL, interview = NULL, vars = NULL, design = FALSE, labels = FALSE) {
    if (!is.null(interview) && year == 2022) {
      stop("Servidor IBGE indisponivel para 2022.")
    }
    criar_mock_provider()(year, quarter, interview, vars, design, labels)
  }

  withr::with_options(
    list(pnadcpainel.mock_provider = mock_falha_anterior),
    {
      expect_warning(
        p <- gerar_painel_pnadc(ano = 2023, verbose = FALSE),
        "Nao foi possivel baixar dados de Visita 1 para o ano anterior"
      )
      expect_s3_class(p, "data.frame")
    }
  )

  mock_falha_corrente <- function(year, quarter = NULL, interview = NULL, vars = NULL, design = FALSE, labels = FALSE) {
    if (!is.null(interview) && year == 2023) {
      stop("Erro de conexao com IBGE.")
    }
    criar_mock_provider()(year, quarter, interview, vars, design, labels)
  }

  withr::with_options(
    list(pnadcpainel.mock_provider = mock_falha_corrente),
    {
      expect_error(
        gerar_painel_pnadc(ano = 2023, verbose = FALSE),
        "Falha ao baixar dados de Visita 1"
      )
    }
  )
})
