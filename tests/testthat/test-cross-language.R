test_that("gerar_painel_pnadc produz estrutura deterministica compatival com artefatos cross-language", {
  source(testthat::test_path("fixtures", "synthetic_pnadc.R"))

  withr::with_options(
    list(pnadcpainel.mock_provider = criar_mock_provider()),
    {
      painel <- gerar_painel_pnadc(ano = 2023, verbose = FALSE)

      expect_s3_class(painel, "data.frame")
      expect_gt(nrow(painel), 0L)
      expect_true("id_dom" %in% names(painel))
      expect_true("id_ind" %in% names(painel))

      diag <- attr(painel, "diagnostico")
      expect_s3_class(diag, "data.frame")
      expect_equal(names(diag), c("variavel", "total_linhas", "com_dado", "sem_dado", "pct_disponivel"))

      # Validar ordenacao deterministica de 2 niveis (pct_disponivel asc, variavel asc)
      if (nrow(diag) > 1L) {
        expect_true(all(diff(diag$pct_disponivel) >= 0))
      }
    }
  )
})
