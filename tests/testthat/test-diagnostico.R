test_that("diagnosticar_painel calcula metricas corretamente", {
  df_test <- data.frame(
    id_dom = c("D1", "D2", "D3", "D4"),
    VD5002 = c(1000, 2000, NA, NA),
    S01013 = c(1, NA, 1, NA)
  )

  diag <- diagnosticar_painel(df_test, colunas = c("VD5002", "S01013"))

  expect_s3_class(diag, "tbl_df")
  expect_equal(names(diag), c("variavel", "total_linhas", "com_dado", "sem_dado", "pct_disponivel"))
  expect_equal(nrow(diag), 2)

  row_vd5002 <- diag[diag$variavel == "VD5002", ]
  expect_equal(row_vd5002$com_dado, 2)
  expect_equal(row_vd5002$sem_dado, 2)
  expect_equal(row_vd5002$pct_disponivel, 50.0)
})

test_that("mensagem_diagnostico produz formato correto", {
  df_test <- data.frame(
    VD5002 = c(1000, 2000, NA, NA),
    S01013 = c(1, NA, 1, NA)
  )
  diag <- diagnosticar_painel(df_test, colunas = c("VD5002", "S01013"))
  df_depois <- df_test[!is.na(df_test$VD5002) & !is.na(df_test$S01013), ]

  msg <- mensagem_diagnostico(diag, df_test, df_depois, ano = 2023)
  expect_true(grepl("Diagn\u00f3stico do painel PNADc - ano 2023", msg))
  expect_true(grepl("Linhas antes do cruzamento", msg))
  expect_true(grepl("descompasso temporal", msg))
})
