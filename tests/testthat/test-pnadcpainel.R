test_that("gerar_painel_pnadc valida o ano de entrada rigorosamente", {
  expect_error(gerar_painel_pnadc(), "deve ser um unico numero inteiro valido")
  expect_error(gerar_painel_pnadc(ano = NULL), "deve ser um unico numero inteiro valido")
  expect_error(gerar_painel_pnadc(ano = NA), "deve ser um unico numero inteiro valido")
  expect_error(gerar_painel_pnadc(ano = NaN), "deve ser um unico numero inteiro valido")
  expect_error(gerar_painel_pnadc(ano = Inf), "deve ser um unico numero inteiro valido")
  expect_error(gerar_painel_pnadc(ano = -Inf), "deve ser um unico numero inteiro valido")
  expect_error(gerar_painel_pnadc(ano = 2000), "Ano invalido")
  expect_error(gerar_painel_pnadc(ano = 2030), "Ano invalido")
  expect_error(gerar_painel_pnadc(ano = "invalido"), "deve ser um unico numero inteiro valido")
  expect_error(gerar_painel_pnadc(ano = c(2022, 2023)), "deve ser um unico numero inteiro valido")
  expect_error(gerar_painel_pnadc(ano = 2023.5), "deve ser um numero inteiro valido")
})

test_that("criar_ids_datazoom padroniza zero a esquerda em V1008 e V2008", {
  dados_mock <- dplyr::tibble(
    UPA    = c("110000016", "110000016"),
    V1008  = c(1, "01"),
    V1014  = c(10, 10),
    V2008  = c(5, 12),
    V20081 = c(8, 11),
    V20082 = c(1995, 1988),
    V2007  = c(1, 2),
    UF     = c("11", "11")
  )

  res <- criar_ids_datazoom(dados_mock)
  expect_equal(res$id_dom[1], "1100000160110")
  expect_equal(res$id_dom[2], "1100000160110")
  expect_equal(res$id_ind[1], "110000016011005081995111")
  expect_equal(res$id_ind[2], "110000016011012111988211")
})

test_that("diagnosticar_painel calcula metricas e ordena por preenchimento e nome", {
  df_test <- dplyr::tibble(
    id_dom = c("D1", "D2", "D3", "D4"),
    B = c(1, NA, 1, NA),
    A = c(1, 1, NA, NA)
  )

  diag <- diagnosticar_painel(df_test, colunas = c("B", "A"))
  expect_equal(diag$variavel, c("A", "B"))
  expect_equal(diag$pct_disponivel, c(50.0, 50.0))
})

test_that("mensagem_diagnostico formata numeros com ponto de milhar", {
  df_antes <- dplyr::tibble(VD5002 = rep(1.0, 1000000))
  df_depois <- dplyr::tibble(VD5002 = rep(1.0, 750000))
  diag <- diagnosticar_painel(df_antes, colunas = "VD5002")

  msg <- mensagem_diagnostico(diag, df_antes, df_depois, ano = 2023)
  expect_true(grepl("1\\.000\\.000", msg))
  expect_true(grepl("750\\.000", msg))
  expect_true(grepl("250\\.000", msg))
})
