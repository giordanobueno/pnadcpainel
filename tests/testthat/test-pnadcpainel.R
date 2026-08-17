source(testthat::test_path("../../R/pnadcpainel.R"))
source(testthat::test_path("fixtures/synthetic_pnadc.R"))

test_that("downcast_pnadc converte colunas para integer e valida fracionarios", {
  df <- dplyr::tibble(
    V2007 = c(1.0, 2.0, NA),
    V2008 = c(15.0, 20.0, 99.0),
    VD4020 = c(2500.5, 3000.0, NA)
  )
  res <- downcast_pnadc(df)
  expect_type(res$V2007, "integer")
  expect_type(res$V2008, "integer")
  expect_type(res$VD4020, "double")

  df_frac <- dplyr::tibble(V2008 = c(1.5, 2.0))
  expect_error(downcast_pnadc(df_frac), "Valores fracionarios nao sao permitidos")
})

test_that("criar_ids_datazoom filtra linhas com UF ou V2007 ausente e padroniza zeros", {
  df_input <- dplyr::tibble(
    UPA    = c("110000016", "110000016", "110000016"),
    V1008  = c(1, "01", "10"),
    V1014  = c(10, 10, 10),
    V2008  = c(5, 12, 25),
    V20081 = c(8, 11, 1),
    V20082 = c(1995, 1988, 2000),
    V2007  = c(1, 2, NA),          # NA em V2007
    UF     = c("11", "11", "11")
  )

  res <- criar_ids_datazoom(df_input)
  expect_equal(nrow(res), 2L)
  expect_equal(res$id_dom[1], "1100000160110")
  expect_equal(res$id_ind[1], "110000016011005081995111")
  expect_equal(res$id_ind[2], "110000016011012111988211")
})

test_that("diagnosticar_painel em data frame vazio ordena por pct_disponivel e variavel", {
  df_empty <- dplyr::tibble(b = numeric(0), a = numeric(0), c = numeric(0))
  diag <- diagnosticar_painel(df_empty, colunas = c("b", "a", "c"))

  expect_equal(nrow(diag), 3L)
  expect_equal(diag$variavel, c("a", "b", "c"))
  expect_equal(diag$pct_disponivel, c(0, 0, 0))
})

test_that("mensagem_diagnostico formata numeros no padrao brasileiro com ponto", {
  df_antes <- dplyr::tibble(VD5002 = rep(1.0, 1000000))
  df_depois <- dplyr::tibble(VD5002 = rep(1.0, 750000))
  diag <- diagnosticar_painel(df_antes, colunas = "VD5002")

  msg <- mensagem_diagnostico(diag, df_antes, df_depois, ano = 2023)
  expect_true(grepl("1\\.000\\.000", msg))
  expect_true(grepl("750\\.000", msg))
  expect_true(grepl("250\\.000", msg))
})

test_that("gerar_painel_pnadc valida o ano e flags rigorosamente", {
  expect_error(gerar_painel_pnadc(), "deve ser um unico numero inteiro valido")
  expect_error(gerar_painel_pnadc(ano = NULL), "deve ser um unico numero inteiro valido")
  expect_error(gerar_painel_pnadc(ano = "2023"), "deve ser um unico numero inteiro valido")
  expect_error(gerar_painel_pnadc(ano = 2023.5), "deve ser um numero inteiro valido")
  expect_error(gerar_painel_pnadc(ano = 2000), "Ano invalido")

  expect_error(gerar_painel_pnadc(ano = 2023, balancear = 1), "balancear")
  expect_error(gerar_painel_pnadc(ano = 2023, balancear = "sim"), "balancear")
})

test_that("gerar_painel_pnadc executa 100% offline via mock provider", {
  mock_fn <- criar_mock_provider_r()
  set_mock_provider(mock_fn)
  withr::defer(set_mock_provider(NULL))

  painel <- gerar_painel_pnadc(ano = 2023, verbose = FALSE)
  expect_true(is.data.frame(painel))
  expect_true("id_dom" %in% names(painel))
  expect_true("id_ind" %in% names(painel))
  expect_false(is.null(attr(painel, "diagnostico")))
})

test_that("low_memory FALSE vs TRUE e invariante", {
  mock_fn <- criar_mock_provider_r()
  set_mock_provider(mock_fn)
  withr::defer(set_mock_provider(NULL))

  res_false <- gerar_painel_pnadc(ano = 2023, low_memory = FALSE, verbose = FALSE)
  res_true  <- gerar_painel_pnadc(ano = 2023, low_memory = TRUE, verbose = FALSE)

  expect_equal(dim(res_false), dim(res_true))
  expect_equal(names(res_false), names(res_true))
  expect_equal(res_false$id_dom, res_true$id_dom)
  expect_equal(res_false$id_ind, res_true$id_ind)
})
