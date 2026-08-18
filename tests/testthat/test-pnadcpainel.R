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
  expect_error(gerar_painel_pnadc(), "Informe 'ano' ou 'anos'")
  expect_error(gerar_painel_pnadc(ano = NULL), "Informe 'ano' ou 'anos'")
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

# ------------------------------------------------------------------------------
# TESTES DE PAINEL LONGITUDINAL MULTI-ANO (CASOS DE USO 1 A 10 EM R)
# ------------------------------------------------------------------------------

criar_mock_multiyear_provider_r <- function() {
  function(year, quarter = NULL, interview = NULL, vars = NULL, design = FALSE, labels = FALSE, verbose = TRUE) {
    if (!is.null(quarter)) {
      y <- as.integer(year)
      q <- as.integer(quarter)

      rows <- list()
      # Person A (Ind 1): present in 2023 Q1..Q4 and 2024 Q1 (T4->T1 continuity!)
      if (y == 2023L || (y == 2024L && q == 1L)) {
        rows[[length(rows) + 1L]] <- dplyr::tibble(
          UPA = "110000016", V1008 = "01", V1014 = "10", Ano = y, Trimestre = q, UF = "11",
          V2007 = 1L, V2008 = 15L, V20081 = 5L, V20082 = 1990L,
          V2001 = 2L, V2005 = 1L, V2009 = 33L, VD3004 = 7L, V3001 = 1L,
          VD4001 = 1L, VD4002 = 1L, VD4009 = 1L, VD4020 = 3500.0, VD4010 = 1L
        )
      }
      # Person B (Ind 2): present ONLY in 2023 Q4 (observed only once)
      if (y == 2023L && q == 4L) {
        rows[[length(rows) + 1L]] <- dplyr::tibble(
          UPA = "110000016", V1008 = "01", V1014 = "10", Ano = y, Trimestre = q, UF = "11",
          V2007 = 2L, V2008 = 20L, V20081 = 8L, V20082 = 1992L,
          V2001 = 2L, V2005 = 2L, V2009 = 31L, VD3004 = 6L, V3001 = 1L,
          VD4001 = 1L, VD4002 = 1L, VD4009 = 3L, VD4020 = 2800.0, VD4010 = 2L
        )
      }
      # Person C (Ind 3): present in 2023 Q1 and 2023 Q2 (observed twice)
      if (y == 2023L && (q == 1L || q == 2L)) {
        rows[[length(rows) + 1L]] <- dplyr::tibble(
          UPA = "110000016", V1008 = "02", V1014 = "10", Ano = y, Trimestre = q, UF = "11",
          V2007 = 1L, V2008 = 5L, V20081 = 1L, V20082 = 1985L,
          V2001 = 2L, V2005 = 1L, V2009 = 38L, VD3004 = 5L, V3001 = 1L,
          VD4001 = 1L, VD4002 = 1L, VD4009 = 1L, VD4020 = 5000.0, VD4010 = 1L
        )
      }
      # Person D (Ind 4): present in all quarters of 2023 and 2024
      rows[[length(rows) + 1L]] <- dplyr::tibble(
        UPA = "110000016", V1008 = "02", V1014 = "10", Ano = y, Trimestre = q, UF = "11",
        V2007 = 2L, V2008 = 12L, V20081 = 11L, V20082 = 1988L,
        V2001 = 2L, V2005 = 2L, V2009 = 35L, VD3004 = 7L, V3001 = 1L,
        VD4001 = 1L, VD4002 = 1L, VD4009 = 1L, VD4020 = 4000.0, VD4010 = 1L
      )
      df <- dplyr::bind_rows(rows)
    } else if (!is.null(interview)) {
      df <- dplyr::tibble(
        UPA = c("110000016", "110000016"),
        V1008 = c("01", "02"),
        V1014 = c("10", "10"),
        Ano = as.integer(year),
        UF = c("11", "11"),
        V5001A = c(2L, 2L),
        VD5002 = c(1500.0, 2500.0),
        V5002A = c(2L, 2L),
        S01013 = c(1L, 1L),
        S01006 = c(2L, 3L),
        S01010 = c(1L, 1L)
      )
    } else {
      stop("E preciso especificar quarter ou interview.")
    }

    if (!is.null(vars)) {
      chaves_obrig <- if (!is.null(interview)) c("UPA", "V1008", "V1014", "Ano", "UF") else c("UPA", "V1008", "V1014", "V2008", "V20081", "V20082", "V2007", "UF", "Ano", "Trimestre")
      cols_manter <- unique(c(chaves_obrig, intersect(vars, names(df))))
      df <- df[, cols_manter, drop = FALSE]
    }
    df
  }
}

test_that("Caso 1 - um unico ano", {
  mock_fn <- criar_mock_provider_r()
  set_mock_provider(mock_fn)
  withr::defer(set_mock_provider(NULL))

  painel <- gerar_painel_pnadc(ano = 2023, verbose = FALSE)
  expect_true(nrow(painel) > 0)
  expect_equal(unique(painel$Ano), 2023L)
  expect_equal(sort(unique(painel$Trimestre)), 1:4)
})

test_that("Caso 2 - dois anos consecutivos (vetor)", {
  mock_fn <- criar_mock_multiyear_provider_r()
  set_mock_provider(mock_fn)
  withr::defer(set_mock_provider(NULL))

  painel <- gerar_painel_pnadc(anos = 2023:2024, verbose = FALSE)
  expect_equal(sort(unique(painel$Ano)), c(2023L, 2024L))
  expect_true("periodo" %in% names(painel))
})

test_that("Caso 3 - continuidade T4 -> T1 mantem mesmo id_ind", {
  mock_fn <- criar_mock_multiyear_provider_r()
  set_mock_provider(mock_fn)
  withr::defer(set_mock_provider(NULL))

  painel <- gerar_painel_pnadc(anos = c(2023, 2024), verbose = FALSE)
  id_person_a <- "110000016011015051990111"
  obs_a <- painel[painel$id_ind == id_person_a, ]
  periodos_a <- obs_a$periodo

  expect_true("2023_4" %in% periodos_a)
  expect_true("2024_1" %in% periodos_a)
  idx_23_4 <- match("2023_4", periodos_a)
  idx_24_1 <- match("2024_1", periodos_a)
  expect_equal(idx_24_1, idx_23_4 + 1L)
})

test_that("Caso 4 - individuo observado apenas uma vez e preservado", {
  mock_fn <- criar_mock_multiyear_provider_r()
  set_mock_provider(mock_fn)
  withr::defer(set_mock_provider(NULL))

  painel <- gerar_painel_pnadc(anos = c(2023, 2024), verbose = FALSE)
  id_person_b <- "110000016011020081992211"
  obs_b <- painel[painel$id_ind == id_person_b, ]
  expect_equal(nrow(obs_b), 1L)
  expect_equal(obs_b$periodo, "2023_4")
})

test_that("Caso 5 - individuo observado em dois trimestres e preservado", {
  mock_fn <- criar_mock_multiyear_provider_r()
  set_mock_provider(mock_fn)
  withr::defer(set_mock_provider(NULL))

  painel <- gerar_painel_pnadc(anos = c(2023, 2024), verbose = FALSE)
  id_person_c <- "110000016021005011985111"
  obs_c <- painel[painel$id_ind == id_person_c, ]
  expect_equal(nrow(obs_c), 2L)
  expect_equal(obs_c$periodo, c("2023_1", "2023_2"))
})

test_that("Caso 6 - individuo observado em todos os trimestres", {
  mock_fn <- criar_mock_multiyear_provider_r()
  set_mock_provider(mock_fn)
  withr::defer(set_mock_provider(NULL))

  painel <- gerar_painel_pnadc(anos = c(2023, 2024), verbose = FALSE)
  id_person_d <- "110000016021012111988211"
  obs_d <- painel[painel$id_ind == id_person_d, ]
  expect_equal(nrow(obs_d), 8L)
})

test_that("Caso 7 - nao duplicacao no chave natural", {
  mock_fn <- criar_mock_multiyear_provider_r()
  set_mock_provider(mock_fn)
  withr::defer(set_mock_provider(NULL))

  painel <- gerar_painel_pnadc(anos = c(2023, 2024), verbose = FALSE)
  dups <- duplicated(painel[, c("id_ind", "Ano", "Trimestre")])
  expect_equal(sum(dups), 0L)
})

test_that("Caso 8 - backward compatibility", {
  mock_fn <- criar_mock_provider_r()
  set_mock_provider(mock_fn)
  withr::defer(set_mock_provider(NULL))

  painel <- gerar_painel_pnadc(ano = 2023, verbose = FALSE)
  expect_true(nrow(painel) > 0)
  expect_true("periodo" %in% names(painel))
})

test_that("Caso 9 - balancear nao elimina presencas longitudinais parciais", {
  mock_fn <- criar_mock_multiyear_provider_r()
  set_mock_provider(mock_fn)
  withr::defer(set_mock_provider(NULL))

  painel_bal <- gerar_painel_pnadc(anos = c(2023, 2024), balancear = TRUE, verbose = FALSE)
  ids_presentes <- unique(painel_bal$id_ind)
  expect_true("110000016011020081992211" %in% ids_presentes)
  expect_true("110000016021005011985111" %in% ids_presentes)
})

test_that("Caso 10 - anos nao consecutivos", {
  mock_fn <- criar_mock_provider_r()
  set_mock_provider(mock_fn)
  withr::defer(set_mock_provider(NULL))

  painel <- gerar_painel_pnadc(anos = c(2020, 2022), verbose = FALSE)
  expect_equal(sort(unique(painel$Ano)), c(2020L, 2022L))
})

test_that("validacao de conflito entre ano e anos", {
  expect_error(gerar_painel_pnadc(ano = 2023, anos = c(2023, 2024)), "Informe apenas 'ano' ou 'anos', nao ambos")
})

test_that("gerar_painel_pnadc_mensal gera mes exato e peso mensal calibrado", {
  mock_fn <- criar_mock_provider_r()
  set_mock_provider(mock_fn)
  withr::defer(set_mock_provider(NULL))

  painel_m <- gerar_painel_pnadc_mensal(ano = 2023, verbose = FALSE)
  expect_true(is.data.frame(painel_m))
  expect_true("mes_exato_aaaamm" %in% names(painel_m))
  expect_true("peso_mensal" %in% names(painel_m))
  expect_equal(attr(painel_m, "taxa_determinacao_mensal"), 100.0)
})

test_that("construir_crosswalk_pnadc funciona offline com mock", {
  mock_fn <- criar_mock_provider_r()
  set_mock_provider(mock_fn)
  withr::defer(set_mock_provider(NULL))

  painel <- gerar_painel_pnadc(ano = 2023, verbose = FALSE)
  cw <- construir_crosswalk_pnadc(painel)
  expect_true(is.data.frame(cw))
  expect_true("ref_month_yyyymm" %in% names(cw))
})


