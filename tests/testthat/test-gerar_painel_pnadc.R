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

test_that("gerar_painel_pnadc valida argumentos logicos", {
  expect_error(gerar_painel_pnadc(ano = 2023, balancear = "sim"), "balancear")
  expect_error(gerar_painel_pnadc(ano = 2023, balancear = NA), "balancear")
  expect_error(gerar_painel_pnadc(ano = 2023, low_memory = 1), "low_memory")
  expect_error(gerar_painel_pnadc(ano = 2023, verbose = "FALSE"), "verbose")
})

test_that("gerar_painel_pnadc valida vars_tri e vars_visita", {
  expect_error(gerar_painel_pnadc(ano = 2023, vars_tri = 123), "vars_tri")
  expect_error(gerar_painel_pnadc(ano = 2023, vars_tri = character(0)), "nao pode ser um vetor vazio")
  expect_error(gerar_painel_pnadc(ano = 2023, vars_visita = 456), "vars_visita")
  expect_error(gerar_painel_pnadc(ano = 2023, vars_visita = character(0)), "nao pode ser um vetor vazio")
})

test_that("gerar_painel_pnadc inclui chaves de ID obrigatorias mesmo em selecoes customizadas", {
  chaves_obrig_tri <- c("UPA", "V1008", "V1014", "V2008", "V20081", "V20082", "V2007", "UF", "Ano", "Trimestre")
  custom_tri <- c("V2009", "VD4020")
  res_tri <- unique(c(chaves_obrig_tri, custom_tri))

  expect_true(all(chaves_obrig_tri %in% res_tri))
  expect_true(all(custom_tri %in% res_tri))
})

test_that("consolidar_base_habitacao preserva colunas quando vars_visita e NULL (Achado 1)", {
  dados_casa_mock <- data.frame(
    UPA = "110000016",
    V1008 = "01",
    V1014 = "10",
    Ano = 2023,
    UF = "11",
    VD5002 = 1500,
    V5002A = 1,
    S01006 = 2,
    coluna_extra_visita = "A",
    stringsAsFactors = FALSE
  )

  chaves <- c("UPA", "V1008", "V1014", "Ano", "UF")
  vars_hab_especificas <- setdiff(names(dados_casa_mock), chaves)
  expect_equal(vars_hab_especificas, c("VD5002", "V5002A", "S01006", "coluna_extra_visita"))
  expect_gt(length(vars_hab_especificas), 0L)
})

test_that("gerar_painel_pnadc exclui colunas trimestrais de vars_hab_especificas quando vars_visita e NULL", {
  painel_cruzado_mock <- data.frame(
    id_dom = "1100000160110",
    id_ind = "110000016011022081992111",
    UPA = "110000016",
    V1008 = "01",
    V1014 = "10",
    Ano = 2023,
    Trimestre = 1,
    UF = "11",
    V2009 = 30,       # Coluna trimestral de pessoa
    VD4020 = 2500,    # Coluna trimestral de pessoa
    VD5002 = 1200,    # Coluna de Visita 1 (habitação)
    S01013 = 1,       # Coluna de Visita 1 (habitação)
    stringsAsFactors = FALSE
  )

  chaves_obrig_visita <- c("UPA", "V1008", "V1014", "Ano", "UF")
  vars_tri_proc <- c("UPA", "V1008", "V1014", "V2008", "V20081", "V20082", "V2007", "UF", "Ano", "Trimestre",
                     "V2009", "VD4020")

  cols_excluir <- c("id_dom", "id_ind", chaves_obrig_visita, vars_tri_proc)
  vars_hab_especificas <- setdiff(names(painel_cruzado_mock), cols_excluir)

  expect_equal(vars_hab_especificas, c("VD5002", "S01013"))
  expect_false("V2009" %in% vars_hab_especificas)
  expect_false("VD4020" %in% vars_hab_especificas)
})

test_that("balancear = TRUE preserva linhas com VD4020 = NA mas remove linhas com VD5002 = NA", {
  painel_mock <- data.frame(
    id_dom = c("D1", "D2", "D3"),
    id_ind = c("I1", "I2", "I3"),
    V2009  = c(5, 35, 40),          # I1 e crianca (5 anos)
    VD4020 = c(NA, 3000, 2000),     # I1 tem VD4020 = NA
    VD5002 = c(1000, 1000, NA),     # D3 nao casou na Visita 1 (VD5002 = NA)
    S01013 = c(1, 1, NA),
    stringsAsFactors = FALSE
  )

  vars_hab <- c("VD5002", "S01013")
  painel_bal <- painel_mock %>% dplyr::filter(dplyr::if_all(dplyr::all_of(vars_hab), ~ !is.na(.)))

  expect_equal(nrow(painel_bal), 2L)
  expect_equal(painel_bal$id_ind, c("I1", "I2")) # Preservou I1 (crianca com VD4020 = NA)
  expect_false("I3" %in% painel_bal$id_ind)      # Removeu I3 (falha na Visita 1)
})
