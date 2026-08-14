test_that("gerar_painel_pnadc valida o ano de entrada", {
  expect_error(gerar_painel_pnadc(ano = 2000), "Ano invalido")
  expect_error(gerar_painel_pnadc(ano = "invalido"), "deve ser um unico numero inteiro valido")
  expect_error(gerar_painel_pnadc(ano = c(2022, 2023)), "deve ser um unico numero inteiro valido")
})

test_that("gerar_painel_pnadc inclui chaves de ID obrigatorias mesmo em selecoes customizadas", {
  chaves_obrig_tri <- c("UPA", "V1008", "V1014", "V2008", "V20081", "V20082", "V2007", "UF", "Ano", "Trimestre")
  custom_tri <- c("V2009", "VD4020")
  res_tri <- unique(c(chaves_obrig_tri, custom_tri))

  expect_true(all(chaves_obrig_tri %in% res_tri))
  expect_true(all(custom_tri %in% res_tri))
})

test_that("consolidar_base_habitacao preserva colunas quando vars_visita e NULL (Achado 1)", {
  # Mock de dados de habitacao com varias colunas alem das chaves
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

  # Quando vars_visita e NULL
  vars_hab_especificas <- setdiff(names(dados_casa_mock), chaves)
  expect_equal(vars_hab_especificas, c("VD5002", "V5002A", "S01006", "coluna_extra_visita"))
  expect_gt(length(vars_hab_especificas), 0)
})

test_that("gerar_painel_pnadc exclui colunas trimestrais de vars_hab_especificas quando vars_visita e NULL (Achado 2)", {
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
  vars_tri_proc <- c("UPA", "V1008", "V1014", "V2008", "V20081", "V20082", "V2007", "UF", "Ano", "Trimestre", "V2009", "VD4020")

  cols_excluir <- c("id_dom", "id_ind", chaves_obrig_visita, vars_tri_proc)
  vars_hab_especificas <- setdiff(names(painel_cruzado_mock), cols_excluir)

  expect_equal(vars_hab_especificas, c("VD5002", "S01013"))
  expect_false("V2009" %in% vars_hab_especificas)
  expect_false("VD4020" %in% vars_hab_especificas)
})
