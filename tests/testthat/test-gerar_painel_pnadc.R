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
