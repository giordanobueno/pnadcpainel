test_that("gerar_painel_pnadc valida o ano de entrada", {
  expect_error(gerar_painel_pnadc(ano = 2000), "Ano invalido")
  expect_error(gerar_painel_pnadc(ano = "invalido"), "deve ser um unico numero inteiro valido")
  expect_error(gerar_painel_pnadc(ano = c(2022, 2023)), "deve ser um unico numero inteiro valido")
})
