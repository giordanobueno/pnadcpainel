test_that("criar_ids_datazoom gera id_dom e id_ind corretamente", {
  df_dummy <- data.frame(
    UPA    = c("110000016", "110000016"),
    V1008  = c("01", "01"),
    V1014  = c("10", "10"),
    V2008  = c(22, 4),
    V20081 = c(8, 4),
    V20082 = c(1992, 1993),
    V2007  = c(1, 2),
    UF     = c("11", "11"),
    stringsAsFactors = FALSE
  )

  res <- criar_ids_datazoom(df_dummy)

  expect_true("id_dom" %in% names(res))
  expect_true("id_ind" %in% names(res))
  expect_equal(res$id_dom[1], "1100000160110")
  expect_equal(res$id_ind[1], "110000016011022081992111")
  expect_false("V2008" %in% names(res))
})

test_that("criar_ids_datazoom valida colunas ausentes", {
  df_invalido <- data.frame(UPA = "110000016")
  expect_error(criar_ids_datazoom(df_invalido), "Colunas obrigatorias ausentes")
})

test_that("criar_ids_datazoom filtra dados invalidos de nascimento e sexo NA", {
  df_invalido <- data.frame(
    UPA    = c("110000016", "110000016"),
    V1008  = c("01", "01"),
    V1014  = c("10", "10"),
    V2008  = c(99, 4),
    V20081 = c(8, 99),
    V20082 = c(1992, 1993),
    V2007  = c(1, NA),
    UF     = c("11", "11"),
    stringsAsFactors = FALSE
  )

  res <- criar_ids_datazoom(df_invalido)
  expect_equal(nrow(res), 0)
})
