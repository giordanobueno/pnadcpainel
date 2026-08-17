# Fixture sintetica para testes offline do pacote pnadcpainel em R

criar_mock_provider_r <- function() {
  function(year, quarter = NULL, interview = NULL, vars = NULL, design = FALSE, labels = FALSE, verbose = TRUE) {
    if (!is.null(quarter)) {
      # 4 trimestres, 2 domicilios ("1100000160110" e "1100000160210"), 2 pessoas por domicilio
      df <- dplyr::tibble(
        UPA       = c("110000016", "110000016", "110000016", "110000016"),
        V1008     = c("01", "01", "02", "02"),
        V1014     = c("10", "10", "10", "10"),
        Ano       = as.integer(year),
        Trimestre = as.integer(quarter),
        UF        = c("11", "11", "11", "11"),
        V2007     = c(1L, 2L, 1L, 2L),
        V2008     = c(15L, 20L, 5L, 12L),
        V20081    = c(5L, 8L, 1L, 11L),
        V20082    = c(1990L, 1992L, 1985L, 1988L),
        V2001     = c(2L, 2L, 2L, 2L),
        V2005     = c(1L, 2L, 1L, 2L),
        V2009     = c(33L, 31L, 38L, 35L),
        VD3004    = c(7L, 6L, 5L, 7L),
        V3001     = c(1L, 1L, 1L, 1L),
        VD4001    = c(1L, 1L, 1L, 2L),
        VD4002    = c(1L, 1L, 1L, NA_integer_), # NA em VD4002 na pessoa 4
        VD4009    = c(1L, 3L, 1L, NA_integer_),
        VD4020    = c(3500.0, 2800.0, 5000.0, NA_real_), # NA em renda da pessoa 4
        VD4010    = c(1L, 2L, 1L, NA_integer_)
      )
    } else if (!is.null(interview)) {
      # Visita 1 (Domicilios)
      df <- dplyr::tibble(
        UPA       = c("110000016", "110000016"),
        V1008     = c("01", "02"),
        V1014     = c("10", "10"),
        Ano       = as.integer(year),
        UF        = c("11", "11"),
        V5001A    = c(2L, 2L),
        VD5002    = c(1500.0, 2500.0),
        V5002A    = c(2L, 2L),
        S01013    = c(1L, 1L),
        S01006    = c(2L, 3L),
        S01010    = c(1L, 1L)
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
