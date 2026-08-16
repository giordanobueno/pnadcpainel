# Helper de Fixtures Sinteticas para Mock do PNADcIBGE

criar_fixture_trimestre <- function(year, quarter, vars = NULL) {
  # 2 domicilios por trimestre, 2 pessoas por domicilio
  df <- data.frame(
    UPA      = c("110000016", "110000016", "110000017", "110000017"),
    V1008    = c("01", "01", "02", "02"),
    V1014    = c("10", "10", "10", "10"),
    Ano      = as.integer(year),
    Trimestre = as.integer(quarter),
    UF       = "11",
    V2007    = c(1L, 2L, 1L, 2L),
    V2008    = c(15L, 20L, 10L, 5L),
    V20081   = c(5L, 8L, 12L, 1L),
    V20082   = c(1990L, 1992L, 1985L, 2015L),
    V2001    = c(2L, 2L, 2L, 2L),
    V2005    = c(1L, 2L, 1L, 2L),
    V2009    = c(33L, 31L, 38L, 8L),
    VD3004   = c(5L, 6L, 4L, 1L),
    V3001    = c(1L, 1L, 1L, 2L),
    VD4001   = c(1L, 1L, 1L, 2L),
    VD4002   = c(1L, 2L, 1L, NA_integer_),
    VD4009   = c(1L, NA_integer_, 2L, NA_integer_),
    VD4020   = c(3500, NA_real_, 2200, NA_real_),
    VD4010   = c(2L, NA_integer_, 5L, NA_integer_),
    stringsAsFactors = FALSE
  )

  if (!is.null(vars)) {
    # Garantir chaves + variaveis solicitadas presentes
    chaves_tri <- c("UPA", "V1008", "V1014", "Ano", "Trimestre", "UF", "V2007", "V2008", "V20081", "V20082")
    cols_manter <- intersect(unique(c(chaves_tri, vars)), names(df))
    df <- df[, cols_manter, drop = FALSE]
  }
  df
}

criar_fixture_habitacao <- function(year, vars = NULL) {
  # Visita 1
  df <- data.frame(
    UPA    = c("110000016", "110000017"),
    V1008  = c("01", "02"),
    V1014  = c("10", "10"),
    Ano    = as.integer(year),
    UF     = "11",
    V5001A = c(2L, 2L),
    VD5002 = c(1750, 1100),
    V5002A = c(2L, 1L),
    S01013 = c(1L, 1L),
    S01006 = c(2L, 3L),
    S01010 = c(1L, 1L),
    stringsAsFactors = FALSE
  )

  if (!is.null(vars)) {
    chaves_hab <- c("UPA", "V1008", "V1014", "Ano", "UF")
    cols_manter <- intersect(unique(c(chaves_hab, vars)), names(df))
    df <- df[, cols_manter, drop = FALSE]
  }
  df
}

criar_mock_provider <- function() {
  function(year, quarter = NULL, interview = NULL, vars = NULL, design = FALSE, labels = FALSE) {
    if (!is.null(quarter)) {
      criar_fixture_trimestre(year, quarter, vars)
    } else if (!is.null(interview) && interview == 1) {
      criar_fixture_habitacao(year, vars)
    } else {
      stop("Parametros invalidos de mock.")
    }
  }
}
