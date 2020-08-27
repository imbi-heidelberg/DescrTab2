context("Functionality")


test_that("Generic example works", {
  dat <- data.frame(
    Geschlecht = factor(), Schulabschluss = factor(),
    Raucher = factor(), Groesse = numeric(), Gewicht = numeric()
  )
  dat <- structure(list(
    Geschlecht = structure(ordered(c(1, 1, 0, 0, 1, 0, 0, 1, 0, 0)),
      .Label = c("WEIBLICH", "MAENNLICH"), class = "factor"
    ),
    Schulabschluss = structure(c(1, 2, 2, 2, 2, 2, 3, 1, 3, 1),
      .Label = c("Hochschule", "Mittlere Reife", "Fachhochschule"),
      class = "factor"
    ),
    Raucher = structure(c(1, 2, 1, 2, NA, NA, 2, 1, 1, 1),
      .Label = c("nein", "ja"), class = "factor"
    ),
    Groesse = c(170, 163, 174, 172, 168, 182, 178, 187, 180, 190),
    Gewicht = c(68, 75, 75, NA, 115, 87, 90, NA, 86, 56)
  ),
  .Names = c("Geschlecht", "Schulabschluss", "Raucher", "Groesse", "Gewicht"),
  row.names = c(NA, 10L), class = "data.frame"
  )
  des.print(
    dat = dat, group = 1, create = "knitr",
    var.names = c("Schulabschluss", "Raucher", "Groesse", "Gewicht"),
    caption = c("WEIBLICH", "MAENNLICH"),
    tab.caption = "Beispiel Auswertung",
    digits.sd = 3,
    digits.m = 3,
    digits.minmax = 1,
    digits.qu = 2,
    digits.p = 4
  )

  expect_equal(1, 1)
})
