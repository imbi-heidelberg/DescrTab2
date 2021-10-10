.N <- function(var) {
  sum(!is.na(var))
}

.Nmiss <- function(var) {
  sum(is.na(var))
}

.mean <- function(var) {
  ret <- base::mean(var, na.rm = TRUE)
  if (is.nan(ret)) {
    return(NA_real_)
  } else {
    return(ret)
  }
}

.sd <- function(var) {
  stats::sd(var, na.rm = TRUE)
}

.median <- function(var) {
  stats::median(var, na.rm = TRUE)
}

.Q1 <- function(var) {
  stats::quantile(var,
                  probs = 0.25,
                  na.rm = TRUE,
                  type = 2
  )
}

.Q3 <- function(var) {
  stats::quantile(var,
                  probs = 0.75,
                  na.rm = TRUE,
                  type = 2
  )
}

.IQR <- function(var) {
  stats::quantile(var,
                  probs = 0.75,
                  na.rm = TRUE,
                  type = 2) -
    stats::quantile(var,
                    probs = 0.25,
                    na.rm = TRUE,
                    type = 2)
}

.min <- function(var) {
  if (any(!is.na(var))) {
    min(var, na.rm = TRUE)
  } else {
    NA_real_
  }
}

.max <- function(var) {
  if (any(!is.na(var))) {
    max(var, na.rm = TRUE)
  } else {
    NA_real_
  }
}

.mode <- function(var) {
  ux <- unique(var)
  ux[which.max(tabulate(match(var, ux)))]
}

.skew <- function(var) {
  mean((var-.mean(var))^(3)) / (sd(var))^(3/2)
}

.kurtosis <- function(var) {
  mean((var-.mean(var))^(4)) / (sd(var))^(2) -3
}

.factormean <- function(var) {
  var %>%
    as.character() %>%
    as.numeric() %>%
    mean(na.rm = TRUE)
}

.factorsd <- function(var) {
  var %>%
    as.character() %>%
    as.numeric() %>%
    stats::sd(na.rm = TRUE)
}

.factormedian <- function(var) {
  var %>%
    as.character() %>%
    as.numeric() %>%
    stats::median(na.rm = TRUE)
}

.factorQ1 <- function(var) {
  var %>%
    as.character() %>%
    as.numeric() %>%
    stats::quantile(
      probs = 0.25,
      na.rm = TRUE,
      type = 2
    )
}

.factorQ3 <- function(var) {
  var %>%
    as.character() %>%
    as.numeric() %>%
    stats::quantile(
      probs = 0.75,
      na.rm = TRUE,
      type = 2
    )
}

.factormin <- function(var) {
  var_num <- var %>%
    as.character() %>%
    as.numeric()
  if (any(!is.na(var))) {
    min(var_num, na.rm = TRUE)
  } else {
    NA_real_
  }
}

.factormax <- function(var) {
  var_num <- var %>%
    as.character() %>%
    as.numeric()
  if (any(!is.na(var))) {
    max(var_num, na.rm = TRUE)
  } else {
    NA_real_
  }
}
