library(exact2x2)

nsim <- 100

for (nsim in c(100, 200, 300, 400, 500)) {
  starttime <- Sys.time()

  var <- sample(c("a", "b"), nsim, TRUE)
  group <- sample(c("a", "b", "c"), nsim, TRUE)
  tabl <- table(var, group)
  x1 <- tabl[1, 1]
  n1 <- sum(tabl[, 1])
  x2 <- tabl[1, 2]
  n2 <- sum(tabl[, 2])
  exact2x2::boschloo(x1, n1, x2, n2, conf.int = FALSE)

  stoptime <- Sys.time()
  print(
    paste(
      "R runtime for nsim =", nsim, ":",
      round(stoptime - starttime, 3),
      " seconds"
    )
  )
}


for (nsim in c(100, 200, 300, 400, 500)) {
  starttime <- Sys.time()

  var <- sample(c("a", "b"), nsim, TRUE)
  group <- sample(c("a", "b", "c"), nsim, TRUE)
  tabl <- table(var, group)
  x1 <- tabl[1, 1]
  n1 <- sum(tabl[, 1])
  x2 <- tabl[1, 2]
  n2 <- sum(tabl[, 2])
  print(exact2x2::uncondExact2x2(x1, n1, x2, n2, conf.int = TRUE,
  method = "FisherAdj",
  parmtype = "difference"
  ))

  stoptime <- Sys.time()
  print(
    paste(
      "R runtime for nsim =", nsim, ":",
      round(stoptime - starttime, 3),
      " seconds"
    )
  )
}


for (nsim in c(100, 200, 300, 400, 500, 600, 700, 800)) {
  starttime <- Sys.time()

  var <- sample(c("a", "b"), nsim, TRUE)
  group <- sample(c("a", "b", "c"), nsim, TRUE)
  tabl <- table(var, group)
  x1 <- tabl[1, 1]
  n1 <- sum(tabl[, 1])
  x2 <- tabl[1, 2]
  n2 <- sum(tabl[, 2])
  fisher.test(var, group)

  stoptime <- Sys.time()
  print(
    paste(
      "R runtime for nsim =", nsim, ":",
      round(stoptime - starttime, 3),
      " seconds"
    )
  )
}

