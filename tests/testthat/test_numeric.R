context("Output tables to the console")
library(magrittr)

verify_output("../console/print3.txt",
              descr(iris) %>% print(printFormat = "numeric"))
verify_output(
  "../console/print4.txt",
  descr(
    iris,
    "Species",
    group_labels = list(setosa = "My custom group label"),
    var_options = list(Sepal.Length = list(label = "My custom variable label"))
  ) %>% print(printFormat = "numeric")
)
