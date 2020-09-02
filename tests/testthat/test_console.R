context("Output tables to the console")
library(magrittr)

verify_output("../console/print1.txt",
              descr(iris) %>% print(printFormat = "console"))

verify_output(
  "../console/print2.txt",
  descr(
    iris,
    "Species",
    group_labels = list(setosa = "My custom group label"),
    var_options = list(Sepal.Length = list(label = "My custom variable label"))
  ) %>% print(printFormat = "console")
)
