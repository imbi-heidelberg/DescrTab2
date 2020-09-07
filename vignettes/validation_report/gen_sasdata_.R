# Type URS-ID here.
URS_ID <- "URS_ID"

# If there are multiple datasets needed, number them by changing this line accordingly for each dataset.
nr_dataset <-  1
dat_list <- list()

# Put the data that you want to analyse with SAS here.
x <- c(1.83,  0.50,  1.62,  2.48, 1.68, 1.88, 1.55, 3.06, 1.30)
y <- c(0.878, 0.647, 0.598, 2.05, 1.06, 1.29, 1.06, 3.14, 1.29)
dat <- # e.g.
  tibble::tibble(x = x,
                 y=y)


require(foreign)
require(here)

code_dir <- here::here(
  "vignettes",
  "validation_report",
  URS_ID)

code_path <-
  paste0(code_dir, "/", URS_ID, "_sas_example_", nr_dataset, ".sas")

dir.create(code_dir)
file.create(code_path)
codefile_con <- file(code_path, open = "r+")

foreign::write.foreign(
  df = dat,
  datafile = here::here(
    "vignettes",
    "validation_report",
    URS_ID,
    paste0(URS_ID, "_dat_", nr_dataset, ".csv")
  ),
  codefile = codefile_con,
  package = "SAS"
)

append_txt <- c(
  "",
  "ods graphics off;",
  paste0('ods html file="', URS_ID, '_example_', nr_dataset, '.html" path=".";'),
  "",
  "PROC UNIVARIATE data=rdata;",
  "VAR diff;",
  "RUN;",
  "",
  "/* Type the SAS example you want to calculate here, e.g.: */",
  "/*proc reg data=rdata;*/",
  "/*   model effect = group;*/",
  "/*run;*/",
  "",
  "ods html close;"
)

write(append_txt, codefile_con, append = T)
close(codefile_con)
