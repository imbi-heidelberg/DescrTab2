# Type URS-ID here.
URS_ID <- "kruskal.test"

# If there are multiple datasets needed, number them by changing this line accordingly for each dataset.
nr_dataset <-  1
dat_list <- list()

# Put the data that you want to analyse with SAS here.
x <- c(2.9, 3.0, 2.5, 2.6, 3.2) # normal subjects
y <- c(3.8, 2.7, 4.0, 2.4)      # with obstructive airway disease
z <- c(2.8, 3.4, 3.7, 2.2, 2.0) # with asbestosis
group <- c(rep("Trt", length(x)), rep("Ctrl", length(y)), rep("Placebo", length(z)))
dat_kruskal.test <- tibble(var=c(x,y,z), group=group)
dat <- dat_kruskal.test


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
