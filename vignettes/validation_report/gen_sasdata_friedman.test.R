# Type URS-ID here.
URS_ID <- "friedman.test"

# If there are multiple datasets needed, number them by changing this line accordingly for each dataset.
nr_dataset <-  1
dat_list <- list()

# Put the data that you want to analyse with SAS here.
RoundingTimes <-
  matrix(c(5.40, 5.50, 5.55,
           5.85, 5.70, 5.75,
           5.20, 5.60, 5.50,
           5.55, 5.50, 5.40,
           5.90, 5.85, 5.70,
           5.45, 5.55, 5.60,
           5.40, 5.40, 5.35,
           5.45, 5.50, 5.35,
           5.25, 5.15, 5.00,
           5.85, 5.80, 5.70,
           5.25, 5.20, 5.10,
           5.65, 5.55, 5.45,
           5.60, 5.35, 5.45,
           5.05, 5.00, 4.95,
           5.50, 5.50, 5.40,
           5.45, 5.55, 5.50,
           5.55, 5.55, 5.35,
           5.45, 5.50, 5.55,
           5.50, 5.45, 5.25,
           5.65, 5.60, 5.40,
           5.70, 5.65, 5.55,
           6.30, 6.30, 6.25),
         nrow = 22,
         byrow = TRUE,
         dimnames = list(1 : 22,
                         c("Round Out", "Narrow Angle", "Wide Angle")))

idx <- rep(1:22, 3)
dat <- tibble(var = c(RoundingTimes[,1], RoundingTimes[,2], RoundingTimes[,3]),
              group = c(rep("Round Out", 22), rep("Narrow Angle", 22), rep("Wide Angle", 22)),
              indices=idx)


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
