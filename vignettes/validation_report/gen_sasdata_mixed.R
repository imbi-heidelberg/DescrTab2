# Type URS-ID here.
URS_ID <- "mixed"

# If there are multiple datasets needed, number them by changing this line accordingly for each dataset.
nr_dataset <-  1
dat_list <- list()

# Put the data that you want to analyse with SAS here.
dat <- nlme::Orthodont
dat2 <- nlme::Orthodont[1:64,]
dat2$Sex <- "Divers"
dat2$distance <- dat2$distance + c(rep(0.1*c(1,4,3,2), 10), 0.1*rep(c(0.4,2,1.5, 2.3), 6) )
dat2$Subject <- str_replace_all(dat2$Subject, "M", "D")
dat <- bind_rows(dat, dat2)
dat <- as_tibble(dat)


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
