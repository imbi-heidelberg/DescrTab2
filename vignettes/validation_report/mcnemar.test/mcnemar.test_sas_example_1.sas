* Written by R;
*  foreign::write.foreign(df = dat, datafile = here::here("vignettes",  ;

DATA  rdata ;
LENGTH
 x $ 10
 y $ 10
;

INFILE  "C:/Users/z7a/Documents/DescrTab2/vignettes/validation_report/mcnemar.test/mcnemar.test_dat_1.csv" 
     DSD 
     LRECL= 29 ;
INPUT
 x
 y $ 
;
RUN;

ods graphics off;
ods html file="mcnemar.test_example_1.html" path=".";



/* Type the SAS example you want to calculate here, e.g.: */
proc freq data=rdata;
   tables x*y /agree;
   exact mcnem /pformat=10.9;
run;


ods html close;
