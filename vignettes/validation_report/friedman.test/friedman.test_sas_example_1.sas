* Written by R;
*  foreign::write.foreign(df = dat, datafile = here::here("vignettes",  ;

DATA  rdata ;
LENGTH
 group $ 12
;

INFILE  "C:/Users/z7a/Documents/DescrTab2/vignettes/validation_report/friedman.test/friedman.test_dat_1.csv" 
     DSD 
     LRECL= 26 ;
INPUT
 var
 group
 indices
;
RUN;

ods graphics off;
ods html file="friedman.test_example_1.html" path=".";

/*PROC UNIVARIATE data=rdata;*/
/*VAR diff;*/
/*RUN;*/

/* Type the SAS example you want to calculate here, e.g.: */
proc freq data=rdata;
   tables indices*group*var /
   cmh2 scores=rank noprint;
run;

ods html close;
