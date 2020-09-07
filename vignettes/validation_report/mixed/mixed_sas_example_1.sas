* Written by R;
*  foreign::write.foreign(df = dat, datafile = here::here("vignettes",  ;

DATA  rdata ;
LENGTH
 Subject $ 3
 Sex $ 6
;

INFILE  "C:/Users/z7a/Documents/DescrTab2/vignettes/validation_report/mixed/mixed_dat_1.csv" 
     DSD 
     LRECL= 26 ;
INPUT
 distance
 age
 Subject
 Sex $ 
;
RUN;

ods graphics off;
ods html file="mixed_example_1.html" path=".";

/*PROC UNIVARIATE data=rdata;*/
/*VAR diff;*/
/*RUN;*/

/* Type the SAS example you want to calculate here, e.g.: */
proc mixed data=rdata CONVG=0.0000000000001;
   class Sex Subject;
   model distance = Sex;
   random Intercept /subject=Subject;
run;

ods html close;
