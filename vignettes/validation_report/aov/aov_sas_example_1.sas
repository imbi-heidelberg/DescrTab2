* Written by R;
*  foreign::write.foreign(df = dat, datafile = here::here("vignettes",  ;

PROC FORMAT;
value P 
     1 = "1" 
     2 = "2" 
     3 = "3" 
;

value N 
     1 = "1" 
     2 = "2" 
     3 = "3" 
;

DATA  rdata ;
INFILE  "C:/Users/z7a/Documents/DescrTab2/vignettes/validation_report/aov/aov_dat_1.csv" 
     DSD 
     LRECL= 24 ;
INPUT
 y
 P
 N
;
FORMAT P P. ;
FORMAT N N. ;
RUN;

ods graphics off;
ods html file="aov_example_1.html" path=".";


/*PROC UNIVARIATE data=rdata;*/
/*VAR diff;*/
/*RUN;*/

/* Type the SAS example you want to calculate here, e.g.: */
proc anova data=rdata;
class P;
   model y = P;
run;


proc anova data=rdata;
class N;
   model y = N;
run;

ods html close;
