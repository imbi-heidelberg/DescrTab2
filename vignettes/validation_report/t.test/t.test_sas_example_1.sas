* Written by R;
*  foreign::write.foreign(df = dat, datafile = here::here("vignettes",  ;

PROC FORMAT;
value group 
     1 = "1" 
     2 = "2" 
;

DATA  rdata ;
INFILE  "C:/Users/z7a/Documents/DescrTab2/vignettes/validation_report/t.test/t.test_dat_1.csv" 
     DSD 
     LRECL= 10 ;
INPUT
 extra
 group
;
FORMAT group group. ;
RUN;

ods graphics off;
ods html file="t.test_example_1.html" path=".";

/*PROC UNIVARIATE data=rdata;*/
/*VAR diff;*/
/*RUN;*/

/* Type the SAS example you want to calculate here, e.g.: */

proc univariate data=rdata;
   var extra;
run;

proc ttest data=rdata;
class group;
   var extra;
run;

ods html close;
