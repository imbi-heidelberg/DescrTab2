* Written by R;
*  foreign::write.foreign(df = dat, datafile = here::here("vignettes",  ;

DATA  rdata ;
INFILE  "C:/Users/z7a/Documents/DescrTab2/vignettes/validation_report/wilcox.test_1_sample/wilcox.test_1_sample_dat_1.csv" 
     DSD 
     LRECL= 23 ;
INPUT
 diff
;
RUN;

ods graphics on;
ods html file="wilcox.test_1_sample_example_1.html" path=".";

/*PROC UNIVARIATE data=rdata;*/
/*VAR diff;*/
/*RUN;*/

/* Type the SAS example you want to calculate here, e.g.: */
proc capability data=rdata;
   var diff;
run;

ods html close;
