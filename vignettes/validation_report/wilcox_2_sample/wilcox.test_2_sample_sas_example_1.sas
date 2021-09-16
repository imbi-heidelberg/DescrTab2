* Written by R;
*  foreign::write.foreign(df = dat, datafile = here::here("vignettes",  ;

DATA  rdata ;
LENGTH
 group $ 4
;

INFILE  "C:/Users/z7a/Documents/DescrTab2/vignettes/validation_report/wilcox.test_2_sample/wilcox.test_2_sample_dat_1.csv" 
     DSD 
     LRECL= 15 ;
INPUT
 var
 group $ 
;
RUN;

ods graphics off;
ods html file="wilcox.test_2_sample_example_1.html" path=".";

/*PROC UNIVARIATE data=rdata;*/
/*VAR diff;*/
/*RUN;*/

/* Type the SAS example you want to calculate here, e.g.: */
proc npar1way data=rdata HL;
class group;
   var var;
   exact wilcoxon;
run;

ods html close;
