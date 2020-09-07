* Written by R;
*  foreign::write.foreign(df = dat, datafile = here::here("vignettes",  ;

PROC FORMAT;
value A 
     1 = "F" 
     2 = "U" 
;

value B 
     1 = "F" 
     2 = "U" 
;

value C 
     1 = "F" 
     2 = "U" 
;

DATA  rdata ;
INFILE  "C:/Users/z7a/Documents/DescrTab2/vignettes/validation_report/CochraneQTest/CochraneQTest_dat_1.csv" 
     DSD 
     LRECL= 9 ;
INPUT
 A
 B
 C
;
FORMAT A A. ;
FORMAT B B. ;
FORMAT C C. ;
RUN;

ods graphics off;
ods html file="CochraneQTest_example_1.html" path=".";
ods select CochransQ;

/*PROC UNIVARIATE data=rdata;*/
/*VAR diff;*/
/*RUN;*/

/* Type the SAS example you want to calculate here, e.g.: */
proc freq data=rdata;
   tables A*B*C/agree noprint;
run;

ods html close;
