* Written by R;
*  foreign::write.foreign(df = dat, datafile = here::here("vignettes",  ;

DATA  rdata ;
LENGTH
 Subject $ 3
 Sex $ 6
;

INFILE  "C:/Users/z7a/Documents/DescrTab2/vignettes/validation_report/mixed/mixed_dat_1.csv" 
     DSD 
     LRECL= 27 ;
INPUT
 distance
 age
 Subject
 Sex $ 
;
RUN;

ods graphics off;
ods html file="mixed_example_1.html" path=".";
ods select all;

proc mixed data=rdata;
   class Sex Subject;
   model distance = Sex / ddfm=satterth;
   random Intercept /subject=Subject;
run;

ods html close;
