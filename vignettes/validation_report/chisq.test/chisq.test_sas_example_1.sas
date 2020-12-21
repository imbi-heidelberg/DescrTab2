* Written by R;
*  foreign::write.foreign(df = dat, datafile = here::here("vignettes",  ;

DATA  rdata ;
LENGTH
 gender $ 1
 party $ 11
;

INFILE  "./chisq.test_dat_1.csv" 
     DSD 
     LRECL= 21 ;
INPUT
 gender
 party $ 
;
RUN;

ods graphics off;
ods html file="chisq.test_example_1.html" path=".";

/*PROC UNIVARIATE data=rdata;*/
/*VAR diff;*/
/*RUN;*/

/* Type the SAS example you want to calculate here, e.g.: */
proc freq data=rdata;
   tables gender party /chisq;
   tables gender*party /chisq; 
run;



data small_sample;
input a $ b $;
datalines;
0 1
0 1
1 1
1 1
0 1
0 1
0 1
0 0
0 0
0 1
1 0
;
run;

proc freq data=small_sample;
tables a*b /chisq;
run;

ods html close;
