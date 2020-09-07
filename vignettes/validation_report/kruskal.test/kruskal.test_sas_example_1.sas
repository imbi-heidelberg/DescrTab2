* Written by R;
*  foreign::write.foreign(df = dat, datafile = here::here("vignettes",  ;

DATA  rdata ;
LENGTH
 group $ 7
;

INFILE  "C:/Users/z7a/Documents/DescrTab2/vignettes/validation_report/kruskal.test/kruskal.test_dat_1.csv" 
     DSD 
     LRECL= 17 ;
INPUT
 var
 group $ 
;
RUN;

ods graphics off;
ods html file="kruskal.test_example_1.html" path=".";
ods select KruskalWallisTest;

/*PROC UNIVARIATE data=rdata;*/
/*  VAR var;*/
/*RUN;*/

/* Type the SAS example you want to calculate here, e.g.: */
proc npar1way data=rdata;
	class group;
   var var;
run;

ods html close;
