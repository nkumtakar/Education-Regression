Appendix:
proc import datafile='/folders/myfolders/sasuser.v94/Project1/ProjectDataZ.xlsx' dbms= xlsx 
out=ProN;
/*proc contents data=ProN; */ 
proc means data=ProN mean std clm; 
var RET_FT4; 
var C150_4_POOLED; 
var MD_EARN_WNE_P10;
var UGDS_MEN;
var UGDS_WOMEN;
run;
/*Finding the univariate data */ 
proc Univariate data=ProN; 
var RET_FT4; 
var C150_4_POOLED; 
var MD_EARN_WNE_P10; 
var UGDS_MEN;
var UGDS_WOMEN; 
run; 
 
/* generating the histograms for all variables*/  
proc Univariate data=ProN; 
histogram RET_FT4; 
histogram C150_4_POOLED; 
histogram UGDS_MEN;
histogram UGDS_WOMEN;
histogram MD_EARN_WNE_P10; 
run; 
/* When you perform the multi regression*/ 
proc reg data=ProN; 
model C150_4_POOLED= RET_FT4 UGDS_MEN UGDS_WOMEN MD_EARN_WNE_P10; 
ods graphics on;
run; 
/* Finding the scatter and best fit line of the single regression models */ 
proc sgplot data=ProN; 
title 'Retention on Graduation Rate '; 
scatter X= RET_FT4  Y=C150_4_POOLED ; 
reg X = RET_FT4  Y=C150_4_POOLED; 
run; 
proc sgplot data=ProN; 
title 'Graduate Earnings on Graduation Rate '; 
scatter X= MD_EARN_WNE_P10 Y=C150_4_POOLED ; 
reg X = MD_EARN_WNE_P10 Y=C150_4_POOLED; 
run; 
proc sgplot data=ProN; 
title '% of male students on Graduation Rate '; 
scatter X= UGDS_MEN Y=C150_4_POOLED ; 
reg X = UGDS_MEN Y=C150_4_POOLED; 
run;
proc sgplot data=ProN; 
title '% of female students on Graduation Rate '; 
scatter X= UGDS_WOMEN Y=C150_4_POOLED ; 
reg X = UGDS_WOMEN Y=C150_4_POOLED; 
run;

/*Logistic Regression*/ 
proc corr data=ProN; 
/*analysing Multicollinerity thru Proc Corr*/ 
proc reg data= ProN outvif outest=ProNest; 
/*analysing MultiCollinearity thru VIF*/
model C150_4_POOLED= RET_FT4 UGDS_MEN UGDS_WOMEN MD_EARN_WNE_P10 /vif ridge=(0.01 0.02 0.03 0.04 0.05 0.06 0.07 0.08 0.09 0.1 1.1 1.2 1.3 1.4 1.5);;
run;
/*coding the dependent variable*/
data ProN2;
set ProN;
if C150_4_POOLED > 0.50 then gcode=1;
else gcode=0;
run;
PROC LOGISTIC data= ProN2 alpha=.05
plots(only)= (effect oddsratio roc);
model gcode (event= '1') = RET_FT4 UGDS_MEN UGDS_WOMEN MD_EARN_WNE_P10
/ clodds=wald clparm=wald Rsquare;
run;
/*forward, backward, stepwise selection on logistic model*/ 
proc LOGISTIC data=ProN2; 
model gcode = RET_FT4 UGDS_MEN UGDS_WOMEN MD_EARN_WNE_P10 /selection=forward Rsquare;
run;
proc LOGISTIC data=ProN2; 
model gcode = RET_FT4 UGDS_MEN UGDS_WOMEN MD_EARN_WNE_P10 /selection=backward Rsquare; 
run;
proc LOGISTIC data=ProN2; 
model gcode = RET_FT4 UGDS_MEN UGDS_WOMEN MD_EARN_WNE_P10 /selection=stepwise Rsquare; 
run;
/*experiment with interaction term*/ 
data ProN3;
set ProN2;
Interaction= RET_FT4*MD_EARN_WNE_P10;

proc logistic data=ProN3;
model gcode= RET_FT4 MD_EARN_WNE_P10 Interaction / Rsquare;
run;
