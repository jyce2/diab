
proc import out=diab
    datafile="/home/u63543840/BIOS663/diabetes.txt"
    dbms=' '
    replace;
    getnames=yes;
	run;

* Variables measured at baseline, except for y;
data diab;
	set diab;
	label age = "Age at baseline (years)"
	 sex = "Binary deidentified"
	 BMI = "Body mass index"
	 BP = 'Blood pressure (mmHg)'
	 S1 = 'Total cholesterol (mg/dL)'
	 S2 = 'Low-density lipoproteins LDL (mg/dL)'
	 S3 = 'High-density lipoproteins HDL (mg/dL)'
	 S4 = 'Total cholesterol / HDL ratio'
	 S5 = 'Log Triglyceride (mg/dL)'
	 S6 = 'Fasting blood glucose value (mg/dL)'
	 y = 'Progression of diabetes 1 year after baseline (response)'; 
run; 


	
* Total cholesterol = LDL + HDL + 20% triglyceride;
* S4 = ratio of total cholesterol / HDL;

* summary statistics;
proc means data=diab mean std min max;
run;

proc freq data=diab;
table sex /missing;
run;


* corrgram matrix check for patterns for linearity;
* simple measure of the linear relationship between x and y (and b/t x predictors).;
* H0: x and y are not linearly associated p=0. Reject H0 if p-value <0.05.;
proc corr data=diab plots(maxpoints=none)=matrix (nvar=all nwith=all histogram);
	var age sex BMI BP S1-S6 y ;
run;

/*

* partial correlations; 
*If the partial correlation is smaller than the simple correlation,
 but greater than 0, then variable 3 partly explains the correlation between X and Y.
 
 *If the partial correlation is larger than the simple correlation because after adjusting
 for variable z (grouping confounder), a stronger linear relationship may be shown bt x and y.
 Simpson' paradox.;

* After adjusting for HDL and total cholesterol, 
total cholesterol/HDL ratio is not linearly associated/correlated with y. (not surprising
since HDL and total cholesterol/HDL ratio are directly related);
proc corr data=diab nosimple;
   var y S4;
   partial S1 S3;
run;

* After adjusting for total cholesterol/ removing the linear effects of total cholesterol,
HDL is not linearly associated/correlated with y. ;
proc corr data=diab nosimple;
   var y S2;
   partial S1;
run;


* After adjusting for HDL, LDL, and triglyceride, 
total cholesterol is not linearly associated/correlated with y. (not surprising
since total cholesterol is comprised of these components and thus directly related);
proc corr data=diab nosimple;
   var y S1;
   partial S2 S3 S5;
run;

* After adjusting for BMI BP S2, S3, S5, S6, and sex
age is not linearly associated/correlated with y. 
explained by not sig. p-value in added-last model preceding rest of predictors  ;
proc corr data=diab nosimple;
   var y age;
   partial BMI BP S2 S3 S5 S6; *sex;
run;

proc corr data=diab nosimple;
   var y age BMI BP S1-S6;
   partial sex;
run;

* scatter plots drop age and sex;
* age;
proc sgplot data=diab;
	scatter x=age y=y /group=sex; *age does not appear linearly associated with y;
run;

* sex;
proc sgplot data=diab;
	vbox y /group=sex; *y does not appear to differ significantly by sex;
	yaxis grid;
run;

proc ttest; *2-sample t test to compare y means between sex;
      class sex; *pooled=assume equal variances, so 2-sample t test diff in means fail to reject H0 that means are equal between groups, p-value=0.3664;
      var y; *fail to reject H0 that variances are relatively equal between groups, p-value=0.6233;
run;
*/

* full model + studentized residual*predicted plots; 
* removed S1 = total choleterol 
* removed S4 = ratio of total cholesterol/HDL;
* redundant predictors that are related;
proc reg data=diab plots=diagnostics ; /*plot shows adj R^2 value*/
   title 'Full model';
   model y = age sex BMI BP S2 S3 S5 S6 / ;
   output out=out1 rstudent=studresid predicted=pred h=leverage;
run;


* model diagnostics suggest heteroscadescity of residual variance is present and needs improvement.
* otherwise, independence of errors, linearity with y (based on corr matrix), 
existence of finite variance, and guassian(approximately) normal errors appear to be met.;


* mean-centering regressors/predictors to fix location problems with intercept
and easier interpretbility of intercept;
* the expected value of Yi when the predictor values are set to their means, rather than 0;
proc sql;  
create table mdiab as
select *, age - mean(age) as m_age,
		  bmi - mean(bmi) as m_bmi,
		  BP - mean(BP) as m_bp,
		  S1 - mean(S1) as m_s1,
		  S2 - mean(S2) as m_s2,
		  S3 - mean(S3) as m_s3,
		  S4 - mean(S4) as m_s4,
		  S5 - mean(S5) as m_s5,
		  S6 - mean(S6) as m_s6
from diab;
quit; 

proc glm data = mdiab;
  model y= m_bmi m_bp sex m_s1 m_s2 m_s3 m_s5;
quit;

/* corrgram with adjusted mean centered selected predictors indicating linearity- r should be relatively the same as full;
ods select MatrixPlot;
proc corr data=mdiab plots(maxpoints=none)=matrix (nvar=all nwith=all histogram);
	var y bmi bp sex s1-s6;
run;
*identical;
ods select MatrixPlot;
proc corr data=mdiab plots(maxpoints=none)=matrix (nvar=all nwith=all histogram);
	var y m_bmi m_bp sex m_s1-m_s6;
run;*/

* VIF;
*eigenanalysis of all predictors test for collinearity;
* highly collinear variables= S1+S2 (total and HDL cholesterol), S3+S5 (LDL+triglyceride);
proc princomp data=mdiab;
	var BMI BP sex S1-S6;
run;

proc reg data=diab;
model y= BMI BP sex S1-S6/tol vif;
run;

* re-test for collinearity, remove highly collinear vars S1, S2;
proc reg data=mdiab;
model y= m_bmi sex m_bp m_s1-m_s6/tol vif;
output out=outp predicted=pred;
run;
ods trace off;


* plot observed by y*predictors with fitted model;


********************************;
* test for transformation of y; 
* minimize the SSE in Box-cox, lambda = 0.5;
proc transreg data=mdiab ss2 detail;
	model boxcox(y) = identity( m_bmi sex m_bp m_s1-m_s6);
run;

* transform y as sqrt(y);
data tdiab;
	set mdiab;
	yt=sqrt(y); 
run;

* refit transformed y model-impoved homogeneity of residual variance;
*ods select RStudentByPredicted;
title 'transformed res';
proc reg data=tdiab plots=rstudentbypredicted;
  model yt=m_bmi m_bp sex m_s1-m_s6;
run;

*****************;
* compare models;
proc glmselect data=tdiab;
	class sex;
	model yt=m_age m_bmi m_bp sex m_s1-m_s6 / slentry=0.005 slstay=0.005 selection=backward(select=SBC);
run;

proc glmselect data=tdiab;
	class sex;
	model yt= m_age m_bmi m_bp sex m_s1-m_s6 / slentry=0.005 slstay=0.005 selection=forward(select=SBC); 
run;

proc glmselect data=tdiab;
	class sex;
	model y= m_age m_bmi m_bp sex m_s1-m_s6 / selection=lasso; 
run;

********************;

* Final model vs. full model diagnostics;

proc reg data=tdiab plots=diagnostics;
	model yt = m_bmi m_bp sex m_s3 m_s5 / bic adjrsq;
run;

proc reg data=tdiab;
	model y = m_age m_bmi m_bp sex m_s1-m_s6 / bic adjrsq;
run;

**********************;
ODS PDF FILE="/home/u63543840/BIOS663/diab.PDF";

*Interaction effects;
proc glm data=tdiab;
class sex;
model yt = m_bmi|m_bp|sex|m_s3|m_s5 / solution;
run;

ods pdf close;



***************************;


proc reg data=tdiab;
model yt= m_bmi sex m_bp m_s3 m_s5/;
output out=outp predicted=pred;
run;
ods trace off;


symbol1 color=black i=none v=star;
symbol2 color=darkcyan v=point line=1 i=RL;

***********************************;

title 'sqrt(y)*BMI, fitting predicted model to observed values';
proc gplot data=outp;
plot y*m_BMI=1 pred*m_BMI=2/overlay;
run;

title 'sqrt(y)*BMI, fitting predicted model to observed values';
proc gplot data=outp;
plot y*m_BP=1 pred*m_BP=2/overlay;
run;
proc gplot data=outp;
plot y*sex=1 pred*sex=2/overlay; 
run;
proc gplot data=outp;
plot y*m_S3=1 pred*m_S3=2/overlay;
run;
proc gplot data=outp;
plot y*m_S5=1 pred*m_S5=2/overlay;
run;
