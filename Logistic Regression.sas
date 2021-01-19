libname stats "/opt/sas/home/smbirade/sasuser.viya";
/* Fill Missing in Training Data */
data stats.insurance_t_bin;
	set stats.insurance_t_bin;
	if missing(INV) then INV=-1;
	if missing(CC) then CC=-1;
	if missing(CCPURC) then CCPURC=-1;
	if missing(HMOWN) then HMOWN=-1;
run;
/* Fix Seperation in Training Data*/
data stats.insurance_t_bin;
	set stats.insurance_t_bin;
	length CASHBK_c $2;
	length MMCRED_c $2;
	CASHBK_c = put(CASHBK, 1.);
	MMCRED_c = put(MMCRED, 1.);
	if CASHBK_c eq '1' or CASHBK_c eq '2' then CASHBK_c = '1+';
	if MMCRED_c eq '3' or MMCRED_c eq '5' then MMCRED_c = '3+';
	drop CASHBK MMCRED;
run;
/*Model 1*/ 
*Finding concordance and saving the models to an output data set;
proc logistic data=stats.insurance_t_bin ;
	class DDA(ref='0') IRA(ref='0') INV(ref='0') MM(ref='0') DDABAL_Bin(ref='1') CHECKS_Bin(ref='1')
		TELLER_Bin(ref='1') SAVBAL_Bin(ref='1') ATMAMT_Bin(ref='1') CDBAL_Bin(ref='1') / param=ref;
	model ins(event='1') = DDA IRA INV MM DDABAL_Bin CHECKS_Bin TELLER_Bin SAVBAL_Bin ATMAMT_Bin CDBAL_Bin
		DDA*IRA MM*DDABAL_Bin DDABAL_Bin*SAVBAL_Bin /clodds=pl clparm=pl;
	title 'Forward Selection Model with Significant Interactions on 10 Var Model';
	output out=Model1 p=phat;
run; 
*Percent concordant is equal to 80.2;

/*Model 2*/
*Finding concordance and saving the models to an output data set;
proc logistic data=stats.insurance_t_bin plots=none;
	class DDA(ref='0') NSF(ref='0') IRA(ref='0') ILS(ref='0') MM(ref='0') BRANCH(ref='B1') DDABAL_Bin(ref='1') CHECKS_Bin(ref='1')
		TELLER_Bin(ref='1') SAVBAL_Bin(ref='1') ATMAMT_Bin(ref='1') CDBAL_Bin(ref='1') / param=ref;
	model ins(event='1') = DDA NSF IRA ILS MM BRANCH DDABAL_Bin CHECKS_Bin TELLER_Bin SAVBAL_Bin ATMAMT_Bin CDBAL_Bin
		DDA*IRA MM*DDABAL_Bin DDABAL_Bin*SAVBAL_Bin /clodds=pl clparm=pl;
	title 'Forward Selection Model with Significant Interactions';
	output out=Model2 p=phat;
run; 
*Percent concordant is equal to 80.6;

/*Model 1 Coefficent of Discrimination*/
proc sort data=Model1;
	by descending INS;
run;

proc ttest data=Model1 order=data;
	ods select statistics summarypanel;
	class INS;
	var phat;
	title 'Coefficient of Discrimination and Plots';
run;
/*Coefficent of Discrimination is 0.2553	*/

/*Model 2 Coefficent of Discrimination*/
proc sort data=Model2;
	by descending INS;
run;

proc ttest data=Model2 order=data;
	ods select statistics summarypanel;
	class INS;
	var phat;
	title 'Coefficient of Discrimination and Plots';
run;
/*Coefficent of Discrimination is 0.2613		*/

/* Model 1 ROC Curve */
ods html select ROCCurve;
proc logistic data=stats.insurance_t_bin plots(only)=ROC ;
	class DDA(ref='0') IRA(ref='0') INV(ref='0') MM(ref='0') DDABAL_Bin(ref='1') CHECKS_Bin(ref='1')
		TELLER_Bin(ref='1') SAVBAL_Bin(ref='1') ATMAMT_Bin(ref='1') CDBAL_Bin(ref='1') / param=ref;
	model ins(event='1') = DDA IRA INV MM DDABAL_Bin CHECKS_Bin TELLER_Bin SAVBAL_Bin ATMAMT_Bin CDBAL_Bin
		DDA*IRA MM*DDABAL_Bin DDABAL_Bin*SAVBAL_Bin /clodds=pl clparm=pl;
 ods output roccurve=ROCdata3;
	title 'Model 3 ROC Curve';
	
run; 
/*Plotting the ROC Curve*/
proc sgplot data=ROCdata3 aspect=1;  
         xaxis label="False Positive Fraction" values=(0 to 1 by 0.25)
               grid offsetmin=.05 offsetmax=.05; 
         yaxis label="True Positive Fraction" values=(0 to 1 by 0.25)
              grid offsetmin=.05 offsetmax=.05;
         lineparm x=0 y=0 slope=1 / transparency=.3 lineattrs=(color=gray);
 series x=_1mspec_ y=_sensit_ ;
inset ("Area Under the ROC Curve =" = "0.803" ) / 
               border opaque position=bottomright;

title "ROC Curve for Model 3";
keylegend "mybar" / title="";

run;

/* Model 2 ROC Curve */
proc logistic data=stats.insurance_t_bin plots(only)=ROC;
	class DDA(ref='0') NSF(ref='0') IRA(ref='0') ILS(ref='0') MM(ref='0') BRANCH(ref='B1') DDABAL_Bin(ref='1') CHECKS_Bin(ref='1')
		TELLER_Bin(ref='1') SAVBAL_Bin(ref='1') ATMAMT_Bin(ref='1') CDBAL_Bin(ref='1') / param=ref;
	model ins(event='1') = DDA NSF IRA ILS MM BRANCH DDABAL_Bin CHECKS_Bin TELLER_Bin SAVBAL_Bin ATMAMT_Bin CDBAL_Bin
		DDA*IRA MM*DDABAL_Bin DDABAL_Bin*SAVBAL_Bin /clodds=pl clparm=pl;
	title 'Model 2 ROC Curve';
ods output roccurve=ROCdata2;
	
run; 
/*Plotting the ROC Curve*/
proc sgplot data=ROCdata2 aspect=1;  
         xaxis label="False Positive Fraction" values=(0 to 1 by 0.25)
               grid offsetmin=.05 offsetmax=.05; 
         yaxis label="True Positive Fraction" values=(0 to 1 by 0.25)
              grid offsetmin=.05 offsetmax=.05;
         lineparm x=0 y=0 slope=1 / transparency=.3 lineattrs=(color=gray);
 series x=_1mspec_ y=_sensit_ ;
inset ("Area Under the ROC Curve =" = "0.807" ) / 
               border opaque position=bottomright;

title "ROC Curve for Model 2";
keylegend "mybar" / title="";

run;

/*K-S Stat Finding the Best Cutoff*/
/*Model 1*/
ods html select KSTest KS2Stats EDFPlot;
proc npar1way data=Model1 d plot=edfplot;
	class INS;
	var phat;
run;
/* D stat is equal to 0.4737, and the cutoff is 0.3144*/

/*Model 2*/
ods html select KSTest KS2Stats EDFPlot;
proc npar1way data=Model2 d plot=edfplot;
	class INS;
	var phat;
run;
/* D stat is equal to 0.4793, and the cutoff is 0.327854*/
/*Now only using validation data*/
/* Fill Missing in Validation  */
data insurance_v_bin;
	set stats.insurance_v_bin;
	if missing(INV) then INV=-1;
	if missing(CC) then CC=-1;
	if missing(CCPURC) then CCPURC=-1;
	if missing(HMOWN) then HMOWN=-1;
run;
/* Fix Seperation in Validation*/
data insurance_v_bin;
	set insurance_v_bin;
	length CASHBK_c $2;
	length MMCRED_c $2;
	CASHBK_c = put(CASHBK, 1.);
	MMCRED_c = put(MMCRED, 1.);
	if CASHBK_c eq '1' or CASHBK_c eq '2' then CASHBK_c = '1+';
	if MMCRED_c eq '3' or MMCRED_c eq '5' then MMCRED_c = '3+';
	drop CASHBK MMCRED;
run;

/*Number of observations*/
proc contents data=insurance_v_bin; run;
/*2124 observation*/
/* Percent of people who both variable annuities*/
proc freq data=insurance_v_bin;
	tables INS;
run;
/*.3493 is the percent of people who purchased variable rate annutities*/



/*Calculating Lift*/
/*Lift*/
/*Model 1*/
proc logistic data=stats.insurance_t_bin plots(only)=(oddsratio)  ;
	class DDA(ref='0') IRA(ref='0') INV(ref='0') MM(ref='0') DDABAL_Bin(ref='1') CHECKS_Bin(ref='1')
		TELLER_Bin(ref='1') SAVBAL_Bin(ref='1') ATMAMT_Bin(ref='1') CDBAL_Bin(ref='1') / param=ref;
	model ins(event='1') = DDA IRA INV MM DDABAL_Bin CHECKS_Bin TELLER_Bin SAVBAL_Bin ATMAMT_Bin CDBAL_Bin
		DDA*IRA MM*DDABAL_Bin DDABAL_Bin*SAVBAL_Bin /clodds=pl clparm=pl;
    score data=insurance_v_bin fitstat outroc=roc_model1;
	
run; 

/*Calculating Lift*/
data work.roc_model1; 
	set work.roc_model1; 
	cutoff = _PROB_; 
	specif = 1-_1MSPEC_; 
	 
	depth=(_POS_+_FALPOS_)/2124 * 100;
	precision=_POS_/(_POS_+_FALPOS_); 
	acc=_POS_+_NEG_; 
	lift=precision/0.3493; 
run;
/*Plot of Lift*/ 
proc sgplot data=work.roc_model1; 
	*where 0.005 <= depth <= 0.50; 
	series y=lift x=depth; 
	refline 1.0 / axis=y; 
	title1 "Lift Chart for Training Data for Model 3"; 
	xaxis label="Depth (%)";
	yaxis label="Lift";
run; 

/*Model 2*/
proc logistic data=stats.insurance_t_bin plots(only)=(oddsratio) ;
	class DDA(ref='0') NSF(ref='0') IRA(ref='0') ILS(ref='0') MM(ref='0') BRANCH(ref='B1') DDABAL_Bin(ref='1') CHECKS_Bin(ref='1')
		TELLER_Bin(ref='1') SAVBAL_Bin(ref='1') ATMAMT_Bin(ref='1') CDBAL_Bin(ref='1') / param=ref;
	model ins(event='1') = DDA NSF IRA ILS MM BRANCH DDABAL_Bin CHECKS_Bin TELLER_Bin SAVBAL_Bin ATMAMT_Bin CDBAL_Bin
		DDA*IRA MM*DDABAL_Bin DDABAL_Bin*SAVBAL_Bin /clodds=pl clparm=pl;
	score data=insurance_v_bin fitstat outroc=roc_model2 ;
run;
/*Calculating Lift*/
data work.roc_model2; 
	set work.roc_model2; 
	cutoff = _PROB_; 
	specif = 1-_1MSPEC_; 
	
	depth=(_POS_+_FALPOS_)/2124 * 100;
	precision=_POS_/(_POS_+_FALPOS_); 
	acc=_POS_+_NEG_; 
	lift=precision/0.3493; 
run;
/*Plot of Lift*/ 
proc sgplot data=work.roc_model2; 
	*where 0.005 <= depth <= 0.50; 
	series y=lift x=depth; 
	refline 1.0 / axis=y; 
	title1 "Lift Chart for Training Data for Model 2"; 
	xaxis label="Depth (%)";
	yaxis label="Lift";
run; 
/*Accuracy*/
/*Model 1 Accuarcy and Confusion Matrix*/


proc logistic data=stats.insurance_t_bin plots(only)=(oddsratio)  ;
	class DDA(ref='0') IRA(ref='0') INV(ref='0') MM(ref='0') DDABAL_Bin(ref='1') CHECKS_Bin(ref='1')
		TELLER_Bin(ref='1') SAVBAL_Bin(ref='1') ATMAMT_Bin(ref='1') CDBAL_Bin(ref='1') / param=ref;
	model ins(event='1') = DDA IRA INV MM DDABAL_Bin CHECKS_Bin TELLER_Bin SAVBAL_Bin ATMAMT_Bin CDBAL_Bin
		DDA*IRA MM*DDABAL_Bin DDABAL_Bin*SAVBAL_Bin /clodds=pl clparm=pl ;
    score data=insurance_v_bin fitstat outroc=roc_model1 out=Model3scores;
  *ods output classification=classtable_Model1_valid;
	
run; 



*classify the predictions using the ks cutoff;
data model3scores;
	set model3scores (keep=p_1 ins);
	if p_1>0.3144 then prediction=1;
	else prediction=0;
run;

*create the final confusion matrix;
proc freq data=model3scores;
	tables ins*prediction;
run;

/*Model 1 has an accuarcy of 69.59%*/

/* Model 2 Accuarcy and Confusion Matrix*/
proc logistic data=insurance_v_bin plots=none;
	class DDA(ref='0') NSF(ref='0') IRA(ref='0') ILS(ref='0') MM(ref='0') BRANCH(ref='B1') DDABAL_Bin(ref='1') CHECKS_Bin(ref='1')
		TELLER_Bin(ref='1') SAVBAL_Bin(ref='1') ATMAMT_Bin(ref='1') CDBAL_Bin(ref='1') / param=ref;
	model ins(event='1') = DDA NSF IRA ILS MM BRANCH DDABAL_Bin CHECKS_Bin TELLER_Bin SAVBAL_Bin ATMAMT_Bin CDBAL_Bin
		DDA*IRA MM*DDABAL_Bin DDABAL_Bin*SAVBAL_Bin /ctable pprob = 0.327854;
	score data=insurance_v_bin fitstat outroc=roc_model1 out=Model2scores;
	
run;

*classify the predictions using the ks cutoff;
data model2scores;
	set model2scores (keep=p_1 ins);
	if p_1>0.3279 then prediction=1;
	else prediction=0;
run;

*create the final confusion matrix;
proc freq data=model2scores;
	tables ins*prediction;
run;

/*Model 2 has an accuarcy of 73.26%*/