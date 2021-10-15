
*Nov 2019


** The following .ado files are needed to run this code:
** coefplot.ado 
** chitest.ado
** eststo.ado

ssc install tab_chi
ssc install coefplot
ssc install triplot


*local path="[your path]\"
local path="C:\Users\avzakharov\Dropbox\tax_experiment\tables\Tax Compliance Moscow\Replication 2021\"
cd "`path'"
set more off
use "`path'mastern_final2021_new_new.dta", clear

*********************************************************************************
********************** TABLE 1: Number of subjects **************************
*********************************************************************************

tab taxrate if period2==1&include_data==1
tab taxrate if period2==1&include_data==1&subject==1

tab taxrate if period2==1&include_data==1&baseline==1
tab taxrate if period2==1&include_data==1&subject==1&baseline==1

tab taxrate if period2==1&include_data==1&shock==1
tab taxrate if period2==1&include_data==1&subject==1&shock==1

tab taxrate if period2==1&include_data==1&status==1
tab taxrate if period2==1&include_data==1&subject==1&status==1

tab taxrate if period2==1&include_data==1&non_fixed==1
tab taxrate if period2==1&include_data==1&subject==1&non_fixed==1



*********************************************************************************
********************** TABLE 2: LYING BEHAVIOR BY COUNTRY ***********************
*********************************************************************************

tabstat declared_0 declared_f declared_1 declared_frac if include_data==1 , by(country_code) format(%9.3g)



*******************************************************************************
******************  Table B1: List of sessions ****************************
*******************************************************************************

egen subjnum=count(subject) if period2==1, by(sess_id)
li sess_id country_code taxrate subjnum if period2==1&subject==1



*******************************************************************************
*******************************************************************************
*************** TABLES: DETERMINANTS OF LYING  ***************************
*******************************************************************************
*******************************************************************************

global preh="&\multicolumn{6}{c|}{Mlogit, average marginal effects }&\multicolumn{2}{c|}{OLS}&\multicolumn{2}{c}{OLS}\\"
global ind_typenewcond="declared_cat==2"
global auditcond=0

global scals_1=`" "t2030 D20=D30" "t2040 D20=D40" "t2050 D20=D50" "t3040 D30=D40" "t3050 D30=D50" "t4050 D40=D50" "rusuk Russia=UK" "'
global scals_2=`" "t2030 D20=D30" "'
global scals_3=`" "t2030 D20=D30" "'
global scals_4=`" "t2030 D20=D30" "t2040 D20=D40" "t2050 D20=D50" "t3040 D30=D40" "t3050 D30=D50" "t4050 D40=D50" "'

global tcond_1="1.tax_20=1.tax_30 1.tax_20=1.tax_40 1.tax_20=1.tax_50 1.tax_30=1.tax_40 1.tax_30=1.tax_50 1.tax_40=1.tax_50 1.russia=1.uk"
global tcond_2="1.tax_20=1.tax_30" 
global tcond_3="1.tax_20=1.tax_30" 
global tcond_4="1.tax_20=1.tax_30 1.tax_20=1.tax_40 1.tax_20=1.tax_50 1.tax_30=1.tax_40 1.tax_30=1.tax_50 1.tax_40=1.tax_50"

global eadd_1="t2030=r(p) t2040=r(p) t2050=r(p) t3040=r(p) t3050=r(p) t4050=r(p) rusuk=r(p)"
global eadd_2="t2030=r(p)"
global eadd_3="t2030=r(p)"
global eadd_4="t2030=r(p) t2040=r(p) t2050=r(p) t3040=r(p) t3050=r(p) t4050=r(p)"

global titles="All\space{}countries Chile Russia UK"
global aplist="append append append append"

********************************************************************************
*************** TABLE 3: Determinants of lying, by period ****************
********************************************************************************

global conds "1==1 country_code==1 country_code==2 country_code==3"
global include_cond="include_data==1"
global var1="ncorrect_rank ncorrect_dev2 i.male age period2 offerdg_frac i.tax_20 i.tax_30"
global var2="i.shock i.shock_H i.status i.status_H i.non_fixed"
global var3="0.tax_20 0.tax_30 0.shock 0.shock_H 0.status 0.status_H 0.non_fixed 0.male"
global varlist_1 = "$var1 " + "i.tax_40 i.tax_50 i.deadweight i.mpcr i.russia i.uk " + "$var2"
global varlist_2 = "$var1 " + "$var2"
global varlist_3 = "$var1 " + " i.mpcr " + "$var2"
global varlist_4 = "$var1 " + "i.tax_40 i.tax_50 i.deadweight i.mpcr " + "$var2"
global varlist1_1 = "$var3 " + "0.tax_40 0.tax_50 0.mpcr 0.deadweight 0.russia 0.uk" 
global varlist1_2 = "$var3"
global varlist1_3 = "$var3 " + "0.mpcr"
global varlist1_4 = "$var3 " + "0.tax_40 0.tax_50 0.mpcr 0.deadweight"
global note = "The first three columns report average marginal effects for multinomial logistic regression (dependent variable is whether the subject declared 0\%, 100\%, or something in between, in a given round). Standard errors are clustered by subject. Fourth and fifth columns report OLS regressions, the dependent variables are the fraction and the amount of income not declared in a given round; only partial lying decisions are considered. Standard errors are clustered by subject. RET rank is the national rank, between 0 and 1, of subject's national performance at the real effort task. RET Deviation is the difference between actual number of correct additions and one predicted from subject and period FE. DG frac is the fraction of the 1000 ECU donated in the dictator game."



global i1=1
global i2=1
global fname="`path'table_reduced1_10_short_2020.tex"			  
do "`path'tables_2_2020.do"

***************************************************************************************************************
*************** TABLE D1: Determinants of lying in each period, by country  ****************
***************************************************************************************************************

global i1=2
global i2=4
global fname="`path'table_reduced1_10_2020.tex"			  
do "`path'tables_2_2020.do"



**********************************************************************************************************************************
*************** Table D6: Determinants of lying in each period, by country. Performance data from training period *
**********************************************************************************************************************************

global fname="`path'table_reduced1_10_training_2020.tex"
global conds "1==1 country_code==1 country_code==2 country_code==3"
global var1="nCorrectSumTest1 i.male age period2 offerdg_frac i.tax_20 i.tax_30"
global var2="i.shock i.shock_H i.status i.status_H i.non_fixed"
global var3="0.tax_20 0.tax_30 0.shock 0.shock_H 0.status 0.status_H 0.non_fixed 0.male"
global varlist_1 = "$var1 " + "i.tax_40 i.tax_50 i.deadweight i.mpcr i.russia i.uk " + "$var2"
global varlist_2 = "$var1 " + "$var2"
global varlist_3 = "$var1 " + " i.mpcr " + "$var2"
global varlist_4 = "$var1 " + "i.tax_40 i.tax_50 i.deadweight i.mpcr " + "$var2"
global varlist1_1 = "$var3 " + "0.tax_40 0.tax_50 0.mpcr 0.deadweight 0.russia 0.uk" 
global varlist1_2 = "$var3"
global varlist1_3 = "$var3 " + "0.mpcr"
global varlist1_4 = "$var3 " + "0.tax_40 0.tax_50 0.mpcr 0.deadweight"
global note = "The first three columns report average marginal effects for multinomial logistic regression (dependent variable is whether the subject declared 0\%,  100\%, or something in between, in a given round). Standard errors are clustered by subject. Fourth and fifth columns report OLS regressions, the dependent variables are the fraction and the amount of income not declared in a given round; only partial lying decisions are considered. Standard errors are clustered by subject. Practice period performance is the number of correct additions in the first practice period (in Russia, the only practice period). DG frac is the fraction of the 1000 ECU donated in the dictator game."
			  
global i1=2
global i2=4
do "`path'tables_2_2020.do"



	
			  			  
***************************************************************************************************************************
**************** Table 4: Determinants of lying in periods 2-10, previous action ********************************
***************************************************************************************************************************

global conds "1==1&period2>1 country_code==1&period2>1 country_code==2&period2>1 country_code==3&period2>1"
global var1="ncorrect_rank ncorrect_dev2 i.male age period2 offerdg_frac i.tax_20 i.tax_30"
global var2="i.shock i.shock_H i.status i.status_H i.non_fixed i.l0 i.lf l_declim l_others"
global var3="0.tax_20 0.tax_30 0.shock 0.shock_H 0.status 0.status_H 0.non_fixed 0.male 0.l0 0.lf"
global varlist_1 = "$var1 " + "i.tax_40 i.tax_50 i.deadweight i.mpcr i.russia i.uk " + "$var2"
global varlist_2 = "$var1 " + "$var2"
global varlist_3 = "$var1 " + " i.mpcr " + "$var2"
global varlist_4 = "$var1 " + "i.tax_40 i.tax_50 i.deadweight i.mpcr " + "$var2"
global varlist1_1 = "$var3 " + "0.tax_40 0.tax_50 0.mpcr 0.deadweight 0.russia 0.uk" 
global varlist1_2 = "$var3"
global varlist1_3 = "$var3 " + "0.mpcr"
global varlist1_4 = "$var3 " + "0.tax_40 0.tax_50 0.mpcr 0.deadweight"
global note = "The first three columns report average marginal effects for multinomial logistic regression (dependent variable is whether the subject declared 0\%, 100\%, or something in between, in a given round). Standard errors are clustered by subject. Fourth and fifth columns report OLS regressions, the dependent variables are the fraction and the amount of income not declared in a given round; only partial lying decisions are considered. Standard errors are clustered by subject. RET rank is the national rank, between 0 and 1, of subject's national performance at the real effort task. RET Deviation is the difference between actual number of correct additions and one predicted from subject and period FE. DG frac is the fraction of the 1000 ECU donated in the dictator game."

global fname="`path'table_reduced2_10_2020.tex"
global i1=1
global i2=1
do "`path'tables_2_2020.do"

*****************************************************************************************************************************************
**************** Table D4: Determinants of lying in periods 2-10, previous action, by country ********************************
*****************************************************************************************************************************************

global fname="`path'table_reduced2_10_countries_2020.tex"
global i1=2
global i2=4
do "`path'tables_2_2020.do"


********************************************************************************
**************** Table 5: Determinants of lying in period 1 **************
********************************************************************************


global conds "1==1&period2==1 country_code==1&period2==1 country_code==2&period2==1 country_code==3&period2==1"
global var1="ncorrect_rank ncorrect_dev2 i.male age offerdg_frac i.tax_20 i.tax_30"
global var2="i.shock i.shock_H i.status i.status_H i.non_fixed"
global var3="0.tax_20 0.tax_30 0.shock 0.shock_H 0.status 0.status_H 0.non_fixed 0.male"
global varlist_1 = "$var1 " + "i.tax_40 i.tax_50 i.deadweight i.mpcr i.russia i.uk " + "$var2"
global varlist_2 = "$var1 " + "$var2"
global varlist_3 = "$var1 " + " i.mpcr " + "$var2"
global varlist_4 = "$var1 " + "i.tax_40 i.tax_50 i.deadweight i.mpcr " + "$var2"
global varlist1_1 = "$var3 " + "0.tax_40 0.tax_50 0.mpcr 0.deadweight 0.russia 0.uk" 
global varlist1_2 = "$var3"
global varlist1_3 = "$var3 " + "0.mpcr"
global varlist1_4 = "$var3 " + "0.tax_40 0.tax_50 0.mpcr 0.deadweight"
global note = "The first three columns report average marginal effects for multinomial logistic regression (dependent variable is whether the subject declared 0\%, 100\%, or something in between, in a given round). Standard errors are clustered by subject. Fourth and fifth columns report OLS regressions, the dependent variables are the fraction and the amount of income not declared in a given round; only partial lying decisions are considered. We only include subjects who partially cheated in at least 8 rounds, and declarations strictly between 0\% and 100\%. Standard errors are clustered by subject. RET rank is the national rank, between 0 and 1, of subject's national performance at the real effort task. RET Deviation is the difference between actual number of correct additions and one predicted from subject and period FE. DG frac is the fraction of the 1000 ECU donated in the dictator game."

global fname="`path'table_reduced1_2020.tex"
global i1=1
global i2=1
do "`path'tables_2_2020.do"


***************************************************************************************************
**************** Table D5: Determinants of lying in period 1, by country **************
***************************************************************************************************

global fname="`path'table_reduced1_countries_2020.tex"
global i1=2
global i2=4
do "`path'tables_2_2020.do"



*********************************************************************************************************
**************** Table D10: Determinants of lying, periods 1-10, more controls *****************
*********************************************************************************************************

global include_cond="include_data_all==1"
global fname="`path'table_reduced1_10_more.tex"
global conds "1==1 country_code==1 country_code==2 country_code==3"
global var1="ncorrect_rank ncorrect_dev2 i.male age period2 offerdg_frac norms i.trust safechoices ideology income2 i.tax_20 i.tax_30"
global var2="i.shock i.shock_H i.status i.status_H i.non_fixed"
global var3="0.tax_20 0.tax_30 0.shock 0.shock_H 0.status 0.status_H 0.non_fixed 0.male 0.trust"
global varlist_1 = "$var1 " + "i.tax_40 i.tax_50 i.deadweight i.mpcr i.russia i.uk " + "$var2"
global varlist_2 = "$var1 " + "$var2"
global varlist_3 = "$var1 " + " i.mpcr " + "$var2"
global varlist_4 = "$var1 " + "i.tax_40 i.tax_50 i.deadweight i.mpcr " + "$var2"
global varlist1_1 = "$var3 " + "0.tax_40 0.tax_50 0.mpcr 0.deadweight 0.russia 0.uk" 
global varlist1_2 = "$var3"
global varlist1_3 = "$var3 " + "0.mpcr"
global varlist1_4 = "$var3 " + "0.tax_40 0.tax_50 0.mpcr 0.deadweight"
global note = "The first three columns report average marginal effects for multinomial logistic regression (dependent variable is whether the subject declared 0\%, 100\%, or something in between, in a given round). Standard errors are clustered by subject. Fourth and fifth columns report OLS regressions, the dependent variables are the fraction and the amount of income not declared in a given round; only partial lying decisions are considered. We only include subjects who partially cheated in at least 8 rounds, and declarations strictly between 0\% and 100\%. Standard errors are clustered by subject. RET rank is the national rank, between 0 and 1, of subject's national performance at the real effort task. RET Deviation is the difference between actual number of correct additions and one predicted from subject and period FE. DG frac is the fraction of the 1000 ECU donated in the dictator game. Norms is the social norms index (see Table \ref{tab:norms}). SafeChoices if the number (0-10) of safe choices on the lottery task. Trust is whether the individual answered ``Most people can be trusted'' (versus ``You can't be too careful with people''). Income is the number of the individual's income bracket, rescaled between 0 and 1 (for Chile and the UK), and the individual's perceived income decile, rescaled between 0 and 1 (for Russia)."

global fname="`path'table_reduced1_10_more_2020.tex"
global i1=1
global i2=1
do "`path'tables_2_2020.do"


*********************************************************************************************************
*********** Table D11: Determinants of lying , periods 1-10, more controls, by countries ****************
*********************************************************************************************************

global fname="`path'table_reduced1_10_more_countries_2020.tex"
global i1=2
global i2=4
do "`path'tables_2_2020.do"


***********************************************************************************************
*************** Table D9: Determinants of lying in each period, UK, DXxDG *********************
***********************************************************************************************


global conds "1==1 country_code==1 country_code==2 country_code==3"
global include_cond="include_data==1"
global var1="ncorrect_rank ncorrect_dev2 i.male age period2 offerdg_frac i.tax_20 i.tax_30"
global var2="i.shock i.shock_H i.status i.status_H i.non_fixed"
global var3="0.tax_20 0.tax_30 0.shock 0.shock_H 0.status 0.status_H 0.non_fixed 0.male"
global varlist_1 = "$var1 " + "i.tax_40 i.tax_50 i.deadweight i.mpcr i.russia i.uk " + "$var2"
global varlist_2 = "$var1 " + "$var2"
global varlist_3 = "$var1 " + " i.mpcr " + "$var2"
global varlist_4 = "$var1 " + "i.tax_40 i.tax_50 i.deadweight deadweight_dg i.mpcr " + "$var2"
global varlist1_1 = "$var3 " + "0.tax_40 0.tax_50 0.mpcr 0.deadweight 0.russia 0.uk" 
global varlist1_2 = "$var3"
global varlist1_3 = "$var3 " + "0.mpcr"
global varlist1_4 = "$var3 " + "0.tax_40 0.tax_50 0.mpcr 0.deadweight"
global note = "The first three columns report average marginal effects for multinomial logistic regression (dependent variable is whether the subject declared 0\%, 100\%, or something in between, in a given round). Standard errors are clustered by subject. Fourth and fifth columns report OLS regressions, the dependent variables are the fraction and the amount of income not declared in a given round; only partial lying decisions are considered. Standard errors are clustered by subject. RET rank is the national rank, between 0 and 1, of subject's national performance at the real effort task. RET Deviation is the difference between actual number of correct additions and one predicted from subject and period FE. DG frac is the fraction of the 1000 ECU donated in the dictator game."



global i1=4
global i2=4
global fname="`path'table_reduced1_10_ukdw_2020.tex"			  
do "`path'tables_2_2020.do"



		  
**************************************************************************************************
*************** Table D12: Determinants of lying in each period, gender interactions **************
**************************************************************************************************

foreach i in 20 30 40 50 {
	gen tax_`i'_t=tax_`i'*male
	lab var tax_`i'_t "D`i'\%xMale"
}

global fname="`path'table_reduced1_10_gender_2020.tex"
global scals_1=`" "t2030 D20=D30" "t2040 D20=D40" "t2050 D20=D50" "t3040 D30=D40" "t3050 D30=D50" "t4050 D40=D50" "e2030 D20xM=D30xM" "e2040 D20xM=D40xM" "e2050 D20xM=D50xM" "e3040 D30xM=D40xM" "e3050 D30xM=D50xM" "e4050 D40xM=D50xM" "rusuk Russia=UK" "'
global tcond_1="1.tax_20=1.tax_30 1.tax_20=1.tax_40 1.tax_20=1.tax_50 1.tax_30=1.tax_40 1.tax_30=1.tax_50 1.tax_40=1.tax_50 tax_20_t=tax_30_t tax_20_t=tax_40_t tax_20_t=tax_50_t tax_30_t=tax_40_t tax_30_t=tax_50_t tax_40_t=tax_50_t 1.russia=1.uk"
global eadd_1="t2030=r(p) t2040=r(p) t2050=r(p) t3040=r(p) t3050=r(p) t4050=r(p) e2030=r(p) e2040=r(p) e2050=r(p) e3040=r(p) e3050=r(p) e4050=r(p) rusuk=r(p)"
global titles="All\space{}countries"

global aplist="append"


global include_cond="include_data==1"
global conds "1==1"
global var1="ncorrect_rank ncorrect_dev2 male age period2 offerdg_frac i.tax_20 i.tax_30"
global var2="i.shock i.shock_H i.status i.status_H i.non_fixed"
global var3="0.tax_20 0.tax_30 0.shock 0.shock_H 0.status 0.status_H 0.non_fixed"
global varlist_1 = "$var1 " + "i.tax_40 i.tax_50 tax_20_t tax_30_t tax_40_t tax_50_t i.deadweight i.mpcr i.russia i.uk " + "$var2"
global varlist1_1 = "$var3 " + "0.tax_40 0.tax_50 0.mpcr 0.deadweight 0.russia 0.uk 1.mpcr 1.deadweight" 


global note = "The first three columns report average marginal effects for multinomial logistic regression (dependent variable is whether the subject declared 0\%, 100\%, or something in between, in a given round). Standard errors are clustered by subject. Fourth and fifth columns report OLS regressions, the dependent variables are the fraction and the amount of income not declared in a given round; only partial lying decisions are considered. We only include subjects who partially cheated in at least 8 rounds, and declarations strictly between 0\% and 100\%. Standard errors are clustered by subject. RET rank is the national rank, between 0 and 1, of subject's national performance at the real effort task. RET Deviation is the difference between actual number of correct additions and one predicted from subject and period FE. DG frac is the fraction of the 1000 ECU donated in the dictator game."

global i1=1
global i2=1
do "`path'tables_2_2020.do"



		  
**************************************************************************************************
*************** Table D13: Determinants of lying in each period, DG interactions ******************
**************************************************************************************************

foreach i in 20 30 40 50 {
	replace tax_`i'_t=tax_`i'*offerdg_frac
	lab var tax_`i'_t "D`i'\%xDG"
}

global fname="`path'table_reduced1_10_dg_2020.tex"
global scals_1=`" "t2030 D20=D30" "t2040 D20=D40" "t2050 D20=D50" "t3040 D30=D40" "t3050 D30=D50" "t4050 D40=D50" "e2030 D20xDG=D30xDG" "e2040 D20xDG=D40xDG" "e2050 D20xDG=D50xDG" "e3040 D30xDG=D40xDG" "e3050 D30xDG=D50xDG" "e4050 D40xDG=D50xDG" "rusuk Russia=UK" "'
global tcond_1="1.tax_20=1.tax_30 1.tax_20=1.tax_40 1.tax_20=1.tax_50 1.tax_30=1.tax_40 1.tax_30=1.tax_50 1.tax_40=1.tax_50 tax_20_t=tax_30_t tax_20_t=tax_40_t tax_20_t=tax_50_t tax_30_t=tax_40_t tax_30_t=tax_50_t tax_40_t=tax_50_t 1.russia=1.uk"
global eadd_1="t2030=r(p) t2040=r(p) t2050=r(p) t3040=r(p) t3050=r(p) t4050=r(p) e2030=r(p) e2040=r(p) e2050=r(p) e3040=r(p) e3050=r(p) e4050=r(p) rusuk=r(p)"
global titles="All\space{}countries"

global aplist="append"


global include_cond="include_data==1"
global conds "1==1"
global var1="ncorrect_rank ncorrect_dev2 male age period2 offerdg_frac i.tax_20 i.tax_30"
global var2="i.shock i.shock_H i.status i.status_H i.non_fixed"
global var3="0.tax_20 0.tax_30 0.shock 0.shock_H 0.status 0.status_H 0.non_fixed"
global varlist_1 = "$var1 " + "i.tax_40 i.tax_50 tax_20_t tax_30_t tax_40_t tax_50_t i.deadweight i.mpcr i.russia i.uk " + "$var2"
global varlist1_1 = "$var3 " + "0.tax_40 0.tax_50 0.mpcr 0.deadweight 0.russia 0.uk 1.mpcr 1.deadweight" 


global note = "The first three columns report average marginal effects for multinomial logistic regression (dependent variable is whether the subject declared 0\%, 100\%, or something in between, in a given round). Standard errors are clustered by subject. Fourth and fifth columns report OLS regressions, the dependent variables are the fraction and the amount of income not declared in a given round; only partial lying decisions are considered. We only include subjects who partially cheated in at least 8 rounds, and declarations strictly between 0\% and 100\%. Standard errors are clustered by subject. RET rank is the national rank, between 0 and 1, of subject's national performance at the real effort task. RET Deviation is the difference between actual number of correct additions and one predicted from subject and period FE. DG frac is the fraction of the 1000 ECU donated in the dictator game."

global i1=1
global i2=1
do "`path'tables_2_2020.do"

**************************************************************************************************
*************** Table D14: Determinants of lying in each period, RET rank interactions ************
**************************************************************************************************

foreach i in 20 30 40 50 {
	replace tax_`i'_t=tax_`i'*ncorrect_rank
	lab var tax_`i'_t "D`i'\%xRET"
}

global fname="`path'table_reduced1_10_ret_2020.tex"
global scals_1=`" "t2030 D20=D30" "t2040 D20=D40" "t2050 D20=D50" "t3040 D30=D40" "t3050 D30=D50" "t4050 D40=D50" "e2030 D20xR=D30xR" "e2040 D20xR=D40xR" "e2050 D20xR=D50xR" "e3040 D30xR=D40xR" "e3050 D30xR=D50xR" "e4050 D40xR=D50xR" "rusuk Russia=UK" "'
global tcond_1="1.tax_20=1.tax_30 1.tax_20=1.tax_40 1.tax_20=1.tax_50 1.tax_30=1.tax_40 1.tax_30=1.tax_50 1.tax_40=1.tax_50 tax_20_t=tax_30_t tax_20_t=tax_40_t tax_20_t=tax_50_t tax_30_t=tax_40_t tax_30_t=tax_50_t tax_40_t=tax_50_t 1.russia=1.uk"
global eadd_1="t2030=r(p) t2040=r(p) t2050=r(p) t3040=r(p) t3050=r(p) t4050=r(p) e2030=r(p) e2040=r(p) e2050=r(p) e3040=r(p) e3050=r(p) e4050=r(p) rusuk=r(p)"
global titles="All\space{}countries"

global aplist="append"


global include_cond="include_data==1"
global conds "1==1"
global var1="ncorrect_rank ncorrect_dev2 male age period2 offerdg_frac i.tax_20 i.tax_30"
global var2="i.shock i.shock_H i.status i.status_H i.non_fixed"
global var3="0.tax_20 0.tax_30 0.shock 0.shock_H 0.status 0.status_H 0.non_fixed"
global varlist_1 = "$var1 " + "i.tax_40 i.tax_50 tax_20_t tax_30_t tax_40_t tax_50_t i.deadweight i.mpcr i.russia i.uk " + "$var2"
global varlist1_1 = "$var3 " + "0.tax_40 0.tax_50 0.mpcr 0.deadweight 0.russia 0.uk 1.mpcr 1.deadweight" 


global note = "The first three columns report average marginal effects for multinomial logistic regression (dependent variable is whether the subject declared 0\%, 100\%, or something in between, in a given round). Standard errors are clustered by subject. Fourth and fifth columns report OLS regressions, the dependent variables are the fraction and the amount of income not declared in a given round; only partial lying decisions are considered. We only include subjects who partially cheated in at least 8 rounds, and declarations strictly between 0\% and 100\%. Standard errors are clustered by subject. RET rank is the national rank, between 0 and 1, of subject's national performance at the real effort task. RET Deviation is the difference between actual number of correct additions and one predicted from subject and period FE. DG frac is the fraction of the 1000 ECU donated in the dictator game."

global i1=1
global i2=1
do "`path'tables_2_2020.do"




**********************************************************************************************************************************
*************** Table D8: Determinants of lying in each period. Expected RET rank *
**********************************************************************************************************************************

global scals_1=`" "t2030 D20=D30" "rusuk Russia=UK" "'
global tcond_1="1.tax_20=1.tax_30 1.russia=1.uk"
global eadd_1="t2030=r(p) rusuk=r(p)"

global fname="`path'table_reduced1_10_expected_2020.tex"
global conds "1==1 country_code==1 country_code==2 country_code==3"
global var1="rank_expected i.male age period2 offerdg_frac i.tax_20 i.tax_30"
global var2=""
global var3="0.tax_20 0.tax_30 0.male"
global varlist_1 = "$var1 " + "i.russia i.uk " + "$var2"
global varlist_2 = "$var1 " + "$var2"
global varlist_3 = "$var1 " + " i.mpcr " + "$var2"
global varlist_4 = "$var1 " + "i.tax_40 i.tax_50 i.deadweight i.mpcr " + "$var2"
global varlist1_1 = "$var3 " + "0.russia 0.uk" 
global varlist1_2 = "$var3"
global varlist1_3 = "$var3 " + "0.mpcr"
global varlist1_4 = "$var3 " + "0.tax_40 0.tax_50 0.mpcr 0.deadweight"
global note = "The first three columns report average marginal effects for multinomial logistic regression (dependent variable is whether the subject declared 0\%,  100\%, or something in between, in a given round). Standard errors are clustered by subject. Fourth and fifth columns report OLS regressions, the dependent variables are the fraction and the amount of income not declared in a given round; only partial lying decisions are considered. Standard errors are clustered by subject. Practice period performance is the number of correct additions in the first practice period (in Russia, the only practice period). DG frac is the fraction of the 1000 ECU donated in the dictator game."
			  
global i1=1
global i2=1
do "`path'tables_2_2020.do"		  



**********************************************************************************************************************************
*************** Tables D24-D26: Determinants of lying in each period, Russia *
**********************************************************************************************************************************

global scals_1=`" "t2030 D20=D30" "'
global tcond_1="1.tax_20=1.tax_30"
global eadd_1="t2030=r(p)"

global fname="`path'table_reduced1_russia_trusting_2020.tex"
global conds "country_code==2"
global varlist_1="ncorrect_rank ncorrect_dev2 i.male age period2 offerdg_frac redistrib_index trusting i.tax_20 i.tax_30 i.shock i.shock_H i.status i.status_H i.non_fixed"
global varlist1_1="0.tax_20 0.tax_30 0.male 0.tax_20 0.tax_30 0.shock 0.shock_H 0.status 0.status_H 0.non_fixed"
global note = "The first three columns report average marginal effects for multinomial logistic regression (dependent variable is whether the subject declared 0\%,  100\%, or something in between, in a given round). Standard errors are clustered by subject. The fourth column reports OLS regression, the dependent variable is the fraction of income not declared in a given round; only partial lying decisions are considered. Standard errors are clustered by subject. DG frac is the fraction of the 1000 ECU donated in the dictator game."
			  
global i1=1
global i2=1
do "`path'tables_2_2020.do"	

global fname="`path'table_reduced1_russia_trusting_sec_2020.tex"
global varlist_1="ncorrect_rank ncorrect_dev2 i.male age period2 offerdg_frac redistrib_index security_index trusting i.tax_20 i.tax_30 i.shock i.shock_H i.status i.status_H i.non_fixed"
do "`path'tables_2_2020.do"	

global fname="`path'table_reduced1_russia_sss_2020.tex"
global varlist_1="ncorrect_rank ncorrect_dev2 i.male age period2 offerdg_frac own_f i.tax_20 i.tax_30 i.shock i.shock_H i.status i.status_H i.non_fixed"
do "`path'tables_2_2020.do"	





****************************************************************************************************************
************** Tables D2-D3: Determinants of lying in each period, interaction of treatment with earnings  *****
****************************************************************************************************************



lab var tax_20_inc "D20\%xE"
lab var tax_30_inc "D30\%xE"
lab var tax_40_inc "D40\%xE"
lab var tax_50_inc "D50\%xE"

global scals_1=`" "t2030 D20=D30" "t2040 D20=D40" "t2050 D20=D50" "t3040 D30=D40" "t3050 D30=D50" "t4050 D40=D50" "e2030 D20xE=D30xE" "e2040 D20xE=D40xE" "e2050 D20xE=D50xE" "e3040 D30xE=D40xE" "e3050 D30xE=D50xE" "e4050 D40xE=D50xE" "rusuk Russia=UK" "'
global scals_2=`" "t2030 D20=D30"  "e2030 D20xE=D30xE" "'
global scals_3=`" "t2030 D20=D30"  "e2030 D20xE=D30xE" "'
global scals_4=`" "t2030 D20=D30" "t2040 D20=D40" "t2050 D20=D50" "t3040 D30=D40" "t3050 D30=D50" "t4050 D40=D50" "e2030 D20xE=D30xE" "e2040 D20xE=D40xE" "e2050 D20xE=D50xE" "e3040 D30xE=D40xE" "e3050 D30xE=D50xE" "e4050 D40xE=D50xE" "'

global tcond_1="1.tax_20=1.tax_30 1.tax_20=1.tax_40 1.tax_20=1.tax_50 1.tax_30=1.tax_40 1.tax_30=1.tax_50 1.tax_40=1.tax_50 tax_20_inc=tax_30_inc tax_20_inc=tax_40_inc tax_20_inc=tax_50_inc tax_30_inc=tax_40_inc tax_30_inc=tax_50_inc tax_40_inc=tax_50_inc 1.russia=1.uk"
global tcond_2="1.tax_20=1.tax_30 tax_20_inc=tax_30_inc" 
global tcond_3="1.tax_20=1.tax_30 tax_20_inc=tax_30_inc" 
global tcond_4="1.tax_20=1.tax_30 1.tax_20=1.tax_40 1.tax_20=1.tax_50 1.tax_30=1.tax_40 1.tax_30=1.tax_50 1.tax_40=1.tax_50 tax_20_inc=tax_30_inc tax_20_inc=tax_40_inc tax_20_inc=tax_50_inc tax_30_inc=tax_40_inc tax_30_inc=tax_50_inc tax_40_inc=tax_50_inc"

global eadd_1="t2030=r(p) t2040=r(p) t2050=r(p) t3040=r(p) t3050=r(p) t4050=r(p) e2030=r(p) e2040=r(p) e2050=r(p) e3040=r(p) e3050=r(p) e4050=r(p) rusuk=r(p)"
global eadd_2="t2030=r(p) e2030=r(p)"
global eadd_3="t2030=r(p) e2030=r(p)"
global eadd_4="t2030=r(p) t2040=r(p) t2050=r(p) t3040=r(p) t3050=r(p) t4050=r(p) e2030=r(p) e2040=r(p) e2050=r(p) e3040=r(p) e3050=r(p) e4050=r(p)"

global titles="All\space{}countries Chile Russia UK"
global aplist="append append append append"

global conds "1==1 country_code==1 country_code==2 country_code==3"
global include_cond="include_data==1"
global var1="profitret i.male age period2 offerdg_frac i.tax_20 i.tax_30"
global var2="i.shock i.shock_H i.status i.status_H i.non_fixed"
global var3="0.tax_20 0.tax_30 0.shock 0.shock_H 0.status 0.status_H 0.non_fixed 1.male 1.shock 1.shock_H 1.status 1.status_H 1.non_fixed 0.male age_subject offerdg_frac"
global varlist_1 = "$var1 " + "i.tax_40 i.tax_50 tax_20_inc tax_30_inc tax_40_inc tax_50_inc i.deadweight i.mpcr i.russia i.uk " + "$var2"
global varlist_2 = "$var1 " + "tax_20_inc tax_30_inc " + "$var2"
global varlist_3 = "$var1 " + "tax_20_inc tax_30_inc i.mpcr " + "$var2"
global varlist_4 = "$var1 " + "i.tax_40 i.tax_50 tax_20_inc tax_30_inc tax_40_inc tax_50_inc i.deadweight i.mpcr " + "$var2"
global varlist1_1 = "$var3 " + "0.tax_40 0.tax_50 0.mpcr 0.deadweight 0.russia 0.uk 1.mpcr 1.deadweight" 
global varlist1_2 = "$var3"
global varlist1_3 = "$var3 " + "0.mpcr"
global varlist1_4 = "$var3 " + "0.tax_40 0.tax_50 0.mpcr 0.deadweight"
global note = "The first three columns report average marginal effects for multinomial logistic regression (dependent variable is whether the subject declared 0\%, 100\%, or something in between, in a given round). Standard errors are clustered by subject. Fourth and fifth columns report OLS regressions, the dependent variables are the fraction and the amount of income not declared in a given round; only partial lying decisions are considered. Standard errors are clustered by subject. RET earnings are the earnings in the real effort task, plus possible windfall income. Other controls are age, gender, fraction donated in the dictator game, and other experimental conditions (not shown). "

global i1=1
global i2=1
global fname="`path'table_reduced1_10_c_short_2020.tex"			  
do "`path'tables_2_2020.do"

global i1=2
global i2=4
global fname="`path'table_reduced1_10_c_2020.tex"			  
do "`path'tables_2_2020.do"




*******************************************************************************
******** Figure D3: Frequency of cheating decisions by country. ***************
*******************************************************************************

use "`path'mastern_final2021_new_new.dta", clear

keep if include_data==1&period2==1
egen nnn=count(russia), by( declared_0_n declared_1_n)
keep declared_0_n declared_1_n nnn
quietly tabstat nnn, stats(n) save
matrix l1=r(StatTotal)
local n=l1[1,1]/100
gen nfreq=nnn/`n'
format %4.1f nfreq
graph drop _all
scatter declared_0_n declared_1_n, msymbol(none) mlabel(nfreq ) mlabposition(0) xtitle(Declared 100%) ytitle(Declared 0%) title(All) name(All)

use "`path'mastern_final2021_new_new.dta", clear
keep if include_data==1&period2==1
egen nnn=count(russia), by( declared_0_n declared_1_n country_code)
keep declared_0_n declared_1_n nnn country_code
egen ccount=count(country_code), by(country_code)
gen nfreq= nnn*100/ ccount
format %4.1f nfreq
scatter declared_0_n declared_1_n if country_code==1, msymbol(none) mlabel(nfreq ) mlabposition(0) xtitle(Declared 100%) ytitle(Declared 0%) title(Chile) name(Chile)
scatter declared_0_n declared_1_n if country_code==2, msymbol(none) mlabel(nfreq ) mlabposition(0) xtitle(Declared 100%) ytitle(Declared 0%) title(Russia) name(Russia)
scatter declared_0_n declared_1_n if country_code==3, msymbol(none) mlabel(nfreq ) mlabposition(0) xtitle(Declared 100%) ytitle(Declared 0%) title(UK) name(UK)
gr combine Chile Russia UK All, note("The figures show the percent of subjects for each number of rounds with 0% and 100% declarations ")
graph export "`path'declared_freq_all.eps", as(eps) preview(off) replace



*******************************************************************************
************** Table C3: Predicted rank and actual rank in the first period ***
*******************************************************************************
use "`path'mastern_final2021_new_new.dta", clear

tab ind_typenew2  pred0 if include_data==1&period2==1, co
*ttest ncorrect_rank if include_data==1&period2==1&inlist(init_pred,1,2), by(init_pred) welch
ttest rank_withingroup if include_data==1&period2==1&inlist(pred0,1,2), by(pred0) welch
ttest rank_withingroup if include_data==1&period2==1&inlist(pred0,3,2), by(pred0) welch
ttest rank_withingroup if include_data==1&period2==1&inlist(pred0,3,4), by(pred0) welch
*tabstat rank_withingroup if period2==1&include_data==1, by(init_pred) stats(mean sd)



**********************************************************************************
************** Table C3: Predicted and actual behavior in Period 10 *******
************** Figure D2: Predicted and actual behavior in Period 10 *****
**********************************************************************************

local iters=50
set matsize 1000 // If you use an IC version of Stata or smaller, please reduce to 800.
set more off
* Set iters=50 to draw graph and iters=1000 to produce the table
tsset subj_id period2

local conds "1==1&period2>1 country_code==1&period2>1 country_code==2&period2>1 country_code==3&period2>1"
local varlist_3_all "i.tax_40 i.tax_50 i.deadweight i.mpcr i.russia i.uk" 
local varlist_3_russia "i.mpcr"
local varlist_3_uk "i.tax_40 i.tax_50 i.deadweight i.mpcr"
local varlist4="i.shock i.shock_H i.status i.status_H i.non_fixed i.l0_temp i.lf_temp l_others_temp"
local varlist = "ncorrect_rank ncorrect_dev2 i.male age period2 offerdg_frac i.tax_20 i.tax_30 "

drop p1 p2 p3 l0_temp lf_temp l_others_temp

quietly gen p1=.
quietly gen p2=.
quietly gen p3=.

quietly gen l0_temp=. 
quietly gen lf_temp=. 
quietly gen l_others_temp=.

forval ii=2/4 {
*forval ii=1/3 {
*forval ii=1/1 {
	
	quietly drop l0_temp lf_temp l_others_temp
	quietly gen l0_temp=l0
	quietly gen lf_temp=lf
	quietly gen l_others_temp=l_others
	

	local cc `: word `ii' of `conds''
	local tt `: word `ii' of `titles''
	local ap `: word `ii' of `aplist''
	estimates clear
	if `ii'==1 {
		local varlist3 `varlist_3_all' 
	} 
	else if `ii'==4 {
		local varlist3 `varlist_3_uk' 
	}
	else if `ii'==3 {
		local varlist3 `varlist_3_russia' 
	}
	else {
		local varlist3=" "
		
	}	
	quietly mlogit declared_cat `varlist' `varlist3' `varlist4' if `cc'&include_data==1, cluster(subj_id)
	
*	forval i=1/3 {
*		margins, dydx(l_others_temp ) predict(outcome(`i'))
*	}

	
forval tt=1/3 {
	matrix xx`ii'`tt'=J(3,`iters',.)
	if `tt'==2 {
		quietly replace l_others_temp=0 if `cc'
				
	}
	else if `tt'==3 {
		quietly replace l_others_temp=l.other_profitret if `cc'

	}

*local tt=1
	matrix xx`ii'`tt'=J(3,`iters',.)
	forval iiii=1/`iters' {
		di("`ii' `tt' `iiii'")
		local vl1="pred p1 p2 p3"
		forval i=1/4  {
			local vn: word `i' of `vl1'
			capture confirm variable `vn'
			if !_rc {
				quietly drop `vn'
			}	
		}
		quietly gen pred=.
		quietly replace pred=declared_cat if period2==1
		quietly gen p1=.
		quietly gen p2=.
		quietly gen p3=.

		forval iii=2/10 {
			quietly replace l0_temp=l.pred==1 if period2==`iii'
			quietly replace lf_temp=l.pred==2 if period2==`iii'
			quietly drop p1 p2 p3
			quietly predict p1, outcome(1) xb
			quietly predict p2, outcome(2) xb
			quietly predict p3, outcome(3) xb
			quietly replace p1=p1-ln(-ln(runiform()))
			quietly replace p2=p2-ln(-ln(runiform()))
			quietly replace p3=p3-ln(-ln(runiform()))
			forval i=1/3 {
				quietly replace pred=`i' if p`i'==max(p1,p2,p3)&period2==`iii'
			}
		}
		quietly tab pred if `cc'&period2==10, matcell(x)
		forval j=1/3 {
			mat xx`ii'`tt'[`j',`iiii']=x[`j',1]
		}
	}
	mata : st_matrix("B`ii'`tt'", rowsum(st_matrix("xx`ii'`tt'")))
	mat B`ii'`tt'=B`ii'`tt'/`iters'
}
}

graph drop _all

//drop C1 C2 C3 C4
local titles="All\space{}countries Chile Russia UK"
forval ii=2/4 {
	local tt `: word `ii' of `titles''
	quietly tab declared_cat if period2==10&include_data==1&country_code==`ii'-1, matcell(C5)
	mata: st_matrix("D",colsum(st_matrix("C5")))
	mat  C5=C5/D[1,1]
	mat C=xx`ii'1,xx`ii'2,xx`ii'3
	mata: st_matrix("D",colsum(st_matrix("C")))
	mat C=C/D[1,1],C5
	mat U = J(1,`iters',1),J(1,`iters',2),J(1,`iters',3),4
	mat C=C',U'
	svmat C
	triplot C1 C2 C3, grid(lpat(blank)) msize(vsmall vsmall vsmall vlarge) msym(+ + + +) separate(C4) mcolor(blue red green black) ttext(Max. lying) brtext(Par. lying) bltext(Hon.) legend(off) ltext(" ") btext(" ") rtext(" ") label(0.2(0.2)0.8) name(g`ii') title(`tt')
	quietly tabstat C1 C2 C3, stats(mean sd) by(C4) save
	mat TT`ii'=r(Stat1),r(Stat2),r(Stat3),r(Stat4)
	drop C1 C2 C3 C4

	}

gr combine g2 g3 g4, note("Each blue dot represents simulated period 10 distribution of outcomes, assuming actual declarations of other group members." "Each red dot represents simulated period 10 distribution of outcomes, assuming that other group members declared 0% of their income in each period." "Each green dot represents simulated period 10 distribution of outcomes, assuming that other group members declared 100% of their income in each period." "Black cross is the actual period 10 distribution of outcomes.") xsize(8) row(1) ysize(2.66) title("Distribution of simulated period 10 outcomes")
graph export "`path'out_of_sample.pdf", as(pdf) replace

local fname="`path'outsample_table.tex"

file open mf using "`fname'", write replace
file write mf "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}" _n
file write mf "\begin{tabular}{lp{7cm}ccc}" _n
file write mf "\hline\hline" _n
file write mf "&Assumption about declarations of other group members&Maximal lying&Partial lying&Honest\\" _n
file write mf "\hline" _n
local titles="All\space{}countries Chile Russia UK"
local mname="Actual\space{}declarations Declared\space{}0\%\space{}in\space{}each\space{}period Declared\space{}100\%\space{}in\space{}each\space{}period {\it\space{}Actual\space{}behavior\space{}in\space{}period\space{}10}"

forval ii=2/4 {
	forval iii=1/4 {
		if `iii'==1 {
			local strr `: word `ii' of `titles''
		}
		else {
			local strr=""
		}
		local strrr `: word `iii' of `mname''
		local strr="`strr'"+"&"+"`strrr'"
		forval i=1/3 {
			local cnum=(`iii'-1)*3+`i'
			*di `cnum'
			local m=round(TT`ii'[1,`cnum'],.001)
			local s=round(TT`ii'[2,`cnum'],.001)
			if `iii'!=4 { 
				local strr="`strr'" + "&`m' (`s')"
			}
			else {
				local strr="`strr'" + "&`m'"
			}
		}
	local strr="`strr'"+"\\"
	file write mf "`strr'" _n
}
file write mf "\hline" _n
}
file write mf "\multicolumn{5}{p{16cm}}{\tiny For each country, each of the rows 1-3 corresponds to the result of 1000 estimations, and reports the mean and standard deviation of the prevalence of maximal lying, partial lying, and honest behavior in Period 10. The fourth row reports the actual frequencies in Period 10.}\\" _n
file write mf "\hline\hline" _n
file write mf "\end{tabular}"
file close mf


********************************************************************************
********** Figure 2: Lying and the Die Roll Result. **********************
********************************************************************************


use "`path'mastern_final2021_new_new.dta", clear

drop if include_data_all==0

egen nnn=count(russia), by(realdie ind_typenew2)
keep ind_typenew2 realdie nnn
duplicates drop
sort realdie ind_typenew2
gen t=_n
drop if realdie==.
replace t=t+1 in 5/24
replace t=t+1 in 9/24
replace t=t+1 in 13/24
replace t=t+1 in 17/24
replace t=t+1 in 21/24
rename ind_typenew2 cc
egen nnt=sum(nnn), by(cc)
gen c0=nnn/nnt
twoway (bar c0 t if cc==1, color(black) barw(.8)) (bar c0 t if cc==2, color(black) barw(.8) fi(70))  (bar c0 t if cc==3, color(black) barw(.8) fi(50)) (bar c0 t if cc==4, color(black) barw(.8) fi(30)), xlabel(2.5 "1" 7.5 "2" 12.5 "3" 17.5 "4" 22.5 "5" 27.5 "6", noticks) legend(row(2) order(1 "Maximal liars" 2 "Partial liars" 3 "Honest" 4 "Other") size(small)) title("") xtitle("") ytitle("Fraction") yline(0.1666, lcolor(black)) note("The graph shows the relative frequencies of reported die rolls for different types of" "individuals. The horizontal line corresponds to 0.1666=1/6.")
graph export "`path'die_type.eps", as(eps) preview(off) replace

********************************************************************************
********** Figure D4:  Lying and the digital die roll result.  *****************
********************************************************************************


use "`path'mastern_final2021_new_new.dta", clear
keep if period2==1
drop if include_data_all==0
drop if digitaldie==.
drop if digitaldie_actual==-1
egen nnn=count(russia), by(digitaldie ind_typenew2)

keep ind_typenew2 digitaldie nnn
duplicates drop
sort digitaldie ind_typenew2
gen t=_n
replace t=t+1 in 5/24
replace t=t+1 in 9/24
replace t=t+1 in 13/24
replace t=t+1 in 17/24
replace t=t+1 in 21/24
rename ind_typenew2 cc
egen nnt=sum(nnn), by(cc)
gen c0=nnn/nnt
twoway (bar c0 t if cc==1, color(black) barw(.8)) (bar c0 t if cc==2, color(black) barw(.8) fi(70))  (bar c0 t if cc==3, color(black) barw(.8) fi(50)) (bar c0 t if cc==4, color(black) barw(.8) fi(30)), xlabel(2.5 "1" 7.5 "2" 12.5 "3" 17.5 "4" 22.5 "5" 27.5 "6", noticks) legend(row(1) order(1 "Maximal liars" 2 "Partial liars" 3 "Honest" 4 "Other") size(small)) title("") xtitle("") ytitle("Fraction") yline(0.1666, lcolor(black)) note("The graph shows the relative frequencies of reported digital die rolls for different types of" "individuals. The horizontal line corresponds to 0.1666=1/6.") xsize(6)
graph export "`path'ddie_type.eps", as(eps) preview(off) replace



**************************************************************************************
****************** Table D28: Lying on the digital die task **************************
**************************************************************************************

use "`path'mastern_final2021_new_new.dta", clear

*gen digitaldie_lie_max=(digitaldie==6)&inlist(digitaldie_actual,1,2,3,4)
*replace digitaldie_lie_max =. if !inlist(digitaldie_actual,1,2,3,4)

gen digitaldie_cat=.
replace digitaldie_cat=1 if digitaldie_lie_max==1
replace digitaldie_cat=2 if digitaldie_lie_nonmax==1
replace digitaldie_cat=3 if digitaldie_lie==0
tab  ind_typenew2 digitaldie_cat if inlist(ind_typenew2,1,2,3)&period2==1&include_data_all==1&inlist(digitaldie_actual,1,2,3,4)
tab  ind_typenew3 digitaldie_cat if inlist(ind_typenew3,1,2,3)&period2==1&include_data_all==1&inlist(digitaldie_actual,1,2,3,4)


**************************************************************************************
************** Table D27: Logit regression of die roll values ************************
**************************************************************************************

use "`path'mastern_final2021_new_new.dta", clear
local fname="`path'table_dieroll_pred.tex"

estimates clear
forval i=1/6 {
	logit realdie_`i' ncorrect_rank male age declared_0_n declared_f_n  russia uk if period2==1&include_data_all==1
	local lll=e(ll)
	margins, dydx(*) post
	est store m`i'
	test declared_0_n declared_f_n
	estadd scalar dec1= r(p)
	estadd scalar ll=`lll'
}
esttab m1 m2 m3 m4 m5 m6 using "`fname'", label scalars("ll L" "dec1 Declared\space{}0\%=Declared\space{}1-99\%") mtitle(1 2 3 4 5 6) drop(ncorrect_rank male age_subject) compress nonum replace note("Logistic regression, marginal coefficients. Individual controls are gender, age, and RET rank (not shown).") se star(* 0.10 ** 0.05 *** 0.01) nogap


********************************************************************************************************
********** Figure C3: Cumulative distributions of reaction times for different declarations *************
********************************************************************************************************


use "`path'mastern_final2021_new_new.dta", clear

drop time_declare_*	
gen time_declare_1=time_declare+uniform()
replace time_declare_1=. if include_data==0
	sort time_declare_1 declared_cat

	cumul time_declare_1 if declared_cat==1, gen(time_declare_cu_1)
	cumul time_declare_1 if declared_cat==2, gen(time_declare_cu_2)
	cumul time_declare_1 if declared_cat==3, gen(time_declare_cu_3)

	label variable time_declare_1 "seconds"

	twoway (line time_declare_cu_1 time_declare_1 if time_declare<31&include_data==1, c(J) lcolor(black)) (line time_declare_cu_2 time_declare_1 if time_declare<31&include_data==1, c(J) lcolor(black) lpattern(dash)) (line time_declare_cu_3 time_declare_1 if time_declare<31&include_data==1, c(J) lcolor(black) lpattern("-.")), legend(label(1 "Maximal lying") label(2 "Partial lying") label(3 "Honest") row(1) size(small)) xsize(5) note("Cumulative distribution functions of RT for different decisions") ytitle("")
	
graph export "`path'response.eps", as(eps) preview(off) replace
 
**********************************************************************************************************************
*********** Figure D5: Distribution of reaction time by country ******************************************************
**********************************************************************************************************************


graph drop _all
local tlist="Chile Russia UK"


forval iii=1/3 {
	use "`path'mastern_final2021_new_new.dta", clear
	
	local gname `: word `iii' of `tlist''
	*drop time_declare_1	
	gen time_declare_1=time_declare+uniform()
	replace time_declare_1=. if include_data==0
		sort time_declare_1 declared_cat

		cumul time_declare_1 if declared_cat==1, gen(time_declare_cu_1)
		cumul time_declare_1 if declared_cat==2, gen(time_declare_cu_2)
		cumul time_declare_1 if declared_cat==3, gen(time_declare_cu_3)

		label variable time_declare_1 "seconds"

		
			twoway (line time_declare_cu_1 time_declare_1 if time_declare<31&include_data==1, c(J) lcolor(black)) (line time_declare_cu_2 time_declare_1 if time_declare<31&include_data==1, c(J) lcolor(black) lpattern(dash)) (line time_declare_cu_3 time_declare_1 if time_declare<31&include_data==1, c(J) lcolor(black) lpattern("-.")), legend(label(1 "Maximal lying") label(2 "Partial lying") label(3 "Honest") row(1) size(small)) xsize(5) note("Cumulative distribution functions of RT for different decisions") ytitle("") name(`gname') title(`gname') 
}
gr combine Chile Russia UK, r(1) xsize(7) ysize(2) ycommon
graph export "`path'response_country.eps", as(eps) preview(off) replace




*********************************************************************************
******************* Table D29: Determinants of reaction time ********************
*********************************************************************************

use "`path'mastern_final2021_new_new.dta", clear
stset time_declare
local fname="`path'table_reactiontime.tex"
 

*forval i=1/3 {
*forval j=1/3 {
*gen declared_cat_`i'_`j'=(l.declared_cat==`i')&(declared_cat==`j')
*}
*}

*gen l_time_declare_pl=time_declare+.5
*gen l_time_declare_pl=ln( time_declare_pl)



*gen declared_cat_p1_1=(declared_cat==1)&period2==1
*gen declared_cat_p1_2=(declared_cat==2)&period2==1
*gen declared_cat_p1_3=(declared_cat==3)&period2==1

*gen declared_cat_1=declared_0
*gen declared_cat_2=declared_f

label variable declared_cat_2 "Partial lie this period"
label variable declared_cat_1 "Maximal lie this period"

label variable declared_cat_1_1 "Max. lie this and previous period"
label variable declared_cat_1_2 "Max. lie prev. period, part. lie this period"
label variable declared_cat_1_3 "Max. lie prev. period, honest this period"
label variable declared_cat_2_2 "Part. lie this and previous period"
label variable declared_cat_2_1 "Part. lie prev. period, max. lie this period"
label variable declared_cat_2_3 "Part. lie prev. period, honest this period"
label variable declared_cat_3_3 "Honest this and previous period"
label variable declared_cat_3_1 "Honest prev. period, max. lie this period"
label variable declared_cat_3_2 "Honest prev. period, part. lie this period"

label variable declared_cat_p1_1 "Maximal lie in period 1"
label variable declared_cat_p1_2 "Partial lie in period 1"
label variable declared_cat_p1_3 "Honest in period 1"

local varlist3="declared_cat_p1_1 declared_cat_p1_2 declared_cat_p1_3 declared_cat_1_1 declared_cat_1_2 declared_cat_1_3 declared_cat_2_1 declared_cat_2_2 declared_cat_2_3 declared_cat_3_1 declared_cat_3_2"
local varlist = "ncorrect_rank ncorrect_dev2 male age period2 offerdg_frac tax_20 tax_30 tax_40 tax_50 mpcr shock shock_H status status_H non_fixed"
local varlist2="1.declared_cat"


local fname="`path'table_reactiontime.tex"

estimates clear
reg l_time_declare_pl `varlist' russia uk if include_data==1, clu(subj_id) 
est store m1
reg l_time_declare_pl  `varlist' declared_cat_1 declared_cat_2 russia uk if include_data==1, clu(subj_id) 
est store m2
reg l_time_declare_pl `varlist' `varlist3' russia uk if include_data==1, clu(subj_id) 
est store m3
local note="OLS regression. Dependent variable is log reaction time. Standard errors are clustered by subject. Baseline category for subject decision in Model 2 is honest behavior in this period. Baseline category for subject decision in Model 3 is honest behavior in this and previous period."
esttab m1 m2 m3 using "`fname'", label mtitle("Model 1" "Model 2" "Model 3") wide compress nonum note(`note') se star(* 0.10 ** 0.05 *** 0.01) replace



***********************************************************************************************************************************
******************* Table D30: Parametric estimation of hazard rate, exponential distribution of reaction time ********************
***********************************************************************************************************************************

local fname="`path'table_reactiontime_c.tex"
estimates clear
streg `varlist' russia uk if include_data==1, clu(subj_id) distribution(exponential) nohr
est store m1
streg `varlist' declared_cat_1 declared_cat_2 russia uk if include_data==1, clu(subj_id) distribution(exponential) nohr
est store m2
streg `varlist' `varlist3' russia uk if include_data==1, clu(subj_id) distribution(exponential) nohr
est store m3
local note="Exponential distribution survival time model. Standard errors are clustered by subject. Baseline category for subject decision in Model 2 is honest behavior in this period. Baseline category for subject decision in Model 3 is honest behavior in this and previous period."
esttab m1 m2 m3 using "`fname'", scalars("ll L") label mtitle("Model 1" "Model 2" "Model 3") wide compress nonum note(`note') se star(* 0.10 ** 0.05 *** 0.01) replace


***********************************************************************************************************************************
******************* Table D31: Parametric estimation of hazard rate, Weibull distribution of reaction time ************************
***********************************************************************************************************************************

local fname="`path'table_reactiontime_w.tex"
estimates clear
streg `varlist' russia uk if include_data==1, clu(subj_id) distribution(weibull) nohr
est store m1
streg `varlist' declared_cat_1 declared_cat_2 russia uk if include_data==1, clu(subj_id) distribution(weibull) nohr
est store m2
streg `varlist' `varlist3' russia uk if include_data==1, clu(subj_id) distribution(weibull) nohr
est store m3
local note="Weibull distribution survival time model. Standard errors are clustered by subject. Baseline category for subject decision in Model 2 is honest behavior in this period. Baseline category for subject decision in Model 3 is honest behavior in this and previous period."
esttab m1 m2 m3 using "`fname'", scalars("ll L") label mtitle("Model 1" "Model 2" "Model 3") wide compress nonum note(`note') se star(* 0.10 ** 0.05 *** 0.01) replace



test declared_cat_p1_3= declared_cat_p1_2
test declared_cat_p1_3= declared_cat_p1_1
test declared_cat_1_2= declared_cat_1_3



********************************************************************************************************
*************** Table D18: Near-maximal cheating depending on performance ****************************** 
********************************************************************************************************

use "`path'mastern_final2021_new_new.dta", clear
global fname="`path'table_nearmax.tex"
global byvar="hightype"
global llist="Low High p"
do "`path'tables_3.do"

********************************************************************************************************
*************** Table D19: Near-maximal cheating depending on gender *********************************** 
********************************************************************************************************

use "`path'mastern_final2021_new_new.dta", clear
global fname="`path'table_nearmax_male.tex"
global byvar="male"
global llist="Female Male p"
do "`path'tables_3.do"

********************************************************************************************************
*************** Table D20: Near-maximal cheating depending on DG donation ****************************** 
********************************************************************************************************

use "`path'mastern_final2021_new_new.dta", clear
global fname="`path'table_nearmax_dg.tex"
global byvar="offerdg_0"
global llist="DG>0 DG=0 p"
do "`path'tables_3.do"





*********************************************************************************
************** TESTS IN THE TEXT ************************************************
*********************************************************************************

*\ref{stata:payment}
tabstat payment if period2==1&include_data_all==1, by(country_code) stats(min mean max)

*\ref{stata:cheatdiff}
tabchi ind_typenew2 country_code if inlist(country_code,1,2)&period2==1&include_data==1
tabchi ind_typenew2 country_code if inlist(country_code,3,2)&period2==1&include_data==1
tabchi ind_typenew2 country_code if inlist(country_code,3,1)&period2==1&include_data==1
tabchi ind_typenew3 country_code if inlist(country_code,1,2)&period2==1&include_data==1
tabchi ind_typenew3 country_code if inlist(country_code,3,2)&period2==1&include_data==1
tabchi ind_typenew3 country_code if inlist(country_code,3,1)&period2==1&include_data==1

*\ref{stata:chile_udd}
tab ind_typenew2 chile_udd if country_code==1&period2==1&include_data==1, chi2

*\label{stata:init_pred}
//drop ppp
gen ppp=inlist(pred0,1,2)
replace ppp=. if pred0==.
drop ttt
gen ttt=declared_cat==1
cc ppp ttt if period2==1&include_data==1, exact
drop ttt
gen ttt=declared_cat==2
cc ppp ttt if period2==1&include_data==1, exact

drop ppp
gen ppp=inlist(pred1,1,2)
replace ppp=. if pred1==.
drop ttt
gen ttt=declared_cat==1
cc ppp ttt if include_data==1, exact
drop ttt
gen ttt=declared_cat==2
cc ppp ttt if  include_data==1, exact


*\ref{stata:robustcheat}
drop ttt
gen ttt= ind_typenew2
replace ttt=. if ~inlist( ind_typenew2,1,3)|include_data_all==0
replace ttt=0 if ttt==3
cc ttt realdie_6 if period2==1&include_data_all==1, exact
cc ttt realdie_5 if period2==1&include_data_all==1, exact
cc ttt realdie_2 if period2==1&include_data_all==1, exact

drop ttt
gen ttt= ind_typenew3
replace ttt=. if ~inlist( ind_typenew3,1,3)|include_data_all==0
replace ttt=0 if ttt==3
cc ttt realdie_6 if period2==1&include_data_all==1, exact
cc ttt realdie_2 if period2==1&include_data_all==1, exact
cc ttt realdie_1 if period2==1&include_data_all==1, exact


*\ref{stata:robustcheat_bino}
bitest realdie_5==.16666 if ind_typenew2==3&period2==1&include_data_all==1
bitest realdie_6==.16666 if ind_typenew2==3&period2==1&include_data_all==1
bitest realdie_5==.16666 if declared_1_n==10&period2==1&include_data_all==1
bitest realdie_6==.16666 if declared_1_n==10&period2==1&include_data_all==1

*\ref{stata:robustcheat_ranksum}
* Mann-Whitney U and chi2 tests, Mostly 1-99 vs mostly 100
drop ttt
gen ttt= ind_typenew2
replace ttt=. if inlist(ttt,1,4)
ranksum realdie if period2==1&include_data_all==1, by(ttt)
tab realdie ttt if period2==1&include_data_all==1, co chi2

* Mann-Whitney U and chi2 tests, Always 1-99 vs always 100
drop ttt
gen ttt=.
replace ttt=1 if declared_f_n==10
replace ttt=2 if declared_1_n==10
ranksum realdie if period2==1&include_data_all==1, by(ttt)
tab realdie ttt if period2==1&include_data_all==1, co chi2

* Fisher exact test, reported 5, consistent partial vs consistent honest
*\label{stata:rep5}
drop ttt
gen ttt= ind_typenew2
replace ttt=. if ~inlist( ind_typenew2,2,3)
replace ttt=0 if ttt==3
cc ttt realdie_5 if period2==1&include_data_all==1, exact

drop ttt
gen ttt= ind_typenew3
replace ttt=. if ~inlist( ind_typenew3,2,3)
replace ttt=0 if ttt==3
cc ttt realdie_5 if period2==1&include_data_all==1, exact


*\label{stata:digdie}
drop tt
gen tt=ind_typenew2==1
cc tt digitaldie_lie_max if period2==1&inlist(ind_typenew2,1,2)&include_data_all==1, exact
cc tt digitaldie_lie_max if period2==1&inlist(ind_typenew2,1,3)&include_data_all==1, exact
cc tt digitaldie_lie_nonmax if period2==1&inlist(ind_typenew2,1,2)&include_data_all==1, exact
cc tt digitaldie_lie_nonmax if period2==1&inlist(ind_typenew2,1,3)&include_data_all==1, exact

drop tt
gen tt=ind_typenew2==2
cc tt digitaldie_lie_max if period2==1&inlist(ind_typenew2,2,3)&include_data_all==1, exact
cc tt digitaldie_lie_nonmax if period2==1&inlist(ind_typenew2,2,3)&include_data_all==1, exact

drop tt
gen tt=ind_typenew3==1
cc tt digitaldie_lie_max if period2==1&inlist(ind_typenew3,1,2)&include_data_all==1, exact
cc tt digitaldie_lie_max if period2==1&inlist(ind_typenew3,1,3)&include_data_all==1, exact
cc tt digitaldie_lie_nonmax if period2==1&inlist(ind_typenew3,1,2)&include_data_all==1, exact
cc tt digitaldie_lie_nonmax if period2==1&inlist(ind_typenew3,1,3)&include_data_all==1, exact

drop tt
gen tt=ind_typenew3==2
cc tt digitaldie_lie_max if period2==1&inlist(ind_typenew3,2,3)&include_data_all==1, exact
cc tt digitaldie_lie_nonmax if period2==1&inlist(ind_typenew3,2,3)&include_data_all==1, exact

*\label{stata:dg_die}
cc offerdg_0 realdie_6 if country_code==2&period2==1&include_data==1&include_data_all==1, exact
cc offerdg_0 realdie_6 if country_code==3&period2==1&include_data==1&include_data_all==1, exact

*\label{stata:rt}
tabstat time_declare_pl if period2!=.&include_data==1, by(declared_cat) stats(mean sd n max)

*\label{stata:whocheats}
drop ttt
gen ttt= ind_typenew2==1
drop hightype
gen hightype=ncorrect_rank>.5
cc hightype ttt if country_code==1&period2==1&include_data==1, exact
cc hightype ttt if country_code==2&period2==1&include_data==1, exact
cc hightype ttt if country_code==3&period2==1&include_data==1, exact
drop ttt
gen ttt= ind_typenew2==2
cc hightype ttt if country_code==1&period2==1&include_data==1, exact
cc hightype ttt if country_code==2&period2==1&include_data==1, exact
cc hightype ttt if country_code==3&period2==1&include_data==1, exact

*\label{stata:whocheats_dg}
tab ind_typenew2 offerdg_0 if include_data==1&period2==1, co
drop ttt
gen ttt=ind_typenew2==1
cc offerdg_0 ttt if country_code==1&include_data==1&period2==1, exact
cc offerdg_0 ttt if country_code==2&include_data==1&period2==1, exact
cc offerdg_0 ttt if country_code==3&include_data==1&period2==1, exact
cc offerdg_0 realdie_6 if country_code==2&include_data==1&period2==1, exact
cc offerdg_0 realdie_6 if country_code==3&include_data==1&period2==1, exact

*\label{stata:ret}
tabstat ncorrect_subjav if period2==1&include_data==1, by(country_code) stats(mean sd)
ttest ncorrect_subjav if period2==1&include_data==1&inlist(country_code,2,3), by(country_code) welch
ttest ncorrect_subjav if period2==1&include_data==1&inlist(country_code,1,3), by(country_code) welch
ttest ncorrect_subjav if period2==1&include_data==1&inlist(country_code,1,2), by(country_code) welch

*\label{stata:nearmaxfig}
//drop declared_near
gen declared_near=declared>0&declared_frac<=.2
replace declared_near=1 if declared_frac>0&declared_frac<=.2
cc hightype declared_near if country_code==1&include_data==1, exact
cc hightype declared_near if country_code==2&include_data==1, exact
cc hightype declared_near if country_code==3&include_data==1, exact


*\label{stata:declared_same}
drop declared_same ncorrect_same
tsset subj_id period2
gen declared_same=declared==l.declared
tab declared_same if period2!=.&ind_typenew2==2&include_data==1&declared_cat==2
gen ncorrect_same=ncorrectret==l.ncorrectret
tab declared_same if period2!=.&ind_typenew2==2&include_data==1&declared_cat==2&ncorrect_same==1

******************************************************************************
********* Figure C1: Distribution of average performance by country **********
******************************************************************************

local i=1
graph twoway (kdensity ncorrect_subjav if period2==1&country_code==1&include_data==1, color(dkgreen) range(0 30) bwidth(`i')) (kdensity ncorrect_subjav if period2==1&country_code==2&include_data==1, color(red) range(0 30) bwidth(`i'))  (kdensity ncorrect_subjav if period2==1&country_code==3&include_data==1, color(blue) range(0 30) bwidth(`i')), legend(row(1) order(1 "Chile" 2 "Russia" 3 "UK")) xtitle("") ytitle("") note("Distribution of the number of correct answers. Epanechnikov density, bwidth=1")
graph export "`path'ret_density.eps", as(eps) preview(off) replace


******************************************************************
******* Table C1: Determinants of subject’s average performance **
******************************************************************

use "`path'mastern_final2021_new_new.dta", clear
local fname="`path'table_ret.tex"

local note = "OLS regressions. Dependent variable is average performance over 10 rounds. DG frac is the fraction of the 1000 ECU donated in the dictator game. Norms is the social norms index (see Table \ref{tab:norms}). SafeChoices if the number (0-10) of safe choices on the lottery task. Income is the number of the individual's income bracket, rescaled between 0 and 1 (for Chile and the UK), and the individual's perceived income decile, rescaled between 0 and 1 (for Russia)."

local varlist1 = "male age_subject offerdg_frac tax_20 tax_30"
local varlist2 = "tax_40 tax_50 deadweight mpcr" 
local varlist4 = "shock status status_H non_fixed norms trust safechoices ideology income2"
local vv = "russia uk"

estimates clear
reg ncorrect_subjav `varlist1' `varlist4' if period2==1&include_data_all==1&country_code==1, robust
est store m1
reg ncorrect_subjav `varlist1' `varlist4' if period2==1&include_data_all==1&country_code==2, robust
est store m2 
reg ncorrect_subjav `varlist1' `varlist2' `varlist4' if period2==1&include_data_all==1&country_code==3, robust
est store m3 
reg ncorrect_subjav `varlist1' `varlist2' `vv' `varlist4' if period2==1&include_data_all==1, robust
est store m4
esttab m1 m2 m3 m4 using "`fname'", label mtitle("Chile" "Russia" "UK" "All") wide compress nonum replace order(`varlist1' `varlist2' `vv' `varlist4') note("OLS regression. Robust standard errors. Dependent variable is subject's average performance over 10 rounds.") se r2 star(* 0.10 ** 0.05 *** 0.01)


*****************************************************************************
******* Table C2: Determinants of subject’s performance, periods 2-10 *******
*****************************************************************************

use "`path'mastern_final2021_new_new.dta", clear
local fname="`path'table_ret_per.tex"

local note = "OLS regressions. Dependent variable is parformance in a round. Standard errors are clustered by subject. DG frac is the fraction of the 1000 ECU donated in the dictator game. Norms is the social norms index (see Table \ref{tab:norms}). SafeChoices if the number (0-10) of safe choices on the lottery task. Income is the number of the individual's income bracket, rescaled between 0 and 1 (for Chile and the UK), and the individual's perceived income decile, rescaled between 0 and 1 (for Russia)."

local varlist1 = "male age_subject period2 offerdg_frac tax_20 tax_30"
local varlist2 = "tax_40 tax_50 deadweight mpcr" 
local varlist4 = "shock l_shock status status_H non_fixed l_others norms trust safechoices ideology income2"
local vv = "russia uk"
local varlist5 = "tax_20 tax_30 tax_40 tax_50" 

estimates clear
reg ncorrectret `varlist1' `varlist4' if period2>1&include_data_all==1&country_code==1, cluster(subj_id)
est store m1
reg ncorrectret `varlist1' `varlist4' if period2>1.&include_data_all==1&country_code==2, cluster(subj_id)
est store m2 
reg ncorrectret `varlist1' `varlist2' `varlist4' if period2>1.&include_data_all==1&country_code==3, cluster(subj_id)
est store m3 
reg ncorrectret `varlist1' `varlist2' `vv' `varlist4' if period2>1.&include_data_all==1, cluster(subj_id)
est store m4
esttab m1 m2 m3 m4
esttab m1 m2 m3 m4 using "`fname'", label mtitle("Chile" "Russia" "UK" "All") drop(`varlist5') wide compress nonum replace order(`varlist1' `varlist2' `vv' `varlist4') note("`note'") r2 se star(* 0.10 ** 0.05 *** 0.01)


************************************************************************
******** Table D17: Determinants of subject’s performance, Russia*******
************************************************************************


use "`path'mastern_final2021_new_new.dta", clear
local fname="`path'table_ret_russia.tex"
local note = "OLS regressions. Dependent variable is average performance over 10 rounds in the first model, and performance in a round for the second model. Robust standard errors for first model, standard errors clustered by subject for the second model. DG frac is the fraction of the 1000 ECU donated in the dictator game. Norms is the social norms index (see Table \ref{tab:norms}). SafeChoices if the number (0-10) of safe choices on the lottery task. Trusting behavior is the trusting behavior index (see Table \ref{tab:trust}). Income is the number of the individual's income bracket, rescaled between 0 and 1 (for Chile and the UK), and the individual's perceived income decile, rescaled between 0 and 1 (for Russia)."


local varlist1 = "male age_subject offerdg_frac tax_20 tax_30 shock status status_H non_fixed mpcr norms trusting safechoices ideology income2"
local varlist2 = "male age_subject period2 offerdg_frac tax_20 tax_30 shock l_shock status status_H non_fixed mpcr l_others norms trusting safechoices ideology income2"

estimates clear
reg ncorrect_subjav `varlist1' if period2==1&include_data==1&country_code==2, robust
est store m1
reg ncorrectret `varlist2' if period2>1.&include_data==1&country_code==2, cluster(subj_id)
est store m2 
esttab m1 m2 using "`fname'", label mtitle("Average" "Per round") wide compress nonum replace order(`varlist2') note("`note'") r2 se star(* 0.10 ** 0.05 *** 0.01)



************************************************************************
************** Table D15: Components of the civicness index. ***********
************************************************************************

pca publictransport taxes drivingfast moneyfound lying accidentaldamage litter drivingalcohol jobapplication buyingstolen if include_data_all==1


************************************************************************
************** Table D16: Components of the trusting behavior index. ***
************************************************************************

pca lendmoney lendstuff dooropen if include_data_all==1


************************************************************************
************** Table D21: Components of the redistribution index. ******
************************************************************************

pca redistrib1 redistrib2 redistrib3 redistrib4 redistrib5 redistrib6 if include_data_all==1&period2==1


************************************************************************
************** Table D22: Components of the economic security index. ***
************************************************************************

pca econself nosecure econcountrypers econselfpers econlosejob if include_data_all==1&period2==1


*******************************************************************************
************** Table D23: Components of the subjective social status index. ***
*******************************************************************************

pca own1 own2 own3 own4 own5 own6 own7 own8 if include_data_all==1&period2==1


********************************************************************************
******************* D32: BALANCE TESTS *****************************************
********************************************************************************


local vli="male age offerdg_frac norms trust safechoices ideology income2" 
* offerdg_frac nCorrectSumTest1" 
local cli="Chile Russia UK"
local fname="`path'table_balance.tex"
file open mf using "`fname'", write replace

file write mf "\begin{tabular}{llcccccc}" _n
file write mf "\hline" _n
file write mf "\hline" _n
file write mf "Country&Variable&10\% deduction&20\% deduction&30\% deduction&40\% deduction&50\% deduction&Anova \$ p\$\\" _n
file write mf "\hline" _n

forval iii=1/3 {
	
	forval ii=1/8 {
		local vname: word `ii' of `vli'
		local lab: variable label `vname'
		oneway `vname' taxrate if period2==1&include_data==1&country_code==`iii'
		local p= string(Ftail(r(df_m),r(df_r),r(F)),"%9.3f")
		tabstat `vname' if period2==1&include_data==1&country_code==`iii', save by(taxrate)
		
		di "`p'"
		if `ii'==1 {
			local cname: word `iii' of `cli'
		} 
		else {
			local cname=""
		}
		if `iii'==3 {
			mat M=r(Stat1)\r(Stat2)\r(Stat3)\r(Stat4)\r(Stat5)
			forval i=1/5 {
				local m`i'=string(M[`i',1],"%9.3f")
			}
		}
		else {
			mat M=r(Stat1)\r(Stat2)\r(Stat3)
			forval i=1/3 {
				local m`i'=string(M[`i',1],"%9.3f")
			}
			local m4=""
			local m5=""
		}
		
		file write mf "`cname'&`lab'&`m1'&`m2'&`m3'&`m4'&`m5'&`p'\\" _n
	}
	file write mf "\hline" _n
}
file write mf "\hline" _n
local note="Norms is the social norms index (see Table \ref{tab:norms}). SafeChoices if the number (0-10) of safe choices on the lottery task. Trust is whether the individual answered ``Most people can be trusted'' (versus ``You can't be too careful with people''). Income is the number of the individual's income bracket, rescaled between 0 and 1 (for Chile and the UK), and the individual's perceived income decile, rescaled between 0 and 1 (for Russia)."
file write mf "&\multicolumn{7}{p{16cm}}{\footnotesize `note'}\\" _n
file write mf "\end{tabular}"
file close mf
