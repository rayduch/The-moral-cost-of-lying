
*Nov 2018


** The following .ado files are needed to run this code:
** coefplot.ado 
** chitest.ado

ssc install streg
ssc install tab_chi
ssc install coefplot
ssc install triplot





local path="[Your path]\"
cd "`path'"
set more off
use "`path'mastern_final2018_new_new.dta", clear

*********************************************************************************
******************* TABLE: LYING DECISIONS ***********************************
*********************************************************************************


tab ind_typenew2 country_code if period2==1&include_data==1, co


*******************************************************************************
*******************************************************************************
************** TABLE: DETERMINANTS OF LYING *******************************
*******************************************************************************
*******************************************************************************


*******************************************************************************
******************* TABLE IN THE MAIN TEXT *********************************
*******************************************************************************

*replace age=age/10
*label variable age "Age/10"

global fname="`path'table_parttype.tex"
global fname_gr="`path'gr_parttype.eps"
global vname="ind_typenew2"
global note = "The first four columns report are average marginal effects for multinomial logistic regression (the dependent variable is whether the subject is a consistent maximal liar, consistent partial liar, is consistently honest, or none of those). The fifth column reports OLS regression, the dependent variable is the fraction of income declared, averaged across all rounds where the subject lied partially, for all subjects who lied partially in at least 8 rounds. Robust standard errors. RET rank is the national rank, between 0 and 1, of subject's national performance at the real effort task. RET Deviation is the difference between actual number of correct additions and one predicted from subject and period FE. DG frac is the fraction of the 1000 ECU donated in the dictator game."
global nnn=8
do "`path'tables_1.do"

*******************************************************************************
******************* TABLE IN THE APPENDIX **********************************
*******************************************************************************
global fname="`path'table_parttype_10.tex"
global fname_gr=0
global vname="ind_typenew3"
global note = "The first four columns report average marginal effects for multinomial logistic regression (the dependent variable is whether the subject was a maximal liar, partial liar, or honest, in all 10 rounds). Robust standard errors. The fifth column reports OLS regression, the dependent variable is the fraction of income declared, averaged across all rounds, for subjects who lied partially in every round. RET rank is the national rank, between 0 and 1, of subject's national performance at the real effort task. RET Deviation is the difference between actual number of correct additions and one predicted from subject and period FE. DG frac is the fraction of the 1000 ECU donated in the dictator game."
global nnn=10
do "`path'tables_1.do"

*******************************************************************************
*******************************************************************************


*coefplot (m1) (m2) (m3) (m4), drop(_cons) xline(0) keep(*:ncorrect_rank *:age_subject *:1.male *:1.offergd_0 *:offerdg)
*******************************************************************************
*******************************************************************************
*************** TABLES: DEPERMINANTS OF LYING  ***************************
*******************************************************************************
*******************************************************************************

global preh="&\multicolumn{6}{c|}{Mlogit, average marginal effects }&\multicolumn{2}{c}{OLS}\\"
global ind_typenewcond="ind_typenew2==2"
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
*************** PERIODS 1-10 *************************************************
********************************************************************************

global conds "1==1 country_code==1 country_code==2 country_code==3"
global include_cond="include_data==1"
global var1="ncorrect_rank ncorrect_dev2 i.male age period2 i.offerdg_0 offerdg_frac i.tax_20 i.tax_30"
global var2="i.shock i.shock_H i.status i.status_H i.non_fixed"
global var3="0.tax_20 0.tax_30 0.shock 0.shock_H 0.status 0.status_H 0.non_fixed 0.male 0.offerdg_0"
global varlist_1 = "$var1 " + "i.tax_40 i.tax_50 i.deadweight i.mpcr i.russia i.uk " + "$var2"
global varlist_2 = "$var1 " + "$var2"
global varlist_3 = "$var1 " + " i.mpcr " + "$var2"
global varlist_4 = "$var1 " + "i.tax_40 i.tax_50 i.deadweight i.mpcr " + "$var2"
global varlist1_1 = "$var3 " + "0.tax_40 0.tax_50 0.mpcr 0.deadweight 0.russia 0.uk" 
global varlist1_2 = "$var3"
global varlist1_3 = "$var3 " + "0.mpcr"
global varlist1_4 = "$var3 " + "0.tax_40 0.tax_50 0.mpcr 0.deadweight"
global note = "The first three columns report average marginal effects for multinomial logistic regression (dependent variable is whether the subject declared 0\%, 100\%, or something in between, in a given round). Standard errors are clustered by subject. The fourth column reports OLS regression, the dependent variable is the fraction of income declared in a given round. We only include subjects who partially cheated in at least 8 rounds, and declarations strictly between 0\% and 100\%. Standard errors are clustered by subject. RET rank is the national rank, between 0 and 1, of subject's national performance at the real effort task. RET Deviation is the difference between actual number of correct additions and one predicted from subject and period FE. DG frac is the fraction of the 1000 ECU donated in the dictator game."

global i1=1
global i2=1
global fname="`path'table_reduced1_10_short.tex"			  
do "`path'tables_2.do"

// ERROR starts "variable tax_40 not found"
global i1=2
global i2=4
global fname="`path'table_reduced1_10.tex"			  
do "`path'tables_2.do"



********************************************************************************
*************** PERIODS 1-10, test round performance **********************
********************************************************************************

global fname="`path'table_reduced1_10_training.tex"
global conds "1==1 country_code==1 country_code==2 country_code==3"
global var1="nCorrectSumTest1 i.male age period2 i.offerdg_0 offerdg_frac i.tax_20 i.tax_30"
global var2="i.shock i.shock_H i.status i.status_H i.non_fixed"
global var3="0.tax_20 0.tax_30 0.shock 0.shock_H 0.status 0.status_H 0.non_fixed 0.male 0.offerdg_0"
global varlist_1 = "$var1 " + "i.tax_40 i.tax_50 i.deadweight i.mpcr i.russia i.uk " + "$var2"
global varlist_2 = "$var1 " + "$var2"
global varlist_3 = "$var1 " + " i.mpcr " + "$var2"
global varlist_4 = "$var1 " + "i.tax_40 i.tax_50 i.deadweight i.mpcr " + "$var2"
global varlist1_1 = "$var3 " + "0.tax_40 0.tax_50 0.mpcr 0.deadweight 0.russia 0.uk" 
global varlist1_2 = "$var3"
global varlist1_3 = "$var3 " + "0.mpcr"
global varlist1_4 = "$var3 " + "0.tax_40 0.tax_50 0.mpcr 0.deadweight"
global note = "The first three columns report average marginal effects for multinomial logistic regression (dependent variable is whether the subject declared 0\%,  100\%, or something in between, in a given round). Standard errors are clustered by subject. The fourth column reports OLS regression, the dependent variable is the fraction of income declared in a given round. We only include subjects who partially cheated in at least 8 rounds, and declarations strictly between 0\% and 100\%. Standard errors are clustered by subject. Practice period performance is the number of correct additions in the first practice period (in Russia, the only practice period). DG frac is the fraction of the 1000 ECU donated in the dictator game."
			  
global i1=2
global i2=4
do "`path'tables_2.do"
// ERROR ends 
	
			  			  
********************************************************************************
**************** PERIODS 2-10, PAST ACTIONS ********************************
********************************************************************************

global conds "1==1&period2>1 country_code==1&period2>1 country_code==2&period2>1 country_code==3&period2>1"
global var1="ncorrect_rank ncorrect_dev2 i.male age period2 i.offerdg_0 offerdg_frac i.tax_20 i.tax_30"
global var2="i.shock i.shock_H i.status i.status_H i.non_fixed i.l0 i.lf l_declim l_others"
global var3="0.tax_20 0.tax_30 0.shock 0.shock_H 0.status 0.status_H 0.non_fixed 0.male 0.offerdg_0 0.l0 0.lf"
global varlist_1 = "$var1 " + "i.tax_40 i.tax_50 i.deadweight i.mpcr i.russia i.uk " + "$var2"
global varlist_2 = "$var1 " + "$var2"
global varlist_3 = "$var1 " + " i.mpcr " + "$var2"
global varlist_4 = "$var1 " + "i.tax_40 i.tax_50 i.deadweight i.mpcr " + "$var2"
global varlist1_1 = "$var3 " + "0.tax_40 0.tax_50 0.mpcr 0.deadweight 0.russia 0.uk" 
global varlist1_2 = "$var3"
global varlist1_3 = "$var3 " + "0.mpcr"
global varlist1_4 = "$var3 " + "0.tax_40 0.tax_50 0.mpcr 0.deadweight"
global note = "The first three columns report average marginal effects for multinomial logistic regression (dependent variable is whether the subject declared 0\%, 100\%, or something in between, in a given round). Standard errors are clustered by subject. The fourth column reports OLS regression, the dependent variable is the fraction of income declared in a given round. We only include subjects who partially cheated in at least 8 rounds, and declarations strictly between 0\% and 100\%. Standard errors are clustered by subject. RET rank is the national rank, between 0 and 1, of subject's national performance at the real effort task. RET Deviation is the difference between actual number of correct additions and one predicted from subject and period FE. DG frac is the fraction of the 1000 ECU donated in the dictator game."

global fname="`path'table_reduced2_10.tex"
global i1=1
global i2=1
do "`path'tables_2.do"

global fname="`path'table_reduced2_10_countries.tex"
global i1=2
global i2=4
do "`path'tables_2.do"

********************************************************************************
**************** PERIOD 1 ****************************************************
********************************************************************************

// Error starts "variable tax_40 not found"

global conds "1==1&period2==1 country_code==1&period2==1 country_code==2&period2==1 country_code==3&period2==1"
global var1="ncorrect_rank ncorrect_dev2 i.male age i.offerdg_0 offerdg_frac i.tax_20 i.tax_30"
global var2="i.shock i.shock_H i.status i.status_H i.non_fixed"
global var3="0.tax_20 0.tax_30 0.shock 0.shock_H 0.status 0.status_H 0.non_fixed 0.male 0.offerdg_0"
global varlist_1 = "$var1 " + "i.tax_40 i.tax_50 i.deadweight i.mpcr i.russia i.uk " + "$var2"
global varlist_2 = "$var1 " + "$var2"
global varlist_3 = "$var1 " + " i.mpcr " + "$var2"
global varlist_4 = "$var1 " + "i.tax_40 i.tax_50 i.deadweight i.mpcr " + "$var2"
global varlist1_1 = "$var3 " + "0.tax_40 0.tax_50 0.mpcr 0.deadweight 0.russia 0.uk" 
global varlist1_2 = "$var3"
global varlist1_3 = "$var3 " + "0.mpcr"
global varlist1_4 = "$var3 " + "0.tax_40 0.tax_50 0.mpcr 0.deadweight"
global note = "The first three columns report average marginal effects for multinomial logistic regression (dependent variable is whether the subject declared 0\%, 100\%, or something in between, in a given round). Standard errors are clustered by subject. The fourth column reports OLS regression, the dependent variable is the fraction of income declared in a given round. We only include subjects who partially cheated in at least 8 rounds, and declarations strictly between 0\% and 100\%. Standard errors are clustered by subject. RET rank is the national rank, between 0 and 1, of subject's national performance at the real effort task. RET Deviation is the difference between actual number of correct additions and one predicted from subject and period FE. DG frac is the fraction of the 1000 ECU donated in the dictator game."

global fname="`path'table_reduced1.tex"
global i1=1
global i2=1
do "`path'tables_2.do"

global fname="`path'table_reduced1_countries.tex"
global i1=2
global i2=4
do "`path'tables_2.do"
// ERROR ends 


********************************************************************************
**************** PERIODS 1-10,  MORE CONTROLS ******************************
********************************************************************************
global include_cond="include_data_all==1"
global fname="`path'table_reduced1_10_more.tex"
global conds "1==1 country_code==1 country_code==2 country_code==3"
global var1="ncorrect_rank ncorrect_dev2 i.male age period2 i.offerdg_0 offerdg_frac norms i.trust safechoices ideology income2 i.tax_20 i.tax_30"
global var2="i.shock i.shock_H i.status i.status_H i.non_fixed"
global var3="0.tax_20 0.tax_30 0.shock 0.shock_H 0.status 0.status_H 0.non_fixed 0.male 0.offerdg_0 0.trust"
global varlist_1 = "$var1 " + "i.tax_40 i.tax_50 i.deadweight i.mpcr i.russia i.uk " + "$var2"
global varlist_2 = "$var1 " + "$var2"
global varlist_3 = "$var1 " + " i.mpcr " + "$var2"
global varlist_4 = "$var1 " + "i.tax_40 i.tax_50 i.deadweight i.mpcr " + "$var2"
global varlist1_1 = "$var3 " + "0.tax_40 0.tax_50 0.mpcr 0.deadweight 0.russia 0.uk" 
global varlist1_2 = "$var3"
global varlist1_3 = "$var3 " + "0.mpcr"
global varlist1_4 = "$var3 " + "0.tax_40 0.tax_50 0.mpcr 0.deadweight"
global note = "The first three columns report average marginal effects for multinomial logistic regression (dependent variable is whether the subject declared 0\%, 100\%, or something in between, in a given round). Standard errors are clustered by subject. The fourth column reports OLS regression, the dependent variable is the fraction of income declared in a given round. We only include subjects who partially cheated in at least 8 rounds, and declarations strictly between 0\% and 100\%. Standard errors are clustered by subject. RET rank is the national rank, between 0 and 1, of subject's national performance at the real effort task. RET Deviation is the difference between actual number of correct additions and one predicted from subject and period FE. DG frac is the fraction of the 1000 ECU donated in the dictator game. Norms is the social norms index (see Table \ref{tab:norms}). SafeChoices if the number (0-10) of safe choices on the lottery task. Trust is whether the individual answered ``Most people can be trusted'' (versus ``You can't be too careful with people''). Income is the number of the individual's income bracket, rescaled between 0 and 1 (for Chile and the UK), and the individual's perceived income decile, rescaled between 0 and 1 (for Russia)."

global fname="`path'table_reduced1_10_more.tex"
global i1=1
global i2=1
do "`path'tables_2.do"

// Error starts "variable tax_40 not found"


global fname="`path'table_reduced1_10_more_countries.tex"
global i1=2
global i2=4
do "`path'tables_2.do"

********************************************************************************
*************** PERIODS 1-10, BY BASELINE/STATUS/SHOCK/NONFIXED *********
********************************************************************************

global fname="`path'table_reduced1_10_treat.tex"
global scals_1=`" "t2030 D20=D30" "t2050 D20=D50" "t3050 D30=D50" "rusuk Russia=UK" "'
global scals_2=`" "t2030 D20=D30" "rusuk Russia=UK" "'
global scals_3=`" "t2030 D20=D30" "rusuk Russia=UK" "'
global scals_4=`" "t2030 D20=D30" "rusuk Russia=UK" "'
global tcond_1="1.tax_20=1.tax_30 1.tax_20=1.tax_50 1.tax_30=1.tax_50 1.russia=1.uk"
global tcond_2="1.tax_20=1.tax_30 1.russia=1.uk"
global tcond_3="1.tax_20=1.tax_30 1.russia=1.uk"
global tcond_4="1.tax_20=1.tax_30 1.russia=1.uk"

global eadd_1="t2030=r(p) t2050=r(p) t3050=r(p) rusuk=r(p)"
global eadd_2="t2030=r(p) rusuk=r(p)"
global eadd_3="t2030=r(p) rusuk=r(p)"
global eadd_4="t2030=r(p) rusuk=r(p)"

global titles="Baseline Status Shock Non-fixed"
global aplist="append append append append"


global conds "baseline==1&mpcr==0&deadweight==0 status==1 shock==1 non_fixed==1"
global var1="ncorrect_rank ncorrect_dev2 i.male age period2 i.offerdg_0 offerdg_frac i.tax_20 i.tax_30"
global var3="0.tax_20 0.tax_30 0.male 0.offerdg_0 0.russia 0.uk"
global varlist_1 = "$var1 " + "i.tax_50 i.russia i.uk " 
global varlist_2 = "$var1 " + "i.status_H i.russia i.uk " 
global varlist_3 = "$var1 " + "i.shock_H i.russia i.uk " 
global varlist_4 = "$var1 " + "i.russia i.uk " 
global varlist1_1 = "$var3 " + "0.tax_50" 
global varlist1_2 = "$var3 " + "0.status_H" 
global varlist1_3 = "$var3 " + "0.shock_H" 
global varlist1_4 = "$var3" 
global note = "The first three columns report average marginal effects for multinomial logistic regression (dependent variable is whether the subject declared 0\%, 100\%, or something in between, in a given round). Standard errors are clustered by subject. The fourth column reports OLS regression, the dependent variable is the fraction of income declared in a given round. We only include subjects who partially cheated in at least 8 rounds, and declarations strictly between 0\% and 100\%. Standard errors are clustered by subject. RET rank is the national rank, between 0 and 1, of subject's national performance at the real effort task. RET Deviation is the difference between actual number of correct additions and one predicted from subject and period FE. DG frac is the fraction of the 1000 ECU donated in the dictator game."
			  	

global i1=1
global i2=4
do "`path'tables_2.do"

		  
********************************************************************************
*************** PERIODS 1-10, MALES AND FEMALES ********************************
********************************************************************************

global fname="`path'table_reduced1_10_gender.tex"
global scals_1=`" "t2030 D20=D30" "t2040 D20=D40" "t2050 D20=D50" "t3040 D30=D40" "t3050 D30=D50" "t4050 D40=D50" "rusuk Russia=UK" "'
global scals_2=`" "t2030 D20=D30" "t2040 D20=D40" "t2050 D20=D50" "t3040 D30=D40" "t3050 D30=D50" "t4050 D40=D50" "rusuk Russia=UK" "'
global tcond_1="1.tax_20=1.tax_30 1.tax_20=1.tax_40 1.tax_20=1.tax_50 1.tax_30=1.tax_40 1.tax_30=1.tax_50 1.tax_40=1.tax_50 1.russia=1.uk"
global tcond_2="1.tax_20=1.tax_30 1.tax_20=1.tax_40 1.tax_20=1.tax_50 1.tax_30=1.tax_40 1.tax_30=1.tax_50 1.tax_40=1.tax_50 1.russia=1.uk"
global eadd_1="t2030=r(p) t2040=r(p) t2050=r(p) t3040=r(p) t3050=r(p) t4050=r(p) rusuk=r(p)"
global eadd_2="t2030=r(p) t2040=r(p) t2050=r(p) t3040=r(p) t3050=r(p) t4050=r(p) rusuk=r(p)"
global titles="All\space{}countries,\space{}females All\space{}countries,\space{}males"

global aplist="append append"


global conds "male==0 male==1"
global var1="ncorrect_rank ncorrect_dev2 age period2 i.offerdg_0 offerdg_frac i.tax_20 i.tax_30"
global var2="i.shock i.shock_H i.status i.status_H i.non_fixed"
global var3="0.tax_20 0.tax_30 0.shock 0.shock_H 0.status 0.status_H 0.non_fixed 0.offerdg_0"
global varlist_1 = "$var1 " + "i.tax_40 i.tax_50 i.deadweight i.mpcr i.russia i.uk " + "$var2"
global varlist_2 = "$var1 " + "i.tax_40 i.tax_50 i.deadweight i.mpcr i.russia i.uk " + "$var2"
global varlist1_1 = "$var3 " + "0.tax_40 0.tax_50 0.mpcr 0.deadweight 0.russia 0.uk" 
global varlist1_2 = "$var3 " + "0.tax_40 0.tax_50 0.mpcr 0.deadweight 0.russia 0.uk" 
global note = "The first three columns report average marginal effects for multinomial logistic regression (dependent variable is whether the subject declared 0\%, 100\%, or something in between, in a given round). Standard errors are clustered by subject. The fourth column reports OLS regression, the dependent variable is the fraction of income declared in a given round. We only include subjects who partially cheated in at least 8 rounds, and declarations strictly between 0\% and 100\%. Standard errors are clustered by subject. RET rank is the national rank, between 0 and 1, of subject's national performance at the real effort task. RET Deviation is the difference between actual number of correct additions and one predicted from subject and period FE. DG frac is the fraction of the 1000 ECU donated in the dictator game."

global i1=1
global i2=2
do "`path'tables_2.do"
		  

// ERROR ends 

*******************************************************************************
******************** APPENDIX ***********************************************
*******************************************************************************

*******************************************************************************
******** FIGURE: DISTRIBUTION OF DECLARATIONS BY COUNTRY ***************
*******************************************************************************

use "`path'mastern_final2018_new_new.dta", clear

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

use "`path'mastern_final2018_new_new.dta", clear
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
graph export "`path'\declared_freq_all.eps", as(eps) preview(off) replace



**************************************************************************************************
***************** FIGURE: DISTRIBUTION OF BEHAVIOR TYPE BY DG DONATIONS ******************
**************************************************************************************************

use "`path'mastern_final2018_new_new.dta", clear

keep if include_data==1&period2==1
collapse (count) c0=russia, by(country_code ind_typenew2 offerdg_0)
rename offerdg_0 hightype
gsort country_code ind_typenew2 -hightype
gen t=_n
replace t=t+1 in 9/24
replace t=t+1 in 17/24

rename ind_typenew2 cc

egen nntot=sum(c0), by(country_code hightype)
replace c0=c0/nntot
generate hi = c0 + invttail(n-1,0.025)*(sqrt(c0*(1-c0)) / sqrt(nntot))
generate lo = max(c0 - invttail(n-1,0.025)*(sqrt(c0*(1-c0)) / sqrt(nntot)),0)

rename hightype type
twoway (bar c0 t if type==1&cc==1, color(red) barw(.8)) (bar c0 t if type==0&cc==1, color(red) barw(.8) fi(50)) (bar c0 t if type==1&cc==2, color(dkgreen) barw(.8)) (bar c0 t if type==0&cc==2, color(dkgreen) barw(.8) fi(50)) (bar c0 t if type==1&cc==3, color(blue) barw(.8)) (bar c0 t if type==0&cc==3, color(blue) barw(.8) fi(50)) (bar c0 t if type==1&cc==4, color(gray) barw(.8)) (bar c0 t if type==0&cc==4, color(gray) barw(.8) fi(50)) (rcap hi lo t, color(black)), xlabel(4.5 "Chile" 13.5 "Russia" 22.5 "UK", noticks) xsize(6) ylabel(0(.2)1) legend(row(2) order(1 "Consistent maximal" 3 "Consistent partial" 5 "Consistent honest" 7 "Other") size(small)) note("Distribution of behavior types by dictator game donations" "Dark shades correspond to subjects with DG=0, light shapes - to subjects with DG>0") xtitle("")
graph export "`path'ind_typenew2_dg0.eps", as(eps) preview(off) replace


*******************************************************************************
************** TABLE: INITIAL PREDICTION OF GROUP RANK ******************
*******************************************************************************
use "`path'mastern_final2018_new_new.dta", clear

tab ind_typenew2  pred0 if include_data==1&period2==1, co
*ttest ncorrect_rank if include_data==1&period2==1&inlist(init_pred,1,2), by(init_pred) welch
ttest rank_withingroup if include_data==1&period2==1&inlist(pred0,1,2), by(pred0) welch
ttest rank_withingroup if include_data==1&period2==1&inlist(pred0,3,2), by(pred0) welch
ttest rank_withingroup if include_data==1&period2==1&inlist(pred0,3,4), by(pred0) welch
*tabstat rank_withingroup if period2==1&include_data==1, by(init_pred) stats(mean sd)



*******************************************************************************
************** TABLE: PREDICTED AND ACTUAL BEHAVIOR IN PERIOD 10 *******
************** FIGURE: PREDICTED AND ACTUAL BEHAVIOR IN PERIOD 10 ******
*******************************************************************************

local iters=50
set matsize 800
set more off
* Set iters=50 to draw graph
tsset subj_id period2

local conds "1==1&period2>1 country_code==1&period2>1 country_code==2&period2>1 country_code==3&period2>1"
local varlist_3_all "i.tax_40 i.tax_50 i.deadweight i.mpcr i.russia i.uk" 
local varlist_3_russia "i.mpcr"
local varlist_3_uk "i.tax_40 i.tax_50 i.deadweight i.mpcr"
local varlist4="i.shock i.shock_H i.status i.status_H i.non_fixed i.l0_temp i.lf_temp l_others_temp"
local varlist = "ncorrect_rank ncorrect_dev2 i.male age period2 i.offerdg_0 offerdg_frac i.tax_20 i.tax_30 "

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
********** FIGURE: DIE ROLL REPORTED BY BEHAVIORAL TYPE ************************
********************************************************************************


use "`path'mastern_final2018_new_new.dta", clear
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
twoway (bar c0 t if cc==1, color(red) barw(.8)) (bar c0 t if cc==2, color(dkgreen) barw(.8))  (bar c0 t if cc==3, color(blue) barw(.8)) (bar c0 t if cc==4, color(gray) barw(.8)), xlabel(2.5 "1" 7.5 "2" 12.5 "3" 17.5 "4" 22.5 "5" 27.5 "6", noticks) legend(row(2) order(1 "Consistent maximal" 2 "Consistent partial" 3 "Consistent honest" 4 "Other") size(small)) title("") xtitle("") ytitle("Fraction") yline(0.1666, lcolor(black)) note("The graph shows the relative frequencies of reported die rolls for different behavioral types." "The horizontal line corresponds to 0.1666=1/6.")
graph export "`path'die_type.eps", as(eps) preview(off) replace

********************************************************************************
********** FIGURE: DIGITAL DIE ROLL REPORTED BY BEHAVIORAL TYPE ********
********************************************************************************


use "`path'mastern_final2018_new_new.dta", clear
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
twoway (bar c0 t if cc==1, color(red) barw(.8)) (bar c0 t if cc==2, color(dkgreen) barw(.8))  (bar c0 t if cc==3, color(blue) barw(.8)) (bar c0 t if cc==4, color(gray) barw(.8)), xlabel(2.5 "1" 7.5 "2" 12.5 "3" 17.5 "4" 22.5 "5" 27.5 "6", noticks) legend(row(2) order(1 "Consistent maximal" 2 "Consistent partial" 3 "Consistent honest" 4 "Other") size(small)) title("") xtitle("") ytitle("Fraction") yline(0.1666, lcolor(black)) note("The graph shows the relative frequencies of reported digital die rolls for different behavioral types." "The horizontal line corresponds to 0.1666=1/6.")
graph export "`path'ddie_type.eps", as(eps) preview(off) replace



**************************************************************************************
************** TABLE: PREDICTING DIE ROLL FROM BEHAVIOR TYPE ******************
**************************************************************************************

use "`path'mastern_final2018_new_new.dta", clear
local fname="`path'table_dieroll_pred.tex"

estimates clear
forval i=1/6 {
	logit realdie_`i' ncorrect_rank male age ind_typenew2_1 ind_typenew2_2 declared_part_av1 ind_typenew2_4 russia uk if period2==1&include_data_all==1
	margins, dydx(*) post
	est store m`i'
}
esttab m1 m2 m3 m4 m5 m6 using "`fname'", label mtitle(1 2 3 4 5 6) drop(ncorrect_rank male age_subject) compress nonum replace note("Logistic regression, marginal coefficients. Individual controls not shown. Average fraction declared is shown for partial liars") se star(* 0.10 ** 0.05 *** 0.01) nogap


********************************************************************************************************
***********FIGURE: CUMULATIVE DISTRIBUTION OF REACTION TIME FOR DIFFERENT DECLARATIONS ******
********************************************************************************************************


use "`path'mastern_final2018_new_new.dta", clear

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
***********FIGURE: CUMULATIVE DISTRIBUTION OF REACTION TIME FOR DIFFERENT DECLARATIONS, BY COUNTRY ******
**********************************************************************************************************************


graph drop _all
local tlist="Chile Russia UK"


forval iii=1/3 {
	use "`path'mastern_final2018_new_new.dta", clear
	
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
gr combine Chile Russia UK, r(1) xsize(5) ysize(2)
graph export "`path'response_country.eps", as(eps) preview(off) replace


*********************************************************************************
******************* TABLE: DETERMINANTS OF REACTION TIME *******************
*********************************************************************************

use "`path'mastern_final2018_new_new.dta", clear
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
local varlist = "ncorrect_rank ncorrect_dev2 male age period2 offerdg_0 offerdg_frac tax_20 tax_30 tax_40 tax_50 mpcr shock shock_H status status_H non_fixed"
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

local fname="`path'table_reactiontime_c.tex"
estimates clear
streg `varlist' russia uk if include_data==1, clu(subj_id) distribution(exponential)
est store m1
streg `varlist' declared_cat_1 declared_cat_2 russia uk if include_data==1, clu(subj_id) distribution(exponential)
est store m2
streg `varlist' `varlist3' russia uk if include_data==1, clu(subj_id) distribution(exponential)
est store m3
local note="Exponential distribution survival time model. Standard errors are clustered by subject. Baseline category for subject decision in Model 2 is honest behavior in this period. Baseline category for subject decision in Model 3 is honest behavior in this and previous period."
esttab m1 m2 m3 using "`fname'", label mtitle("Model 1" "Model 2" "Model 3") wide compress nonum note(`note') se star(* 0.10 ** 0.05 *** 0.01) replace


local fname="`path'table_reactiontime_w.tex"
estimates clear
streg `varlist' russia uk if include_data==1, clu(subj_id) distribution(weibull)
est store m1
streg `varlist' declared_cat_1 declared_cat_2 russia uk if include_data==1, clu(subj_id) distribution(weibull)
est store m2
streg `varlist' `varlist3' russia uk if include_data==1, clu(subj_id) distribution(weibull)
est store m3
local note="Weibull distribution survival time model. Standard errors are clustered by subject. Baseline category for subject decision in Model 2 is honest behavior in this period. Baseline category for subject decision in Model 3 is honest behavior in this and previous period."
esttab m1 m2 m3 using "`fname'", label mtitle("Model 1" "Model 2" "Model 3") wide compress nonum note(`note') se star(* 0.10 ** 0.05 *** 0.01) replace



test declared_cat_p1_3= declared_cat_p1_2
test declared_cat_p1_3= declared_cat_p1_1
test declared_cat_1_2= declared_cat_1_3



***********************************************************************************************
*********** FIGURE: PREVALENCE OF LYING DEPENDING ON SUBJECT PERFORMANCE *************
***********************************************************************************************

use "`path'mastern_final2018_new_new.dta", clear
//drop declared_near
gen declared_near=declared>0&declared_frac<=.2
collapse (mean) c0=declared_0 cf=declared_f cn=declared_near (sd) c0_sd=declared_0 cf_sd=declared_f cn_sd=declared_near (count) n=cheat if include_data==1, by(country_code hightype)
rename hightype type
replace type=type+1
expand 3
sort country_code type
gen t=_n
gen cc=0
forval i=1/3 {
forval ii=1/3 {
forval iii=1/2 {
local obsn=(`i'-1)*6+(`iii'-1)*3+`ii'
replace cc=`ii' in `obsn'
if `ii'==2 {
replace c0=cf in `obsn'
replace c0_sd=cf_sd in `obsn'
}
else if `ii'==3 {
replace c0=cn in `obsn'
replace c0_sd=cn_sd in `obsn'
}
}
}
}
generate hi = c0 + invttail(n-1,0.025)*(c0_sd / sqrt(n))
generate lo = c0 - invttail(n-1,0.025)*(c0_sd / sqrt(n))
replace t=t+1 in 7/18
replace t=t+1 in 13/18

twoway (bar c0 t if type==1&cc==1, color(red) barw(.8)) (bar c0 t if type==1&cc==2, color(red) barw(.8) fi(60)) (bar c0 t if type==1&cc==3, color(red) barw(.8) fi(30)) (bar c0 t if type==2&cc==1, color(blue) barw(.8)) (bar c0 t if type==2&cc==2, color(blue) barw(.8) fi(60)) (bar c0 t if type==2&cc==3, color(blue) barw(.8) fi(30)) (rcap hi lo t, color(black)), xlabel(3 "Chile" 10 "Russia" 17.5 "UK", noticks) note("The graph shows the total share of declarations in each country that is maximal lying, partial lying," "or near-maximal lying, defined as declarations between 1 EDU and 20\% of income.") ylabel(0(.2)1) legend(row(3) order(1 "Low performance, maximal" 4 "High performance, maximal" 2 "Low performance, limited" 5 "High performance, limited" 3 "Low performance, near-maximal" 6 "High performance, near-maximal" ) size(small)) xsize(6) xtitle("")
graph export "`path'cheat_hilo.eps", as(eps) preview(off) replace


*******************************************************************************
*************** TABLE: NEAR-MAXIMAL CHEATING ****************************** 
*******************************************************************************

use "`path'mastern_final2018_new_new.dta", clear
global fname="`path'table_nearmax.tex"
global byvar="hightype"
global llist="Low High p"
do "`path'tables_3.do"

use "`path'mastern_final2018_new_new.dta", clear
global fname="`path'table_nearmax_male.tex"
global byvar="male"
global llist="Female Male p"
do "`path'tables_3.do"

use "`path'mastern_final2018_new_new.dta", clear
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
//drop ttt
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
tsset subj_id period2
gen declared_same=declared==l.declared
tab declared_same if period2!=.&ind_typenew2==2&include_data==1&declared_cat==2
gen ncorrect_same=ncorrectret==l.ncorrectret
tab declared_same if period2!=.&ind_typenew2==2&include_data==1&declared_cat==2&ncorrect_same==1

******************************************************************************
********* FIGURE: DISTRIBUTION OF RET PERFORMANCE ****************************
******************************************************************************

local i=1
graph twoway (kdensity ncorrect_subjav if period2==1&country_code==1&include_data==1, color(dkgreen) range(0 30) bwidth(`i')) (kdensity ncorrect_subjav if period2==1&country_code==2&include_data==1, color(red) range(0 30) bwidth(`i'))  (kdensity ncorrect_subjav if period2==1&country_code==3&include_data==1, color(blue) range(0 30) bwidth(`i')), legend(row(1) order(1 "Chile" 2 "Russia" 3 "UK")) xtitle("") ytitle("") note("Distribution of the number of correct answers. Epanechnikov density, bwidth=1")
graph export "`path'ret_density.eps", as(eps) preview(off) replace


******************************************************************
******* TABLE: RET PERFORMANCE ***********************************
******************************************************************

use "`path'mastern_final2018_new_new.dta", clear
local fname="`path'table_ret.tex"

local note = "OLS regressions. Dependent variable is average performance over 10 rounds. DG frac is the fraction of the 1000 ECU donated in the dictator game. Norms is the social norms index (see Table \ref{tab:norms}). SafeChoices if the number (0-10) of safe choices on the lottery task. Income is the number of the individual's income bracket, rescaled between 0 and 1 (for Chile and the UK), and the individual's perceived income decile, rescaled between 0 and 1 (for Russia)."

local varlist1 = "male age_subject offerdg_0 offerdg tax_20 tax_30"
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


******************************************************************
******* TABLE: RET PERFORMANCE, PERIOD ***************************
******************************************************************

use "`path'mastern_final2018_new_new.dta", clear
local fname="`path'table_ret_per.tex"

local note = "OLS regressions. Dependent variable is parformance in a round. Standard errors are clustered by subject. DG frac is the fraction of the 1000 ECU donated in the dictator game. Norms is the social norms index (see Table \ref{tab:norms}). SafeChoices if the number (0-10) of safe choices on the lottery task. Income is the number of the individual's income bracket, rescaled between 0 and 1 (for Chile and the UK), and the individual's perceived income decile, rescaled between 0 and 1 (for Russia)."

local varlist1 = "male age_subject period2 offerdg_0 offerdg tax_20 tax_30"
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
***************** TABLE: RET PERFORMANCE, RUSSIA ******************
***********************************************************************


use "`path'mastern_final2018_new_new.dta", clear
local fname="`path'table_ret_russia.tex"
local note = "OLS regressions. Dependent variable is average performance over 10 rounds in the first model, and performance in a round for the second model. Robust standard errors for first model, standard errors clustered by subject for the second model. DG frac is the fraction of the 1000 ECU donated in the dictator game. Norms is the social norms index (see Table \ref{tab:norms}). SafeChoices if the number (0-10) of safe choices on the lottery task. Trusting behavior is the trusting behavior index (see Table \ref{tab:trust}). Income is the number of the individual's income bracket, rescaled between 0 and 1 (for Chile and the UK), and the individual's perceived income decile, rescaled between 0 and 1 (for Russia)."


local varlist1 = "male age_subject offerdg_0 offerdg tax_20 tax_30 shock status status_H non_fixed mpcr norms trusting safechoices ideology income2"
local varlist2 = "male age_subject period2 offerdg_0 offerdg tax_20 tax_30 shock l_shock status status_H non_fixed mpcr l_others norms trusting safechoices ideology income2"

estimates clear
reg ncorrect_subjav `varlist1' if period2==1&include_data==1&country_code==2, robust
est store m1
reg ncorrectret `varlist2' if period2>1.&include_data==1&country_code==2, cluster(subj_id)
est store m2 
esttab m1 m2 using "`fname'", label mtitle("Average" "Per round") wide compress nonum replace order(`varlist2') note("`note'") r2 se star(* 0.10 ** 0.05 *** 0.01)


