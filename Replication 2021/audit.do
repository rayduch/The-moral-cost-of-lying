
**Regression for 10% audit rate


*********************************************************************************
*********************************************************************************
*********************************************************************************

****************************************************************************

global preh="&\multicolumn{6}{c|}{Mlogit, average marginal effects }&\multicolumn{2}{c|}{OLS}&\multicolumn{2}{c}{OLS}\\"
global ind_typenewcond="declared_cat==2"
global auditcond=0

global scals_1=`" "t2030 D20=D30" "rusuk Russia=UK" "'
global scals_2=`" "t2030 D20=D30" "'
global scals_3=`" "t2030 D20=D30" "'
global scals_4=`" "t2030 D20=D30" "'

global tcond_1="1.tax_20=1.tax_30 1.russia=1.uk"
global tcond_2="1.tax_20=1.tax_30" 
global tcond_3="1.tax_20=1.tax_30" 
global tcond_4="1.tax_20=1.tax_30"

global eadd_1="t2030=r(p) rusuk=r(p)"
global eadd_2="t2030=r(p)"
global eadd_3="t2030=r(p)"
global eadd_4="t2030=r(p)"

global titles="All\space{}countries Chile Russia UK"
global aplist="append append append append"



global conds "1==1 country_code==1 country_code==2 country_code==3"
global include_cond=" include_data_all_audit==1&auditrate==10"
global var1="ncorrect_rank_audit ncorrect_dev2_audit i.male age period4 offerdg_frac i.tax_20 i.tax_30"
global var2="i.shock i.shock_H i.status i.status_H i.non_fixed"
global var3="0.tax_20 0.tax_30 0.shock 0.shock_H 0.status 0.status_H 0.non_fixed 0.male"
global varlist_1 = "$var1 " + "i.russia i.uk " + "$var2"
global varlist_2 = "$var1 " + "$var2"
global varlist_3 = "$var1 " + "$var2"
global varlist_4 = "$var1 " + "$var2"
global varlist1_1 = "$var3 " + "0.russia 0.uk" 
global varlist1_2 = "$var3"
global varlist1_3 = "$var3" 
global varlist1_4 = "$var3"
global note = "The first three columns report average marginal effects for multinomial logistic regression (dependent variable is whether the subject declared 0\%, 100\%, or something in between, in a given round). Standard errors are clustered by subject. Fourth and fifth columns report OLS regressions, the dependent variables are the fraction and the amount of income not declared in a given round; only partial lying decisions are considered. Standard errors are clustered by subject. RET rank is the national rank, between 0 and 1, of subject's national performance at the real effort task. RET Deviation is the difference between actual number of correct additions and one predicted from subject and period FE. DG frac is the fraction of the 1000 ECU donated in the dictator game."



global i1=1
global i2=1

do "`path'tables_2_2020.do"
