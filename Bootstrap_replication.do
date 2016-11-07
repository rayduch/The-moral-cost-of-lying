
******************************
**** Bootstrap calculations
*******************************

cd "C:\Users\André Laroze\Dropbox\CESS-Santiago\Denise\Cheating\Why-we-Cheat-2016-master\Replication\Laroze rep package"

use "MasterfileOxfordChile_20160506.dta", clear


log using "bootstrap.log", replace

****************
**** Data prep
***************

/* Analyse Combined Data */


g T10 = 0
g T20 = 0
g T30 = 0
g T40 = 0
g T50 = 0
g EndT = 0
replace T10 = 1 if taxrate == 10
replace T20 = 1 if taxrate == 20
replace T30 = 1 if taxrate == 30
replace T40 = 1 if taxrate == 40
replace T50 = 1 if taxrate == 50
replace EndT = 1 if session == 13 | session == 14


g AR0 = 0
g AR30 = 0
g AR70 = 0
g AR100 = 0 
replace AR0 = 1 if auditrate == 0
replace AR30 = 1 if auditrate == 30
replace AR70 = 1 if auditrate == 70
replace AR100 = 1 if auditrate == 100


g Block = .
replace Block = 1 if auditrate < 50
replace Block = 2 if auditrate > 50
replace Block = 2 if session >= 23 & auditrate == 30

g periodo = .
replace periodo = period if Block == 1
replace periodo = period+10 if Block == 2

g percevaded = .
replace percevaded = (profitret - declared)/profitret

g difsalaries = 0
replace difsalaries = 1 if session == 10 | session == 11  | session == 12  | session == 15  | session == 16  | session == 17 | session == 18

replace type = . if type == 0
g highsalary = .
replace highsalary = 1 if type == 1 & difsalaries == 1
replace highsalary = 0 if type == 2 & difsalaries == 1

g deadweightloss = 0
replace deadweightloss = 70 if session == 23 | session == 24

g diffMPCR = 0
replace diffMPCR = 1 if session >= 19 & session <= 22



g shock = 0
replace shock = 1 if session == 25  | session == 26  | session == 27 | session==28 | session==29 | session==30

g receiveshock = .
replace receiveshock = 1 if type == 1 & shock == 1
replace receiveshock = 0 if type == 2 & shock == 1


/* Create Integrity Measure */

egen integrity = rowmean(publictransport-buyingstolen)

/* create cost to comply */


gen cost_comply=(taxrate/100)*profitret


/* Create average cheating by group */



sort grupo periodo
by grupo periodo: egen avgcheatperiodgroup = mean(percevad)



/* Calculate High Performance Types */

gen high_perform=0
replace high_perform=1 if ncorrectret>11

 sort session sujeto
 

 egen avg_RET=mean(ncorrectret), by(session sujeto)
 gen perform_high=0
 replace perform_high=1 if avg_RET>11
 
 
 saveold "MasterfileOxfordChile_20160506_b.dta", version(12) replace
  
**************************
**** Bootstrap estimation
**************************



/* BASELINE: Baseline Treatment Means */

preserve

keep if session<4

keep if auditrate==0

*mean percevaded, over(perform_high)


ttest percevaded, by(perform_high)
return list
matrix mu = (r(mu_1), r(mu_2))
matrix list mu

bs difference = (r(mu_1)-r(mu_2)), reps(1000) seed(01010) cluster(sujeto): ttest percevaded, by(perform_high)

/*                                 (Replications based on 72 clusters in sujeto)
------------------------------------------------------------------------------
             |   Observed   Bootstrap                         Normal-based
             |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
  difference |  -.2389356   .0932844    -2.56   0.010    -.4217697   -.0561015
------------------------------------------------------------------------------
*/

restore



/* STATUS: Status Treatment Means */

preserve

keep if session==15 | session==16 | session==17 | session==18 
keep if auditrate==0
tab highsalary, missing
mean percevaded, over(highsalary perform_high)

ttest percevaded, by(perform_high)
return list
matrix mu = (r(mu_1), r(mu_2))
matrix list mu

bs difference = (r(mu_1)-r(mu_2)), reps(1000) seed(01010) cluster(sujeto): ttest percevaded, by(perform_high)
 
bs difference = (r(mu_1)-r(mu_2)), reps(1000) seed(01010) cluster(sujeto): ttest percevaded if highsalary==0, by(perform_high)

/*                                 (Replications based on 36 clusters in sujeto)

------------------------------------------------------------------------------
             |   Observed   Bootstrap                         Normal-based
             |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
  difference |  -.3046128   .1458378    -2.09   0.037    -.5904496    -.018776
------------------------------------------------------------------------------
*/
 
 
 
 
bs difference = (r(mu_1)-r(mu_2)), reps(1000) seed(01010) cluster(sujeto): ttest percevaded if highsalary==1, by(perform_high)

/*                                 (Replications based on 36 clusters in sujeto)
------------------------------------------------------------------------------
             |   Observed   Bootstrap                         Normal-based
             |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
  difference |  -.2124557   .1350222    -1.57   0.116    -.4770943    .0521829
------------------------------------------------------------------------------
*/

restore


**** Shock

* Bootstrapped Shock Treatment Oxford 

preserve

keep if session>24 & session <28

keep if auditrate==0

tab shock
tab receiveshock

ttest percevaded, by(perform_high)
return list
matrix mu = (r(mu_1), r(mu_2))
matrix list mu

bs difference = (r(mu_1)-r(mu_2)), reps(1000) seed(01010) cluster(sujeto): ttest percevaded, by(perform_high)
 
bs difference = (r(mu_1)-r(mu_2)), reps(1000) seed(01010) cluster(sujeto): ttest percevaded if receiveshock==0, by(perform_high)

/*                                 (Replications based on 56 clusters in sujeto)
------------------------------------------------------------------------------
             |   Observed   Bootstrap                         Normal-based
             |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
  difference |  -.1776674   .1298859    -1.37   0.171     -.432239    .0769042
------------------------------------------------------------------------------
*/


 
bs difference = (r(mu_1)-r(mu_2)), reps(1000) seed(01010) cluster(sujeto): ttest percevaded if receiveshock==1, by(perform_high)

/*                                 (Replications based on 56 clusters in sujeto)
------------------------------------------------------------------------------
             |   Observed   Bootstrap                         Normal-based
             |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
  difference |  -.1100031    .101378    -1.09   0.278    -.3087003    .0886941
------------------------------------------------------------------------------
*/
 
 
restore 

**** Redistribution
preserve


keep if  session>18 & session<23

keep if auditrate==0

tab session

mean percevaded, over(perform_high)


ttest percevaded, by(perform_high)
return list
matrix mu = (r(mu_1), r(mu_2))
matrix list mu

bs difference = (r(mu_1)-r(mu_2)), reps(1000) seed(01010) cluster(sujeto): ttest percevaded, by(perform_high)

/*                                 (Replications based on 84 clusters in sujeto)
------------------------------------------------------------------------------
             |   Observed   Bootstrap                         Normal-based
             |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
  difference |  -.0884571    .074846    -1.18   0.237    -.2351525    .0582383
------------------------------------------------------------------------------
*/

restore

**** Non-Fixed
/* NON-FIXED: Analyse Sessions 31-34 */
  
  preserve
  
  keep if session>30
  
  keep if auditrate==0
  
tab highsalary, missing
mean percevaded, over(perform_high)

ttest percevaded, by(perform_high)
return list
matrix mu = (r(mu_1), r(mu_2))
matrix list mu

bs difference = (r(mu_1)-r(mu_2)), reps(1000) seed(01010) cluster(sujeto): ttest percevaded, by(perform_high)



/*                                  (Replications based on 88 clusters in sujeto)
------------------------------------------------------------------------------
             |   Observed   Bootstrap                         Normal-based
             |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
  difference |  -.3629419   .0735158    -4.94   0.000    -.5070303   -.2188535
------------------------------------------------------------------------------
*/

restore


log close
