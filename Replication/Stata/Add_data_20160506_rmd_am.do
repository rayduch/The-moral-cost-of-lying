clear

/* Created Chile Data */



*cd "/Users/akitaka/Dropbox/ray_projects/small_stuff/merge/"
*cd "/Users/Administrator/Dropbox/Tax Compliance Experiments/Data/May 2016/"
cd "~/Dropbox/Tax Compliance Experiments/Data/May 2016"

import excel "~/Dropbox/Tax Compliance Experiments/Data/November 2015/Santiago-Data_rmd.xlsx", sheet("Masterfile Chile") firstrow case(lower) clear
saveold "~/Dropbox/Tax Compliance Experiments/Data/November 2015/MasterfileOxfordChile_2015_31_10.dta", replace


import excel "~/Dropbox/Tax Compliance Experiments/Data/November 2015/Santiago-Data_session30.xlsx", sheet("Sheet1") firstrow case(lower) clear
saveold "~/Dropbox/Tax Compliance Experiments/Data/November 2015/MasterfileOxfordChile_30.dta", replace

import excel "TE4_Oxford_May_2016c", sheet("MasterfileOxford2016") firstrow case(lower) clear
saveold "MasterfileOxford2016c.dta", replace


clear
use "~/Dropbox/Taxes_Ray/Data/TE_MasterFile_22-03-2015_576Ss.dta"

g session = .
replace session = 1 if sujeto <= 24
replace session = 2 if sujeto >= 25 & sujeto <= 48
replace session = 3 if sujeto >= 49 & sujeto <= 72
replace session = 4 if sujeto >= 73 & sujeto <= 96
replace session = 5 if sujeto >= 97 & sujeto <= 120
replace session = 6 if sujeto >= 121 & sujeto <= 140
replace session = 7 if sujeto >= 141 & sujeto <= 164
replace session = 8 if sujeto >= 165 & sujeto <= 184
replace session = 9 if sujeto >= 185 & sujeto <= 208
replace session = 10 if sujeto >= 209 & sujeto <= 232
replace session = 11 if sujeto >= 233 & sujeto <= 256
replace session = 12 if sujeto >= 257 & sujeto <= 276
replace session = 13 if sujeto >= 277 & sujeto <= 300
replace session = 14 if sujeto >= 301 & sujeto <= 320
replace session = 15 if sujeto >= 321 & sujeto <= 344
replace session = 16 if sujeto >= 345 & sujeto <= 356
replace session = 17 if sujeto >= 357 & sujeto <= 372
replace session = 18 if sujeto >= 373 & sujeto <= 392
replace session = 19 if sujeto >= 393 & sujeto <= 416
replace session = 20 if sujeto >= 417 & sujeto <= 436
replace session = 21 if sujeto >= 437 & sujeto <= 456
replace session = 22 if sujeto >= 457 & sujeto <= 476
replace session = 23 if sujeto >= 477 & sujeto <= 500
replace session = 24 if sujeto >= 501 & sujeto <= 520
replace session = 25 if sujeto >= 521 & sujeto <= 536
replace session = 26 if sujeto >= 537 & sujeto <= 556
replace session = 27 if sujeto >= 557 & sujeto <= 576

/* Add Chile Results */

append using "~/Dropbox/Tax Compliance Experiments/Data/November 2015/MasterfileOxfordChile_2015_31_10.dta"


replace session = 28 if sujeto >= 577 & sujeto <= 592
replace session = 29 if sujeto >= 593 & sujeto <= 612

replace grupo=145 if session==28 & group==1
replace grupo=146 if session==28 & group==2
replace grupo=147 if session==28 & group==3
replace grupo=148 if session==28 & group==4

*There is an error with group 1 -- it has 8 subjects

replace grupo=149 if session==29 & group==1
replace grupo=150 if session==29 & group==2
replace grupo=151 if session==29 & group==3
replace grupo=152 if session==29 & group==4

/* Add Chile Results - November*/

append using "~/Dropbox/Tax Compliance Experiments/Data/November 2015/MasterfileOxfordChile_30.dta"

replace session = 30 if sujeto >= 613 & sujeto <= 628

/*This is Tax Rate 30 */
replace grupo=153 if session==30 & group==1
replace grupo=154 if session==30 & group==2
replace grupo=155 if session==30 & group==3
replace grupo=156 if session==30 & group==4

replace chile=1 if session==30

*import excel "", sheet("Masterfile Chile") firstrow case(lower) clear
*saveold "~/Dropbox/Tax Compliance Experiments/Data/November 2015/MasterfileOxfordChile_2015_31_10.dta", replace


append using "MasterfileOxford2016c.dta"

replace session = 31 if sujeto >= 629 & sujeto <= 644
replace session = 32 if sujeto >= 645 & sujeto <= 660
replace session = 33 if sujeto >=661 & sujeto <=676
replace session = 34 if sujeto >=677 & sujeto <=688




saveold "MasterfileOxfordChile_20160506.dta", replace


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



/* Calculate High Performance Types */

gen high_perform=0
replace high_perform=1 if ncorrectret>11

 sort session sujeto
 

 egen avg_RET=mean(ncorrectret), by(session sujeto)
 gen perform_high=0
 replace perform_high=1 if avg_RET>11
 
 
 

mean percevaded if session>24 & auditrate==0, over(perform_high)


/* Oxford Shock Treatment Estimates */

preserve

keep if session>24 & session <28

keep if auditrate==0
twoway scatter percevaded profitret || lfit  percevaded profitret if receiveshock==1 ||  lfit  percevaded profitret if receiveshock==0 

gen T20_int=T20*ncorrectret
gen T30_int=T30*ncorrectret
gen receive_int=receiveshock*ncorrectret
gen T20_int2=T20*receiveshock
gen T30_int2=T30*receiveshock

regr percevaded ncorrectret T20 T30 T20_int T30_int ideology gender age if receiveshock==1 

regr percevaded ncorrectret T20 T30 T20_int T30_int ideology gender age if receiveshock==0

regr percevaded ncorrectret T20 T30 T20_int T30_int ideology gender age receiveshock receive_int
 
regr percevaded ncorrectret T20 T30 T20_int2 T30_int2 ideology gender age receiveshock receive_int

regr percevaded ncorrectret T20 T30 T20_int2 T30_int2 T20_int T30_int ideology gender age receiveshock receive_int

restore


/* Chile Shock Analysis  */

preserve


keep if session>27 & session <31


keep if auditrate==0
twoway scatter percevaded profitret || lfit  percevaded profitret if receiveshock==1 ||  lfit  percevaded profitret if receiveshock==0 

gen T20_int=T20*ncorrectret
gen T30_int=T30*ncorrectret
gen receive_int=receiveshock*ncorrectret
gen T20_int2=T20*receiveshock
gen T30_int2=T30*receiveshock

regr percevaded ncorrectret T20 T20_int ideology gender age if receiveshock==1 

regr percevaded ncorrectret T20 T20_int ideology gender age if receiveshock==0

regr percevaded ncorrectret T20 T20_int  if receiveshock==1 

regr percevaded ncorrectret T20 T20_int  if receiveshock==0

regr percevaded ncorrectret T20 T20_int receiveshock receive_int
probit cheat ncorrectret T20 T20_int receiveshock receive_int


regr percevaded ncorrectret T20 T30 T20_int T30_int ideology gender age if receiveshock==1 

regr percevaded ncorrectret T20 T30 T20_int T30_int ideology gender age if receiveshock==0

regr percevaded ncorrectret T20 T30 T20_int T30_int ideology gender age receiveshock receive_int
 
regr percevaded ncorrectret T20 T30 T20_int2 T30_int2 ideology gender age receiveshock receive_int

regr percevaded ncorrectret T20 T30 T20_int2 T30_int2 T20_int T30_int ideology gender age receiveshock receive_int



restore 





  **** Graphs *****

  preserve
  
keep if session<4 | session==15 | session==16 | session==17 | session==18 | session==25 | session==26 |  session==27
gen treat_type=.
replace treat_type=0 if session<4
replace treat_type=1 if highsalary==0
replace treat_type=2 if highsalary==1
replace treat_type=3 if session>24

label define treat_label  0	"Baseline" 1 "Low Status" 2 "High Status" 3 "Shock"
label values treat_type treat_label 


hist ncorrectret, by(treat_type, title("Real Effort Task: Correct Additions") legend(off)) percent width(1) scheme(s1mono) ///
  xtitle("Number of Correct Additions") legend(off)

mean ncorrectret, over(treat_type)

restore




/* Boot Strap Difference in Means ttest Clustered */


ttest percevaded  if session>24 & auditrate==0, by(perform_high)
return list
matrix mu = (r(mu_1), r(mu_2))
matrix list mu

*bs difference = (r(mu_1)-r(mu_2)), reps(1000) seed(01010) cluster(subjectID): ttest outcomeVariable, by(treatmentVariable)
 
 
bs difference = (r(mu_1)-r(mu_2)), reps(1000) seed(01010) cluster(sujeto): ttest percevaded, by(perform_high)
 
 
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
 
 bs difference = (r(mu_1)-r(mu_2)), reps(1000) seed(01010) cluster(sujeto): ttest percevaded if receiveshock==1, by(perform_high)
 
restore 


 
  /* BASELINE NON-FIXED: Analyse Sessions 31-34 */
  
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


probit cheat ncorrectret cost_comply, vce(cluster sujeto)


probit cheat ncorrectret cost_comply


regr percevaded ncorrectret cost_comply, vce(cluster sujeto)

restore


saveold "MasterfileOxfordChile_20160506_b.dta", replace




/* Self Assessed Performance */

tab init_pred perform_high if session>31, column

  /* Analysis of Real Die */
  
 preserve
 
keep if session>30
sort perform_high
twoway hist realdie, by(perform_high) percent
*twoway hist digitaldie, by(perform_high) percent

by perform_high: tab realdie

corr ncorrectret digitaldie

 restore
 
  **** Dictator Analysis *****

preserve
  
keep if session<4 | session==15 | session==16 | session==17 | session==18 | session==25 | session==26 |  session==27 | session>30

sort perform_high
twoway hist offerdg, by(perform_high) percent

mean offerdg, over(perform_high)

corr ncorrectret offerdg

*tab sujeto

restore

 
 /* Analyse the Outlying RET Outcomes for Oxford Data */
 preserve
 
 keep if session<28 & auditrate==0
 

 
 gen low_high=0
 replace low_high=1 if perform_high==1
 replace low_high=2 if perform_high==1 & ncorrectret<10
 replace low_high=3 if perform_high==0 
 replace low_high=4 if perform_high==0 & ncorrectret>12
 
 egen avg_percevaded=mean(percevaded), by(session sujeto)
 
 gen diff_low=0
 replace diff_low=percevaded-avg_percevaded if low_high==2
 
 gen diff_high=0
 replace diff_high=percevaded-avg_percevaded if low_high==4
 
  
 sort session sujeto
* list session sujeto period ncorrectret high_perform avg_RET perform_high low_high diff_high diff_low
 
 twoway hist diff_high if low_high==4, xlabel(-.4(.1).7) bin(20) gap(5) scheme(s1mono) xtitle("Histogram of Average Cheating Minus Cheating When Performance is High") percent
 twoway hist diff_low if low_high==2, xlabel(-.8(.1).7) bin(20) gap(5) scheme(s1mono) xtitle("Histogram of Average Cheating Minus Cheating When Performance is Low") percent

 mean diff_high if low_high==4
 mean diff_low if low_high==2
 
 
 /* Analyse Integrity by Performance Type */
 
 mean integrity, over(perform_high)
 mean taxes, over(perform_high)
 mean lying, over(perform_high)
 
 
 
 
 
 
 hist safechoices, percent scheme(s1mono) 
 
 
 
 restore
 
 
/* Treatment Effect Figure  */


/* SHOCK: Shock Treatment Means */

preserve

keep if shock==1 & auditrate==0
 
sort receiveshock

mean percevaded, over(receiveshock perform_high)


gen receive_inter=ncorrectret*receiveshock


ttest percevaded, by(perform_high)
return list
matrix mu = (r(mu_1), r(mu_2))
matrix list mu

bs difference = (r(mu_1)-r(mu_2)), reps(1000) seed(01010) cluster(sujeto): ttest percevaded, by(perform_high)
 
 bs difference = (r(mu_1)-r(mu_2)), reps(1000) seed(01010) cluster(sujeto): ttest percevaded if receiveshock==0, by(perform_high)
 
 bs difference = (r(mu_1)-r(mu_2)), reps(1000) seed(01010) cluster(sujeto): ttest percevaded if receiveshock==1, by(perform_high)
 
  probit cheat ncorrectret cost_comply, vce(cluster sujeto)
 
 probit cheat ncorrectret cost_comply receiveshock receive_inter, vce(cluster sujeto)

regr percevaded ncorrectret cost_comply, vce(cluster sujeto)

regr percevaded ncorrectret cost_comply  receiveshock receive_inter, vce(cluster sujeto)

 
restore



/* STATUS: Status Treatment Means */

preserve

keep if session==15 | session==16 | session==17 | session==18 
keep if auditrate==0
tab highsalary, missing
mean percevaded, over(highsalary perform_high)

gen highsalary_inter=ncorrectret*highsalary

ttest percevaded, by(perform_high)
return list
matrix mu = (r(mu_1), r(mu_2))
matrix list mu

bs difference = (r(mu_1)-r(mu_2)), reps(1000) seed(01010) cluster(sujeto): ttest percevaded, by(perform_high)
 
 bs difference = (r(mu_1)-r(mu_2)), reps(1000) seed(01010) cluster(sujeto): ttest percevaded if highsalary==0, by(perform_high)
 
 bs difference = (r(mu_1)-r(mu_2)), reps(1000) seed(01010) cluster(sujeto): ttest percevaded if highsalary==1, by(perform_high)
 
 probit cheat ncorrectret cost_comply, vce(cluster sujeto)
 
 probit cheat ncorrectret cost_comply highsalary highsalary_inter, vce(cluster sujeto)

regr percevaded ncorrectret cost_comply, vce(cluster sujeto)

regr percevaded ncorrectret cost_comply  highsalary highsalary_inter, vce(cluster sujeto)
restore



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

probit cheat ncorrectret cost_comply, vce(cluster sujeto)



*probit cheat perform_high cost_comply, vce(cluster sujeto)



*probit cheat perform_high#cost_comply, vce(cluster sujeto)

*probit cheat ncorrectret, vce(cluster sujeto)

regr percevaded ncorrectret cost_comply, vce(cluster sujeto)

*regr percevaded perform_high cost_comply, vce(cluster sujeto)

*corr cost_comply ncorrectret

restore




/* REDISTRIBUTE: Redistribution Treatment Means */

preserve


keep if diffMPCR==1

keep if auditrate==0

tab session

mean percevaded, over(perform_high)


ttest percevaded, by(perform_high)
return list
matrix mu = (r(mu_1), r(mu_2))
matrix list mu

bs difference = (r(mu_1)-r(mu_2)), reps(1000) seed(01010) cluster(sujeto): ttest percevaded, by(perform_high)


probit cheat ncorrectret cost_comply, vce(cluster sujeto)

*probit cheat perform_high cost_comply, vce(cluster sujeto)


regr percevaded ncorrectret cost_comply, vce(cluster sujeto)

restore



/*  Risk Analysis  */

gen risk_averse_level=safechoices
recode risk_averse_level 0/3=1 4/6=2 7/10=3

mean percevaded, over(perform_high risk_averse_level)

corr ncorrectret safechoices

regr percevaded ncorrectret safechoices T20 T30



/* Risk Analysis Baseline */

preserve

keep if session<4

keep if auditrate==0

corr ncorrectret safechoices

mean percevaded, over(perform_high risk_averse_level)

regr percevaded ncorrectret safechoices T20 T30

restore





/* Risk Analysis Status Treatment Means */

preserve

keep if session==15 | session==16 | session==17 | session==18 
keep if auditrate==0
tab highsalary, missing
mean percevaded, over(highsalary perform_high risk_averse_level)
mean percevaded, over(perform_high risk_averse_level)

corr ncorrectret safechoices

regr percevaded ncorrectret safechoices T20 T30

restore




/*  Risk Analysis Shock Treatment Means */

preserve

keep if shock==1 & auditrate==0
 
sort receiveshock

mean percevaded, over(receiveshock perform_high risk_averse_level)
mean percevaded, over(perform_high risk_averse_level)
corr ncorrectret safechoices

regr percevaded ncorrectret safechoices T20 T30

restore



/* Redistribution Treatment Means */

preserve


keep if diffMPCR==1

keep if auditrate==0

tab session

mean percevaded, over(perform_high risk_averse_level)

corr ncorrectret safechoices

regr percevaded ncorrectret safechoices T20 T30

ttest percevaded, by(perform_high)
return list
matrix mu = (r(mu_1), r(mu_2))
matrix list mu

bs difference = (r(mu_1)-r(mu_2)), reps(1000) seed(01010) cluster(sujeto): ttest percevaded, by(perform_high)
 

restore


* Shock Treatment *




 
 *  Shock Treatment *
 
preserve

keep if session>24 & session <28

keep if auditrate==0
keep if shock==1



gen receive_int=receiveshock*ncorrectret

ttest percevaded, by(perform_high)
return list
matrix mu = (r(mu_1), r(mu_2))
matrix list mu

bs difference = (r(mu_1)-r(mu_2)), reps(1000) seed(01010) cluster(sujeto): ttest percevaded, by(perform_high)


probit cheat ncorrectret cost_comply, vce(cluster sujeto)

probit cheat ncorrectret cost_comply receiveshock, vce(cluster sujeto)

probit cheat ncorrectret cost_comply receiveshock receive_int, vce(cluster sujeto)

probit cheat ncorrectret cost_comply receiveshock receive_int safechoices, vce(cluster sujeto)



restore




/* Redistribution Treatment Regression with Safechoices */

 
*gen cost_comply=(taxrate/100)*profitret


preserve

keep if diffMPCR==1

keep if auditrate==0

tab session

corr ncorrectret safechoices

probit cheat ncorrectret cost_comply 


probit cheat ncorrectret cost_comply safechoices


restore






