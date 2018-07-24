
************************************************************************
**** Bootstrap calculations
**** Who cheats
**** File prepared by Denise Laroze on the basis of Ray Duch's  code
************************************************************************

cd "C:\Users\André Laroze\Dropbox\CESS-Santiago\Archive\Tax Compliance Experiments\Rep Material\Laroze rep package\"

log using "bootstrap.log", replace


 
**************************
**** Bootstrap estimation
**************************

use "masterfile_Oct2017.dta", clear

/* All Treatments*/

preserve

keep if  session <4 | session==15 | session==16 | session==17 | session==18 | session>24 & session <28 | session>18 & session<23| session>30

keep if auditrate==0


ttest percevaded, by(perform_high)
/*

Two-sample t test with equal variances
------------------------------------------------------------------------------
   Group |     Obs        Mean    Std. Err.   Std. Dev.   [95% Conf. Interval]
---------+--------------------------------------------------------------------
       0 |   1,700    .6112218    .0108653    .4479873    .5899111    .6325326
       1 |   2,500    .8427561    .0067748     .338738    .8294714    .8560408
---------+--------------------------------------------------------------------
combined |   4,200    .7490399    .0062185    .4030034    .7368483    .7612314
---------+--------------------------------------------------------------------
    diff |           -.2315343     .012156               -.2553666    -.207702
------------------------------------------------------------------------------
    diff = mean(0) - mean(1)                                      t = -19.0469
Ho: diff = 0                                     degrees of freedom =     4198

    Ha: diff < 0                 Ha: diff != 0                 Ha: diff > 0
 Pr(T < t) = 0.0000         Pr(|T| > |t|) = 0.0000          Pr(T > t) = 1.0000


*/



bs difference = (r(mu_1)-r(mu_2)), reps(1000) seed(01010) cluster(sujeto): ttest percevaded, by(perform_high)


/*
                                (Replications based on 420 clusters in sujeto)
------------------------------------------------------------------------------
             |   Observed   Bootstrap                         Normal-based
             |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
  difference |  -.2315343   .0374784    -6.18   0.000    -.3049907   -.1580779
------------------------------------------------------------------------------
                       

*/

restore


/* BASELINE: Baseline Treatment Means */

preserve

keep if session<4

keep if auditrate==0

*mean percevaded, over(perform_high)


ttest percevaded, by(perform_high)
/*
Two-sample t test with equal variances
------------------------------------------------------------------------------
   Group |     Obs        Mean    Std. Err.   Std. Dev.   [95% Conf. Interval]
---------+--------------------------------------------------------------------
       0 |     260    .5946123    .0288993    .4659876    .5377048    .6515198
       1 |     460    .8335479    .0158219    .3393427    .8024555    .8646403
---------+--------------------------------------------------------------------
combined |     720    .7472656    .0151347    .4061063    .7175521     .776979
---------+--------------------------------------------------------------------
    diff |           -.2389356    .0302442               -.2983133   -.1795579
------------------------------------------------------------------------------
    diff = mean(0) - mean(1)                                      t =  -7.9002
Ho: diff = 0                                     degrees of freedom =      718

    Ha: diff < 0                 Ha: diff != 0                 Ha: diff > 0
 Pr(T < t) = 0.0000         Pr(|T| > |t|) = 0.0000          Pr(T > t) = 1.0000

*/

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
/*
Two-sample t test with equal variances
------------------------------------------------------------------------------
   Group |     Obs        Mean    Std. Err.   Std. Dev.   [95% Conf. Interval]
---------+--------------------------------------------------------------------
       0 |     220    .6382618    .0308648    .4577994    .5774317    .6990919
       1 |     500    .8981496    .0121076    .2707342    .8743615    .9219378
---------+--------------------------------------------------------------------
combined |     720    .8187395    .0133883    .3592454    .7924547    .8450243
---------+--------------------------------------------------------------------
    diff |           -.2598878    .0274198               -.3137204   -.2060552
------------------------------------------------------------------------------
    diff = mean(0) - mean(1)                                      t =  -9.4781
Ho: diff = 0                                     degrees of freedom =      718

    Ha: diff < 0                 Ha: diff != 0                 Ha: diff > 0
 Pr(T < t) = 0.0000         Pr(|T| > |t|) = 0.0000          Pr(T > t) = 1.0000

*/

return list
matrix mu = (r(mu_1), r(mu_2))
matrix list mu

bs difference = (r(mu_1)-r(mu_2)), reps(1000) seed(01010) cluster(sujeto): ttest percevaded, by(perform_high)
 
 
 
ttest percevaded if highsalary==0, by(perform_high) 
/*
Two-sample t test with equal variances
------------------------------------------------------------------------------
   Group |     Obs        Mean    Std. Err.   Std. Dev.   [95% Conf. Interval]
---------+--------------------------------------------------------------------
       0 |     100    .6341818    .0463655    .4636548    .5421827     .726181
       1 |     260    .9387946     .012355    .1992183    .9144656    .9631236
---------+--------------------------------------------------------------------
combined |     360      .85418    .0172065    .3264701    .8203418    .8880181
---------+--------------------------------------------------------------------
    diff |           -.3046128    .0349385               -.3733232   -.2359024
------------------------------------------------------------------------------
    diff = mean(0) - mean(1)                                      t =  -8.7186
Ho: diff = 0                                     degrees of freedom =      358

    Ha: diff < 0                 Ha: diff != 0                 Ha: diff > 0
 Pr(T < t) = 0.0000         Pr(|T| > |t|) = 0.0000          Pr(T > t) = 1.0000

*/ 
 
 
bs difference = (r(mu_1)-r(mu_2)), reps(1000) seed(01010) cluster(sujeto): ttest percevaded if highsalary==0, by(perform_high)

/*                                 (Replications based on 36 clusters in sujeto)

------------------------------------------------------------------------------
             |   Observed   Bootstrap                         Normal-based
             |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
  difference |  -.3046128   .1458378    -2.09   0.037    -.5904496    -.018776
------------------------------------------------------------------------------
*/
 
 
 
ttest percevaded if highsalary==1, by(perform_high) 
/*
Two-sample t test with equal variances
------------------------------------------------------------------------------
   Group |     Obs        Mean    Std. Err.   Std. Dev.   [95% Conf. Interval]
---------+--------------------------------------------------------------------
       0 |     120    .6416618    .0415158    .4547826    .5594564    .7238672
       1 |     240    .8541176    .0210437    .3260078    .8126627    .8955724
---------+--------------------------------------------------------------------
combined |     360     .783299    .0203697     .386487    .7432402    .8233578
---------+--------------------------------------------------------------------
    diff |           -.2124557    .0417886               -.2946377   -.1302738
------------------------------------------------------------------------------
    diff = mean(0) - mean(1)                                      t =  -5.0841
Ho: diff = 0                                     degrees of freedom =      358

    Ha: diff < 0                 Ha: diff != 0                 Ha: diff > 0
 Pr(T < t) = 0.0000         Pr(|T| > |t|) = 0.0000          Pr(T > t) = 1.0000

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
/*
Two-sample t test with equal variances
------------------------------------------------------------------------------
   Group |     Obs        Mean    Std. Err.   Std. Dev.   [95% Conf. Interval]
---------+--------------------------------------------------------------------
       0 |     180    .7081529    .0324689    .4356158    .6440819    .7722239
       1 |     380    .8519882    .0169603    .3306178      .81864    .8853363
---------+--------------------------------------------------------------------
combined |     560    .8057554    .0157772    .3733569    .7747655    .8367453
---------+--------------------------------------------------------------------
    diff |           -.1438353    .0332598               -.2091651   -.0785055
------------------------------------------------------------------------------
    diff = mean(0) - mean(1)                                      t =  -4.3246
Ho: diff = 0                                     degrees of freedom =      558

    Ha: diff < 0                 Ha: diff != 0                 Ha: diff > 0
 Pr(T < t) = 0.0000         Pr(|T| > |t|) = 0.0000          Pr(T > t) = 1.0000

*/



return list
matrix mu = (r(mu_1), r(mu_2))
matrix list mu

bs difference = (r(mu_1)-r(mu_2)), reps(1000) seed(01010) cluster(sujeto): ttest percevaded, by(perform_high)
 

 
 ttest percevaded if receiveshock==0, by(perform_high)
 /*
Two-sample t test with equal variances
------------------------------------------------------------------------------
   Group |     Obs        Mean    Std. Err.   Std. Dev.   [95% Conf. Interval]
---------+--------------------------------------------------------------------
       0 |      90    .6695881     .049006    .4649114    .5722143     .766962
       1 |     190    .8472556    .0241898    .3334332    .7995389    .8949722
---------+--------------------------------------------------------------------
combined |     280    .7901482    .0232358    .3888087    .7444085    .8358879
---------+--------------------------------------------------------------------
    diff |           -.1776674    .0486898               -.2735149     -.08182
------------------------------------------------------------------------------
    diff = mean(0) - mean(1)                                      t =  -3.6490
Ho: diff = 0                                     degrees of freedom =      278

    Ha: diff < 0                 Ha: diff != 0                 Ha: diff > 0
 Pr(T < t) = 0.0002         Pr(|T| > |t|) = 0.0003          Pr(T > t) = 0.9998

*/
 
 
 bs difference = (r(mu_1)-r(mu_2)), reps(1000) seed(01010) cluster(sujeto): ttest percevaded if receiveshock==0, by(perform_high)

/*                                 (Replications based on 56 clusters in sujeto)
------------------------------------------------------------------------------
             |   Observed   Bootstrap                         Normal-based
             |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
  difference |  -.1776674   .1298859    -1.37   0.171     -.432239    .0769042
------------------------------------------------------------------------------
*/


 
 
ttest percevaded if receiveshock==1, by(perform_high) 
/*


Two-sample t test with equal variances
------------------------------------------------------------------------------
   Group |     Obs        Mean    Std. Err.   Std. Dev.   [95% Conf. Interval]
---------+--------------------------------------------------------------------
       0 |      90    .7467176    .0424923    .4031178    .6622863     .831149
       1 |     190    .8567207    .0238385    .3285908    .8096971    .9037444
---------+--------------------------------------------------------------------
combined |     280    .8213626    .0213499    .3572523    .7793352      .86339
---------+--------------------------------------------------------------------
    diff |           -.1100031    .0453191               -.1992154   -.0207908
------------------------------------------------------------------------------
    diff = mean(0) - mean(1)                                      t =  -2.4273
Ho: diff = 0                                     degrees of freedom =      278

    Ha: diff < 0                 Ha: diff != 0                 Ha: diff > 0
 Pr(T < t) = 0.0079         Pr(|T| > |t|) = 0.0158          Pr(T > t) = 0.9921


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
/*
------------------------------------------------------------------------------
   Group |     Obs        Mean    Std. Err.   Std. Dev.   [95% Conf. Interval]
---------+--------------------------------------------------------------------
       0 |     350    .7367781    .0211353    .3954059    .6952095    .7783468
       1 |     490    .8252352      .01627    .3601509    .7932676    .8572029
---------+--------------------------------------------------------------------
combined |     840    .7883781    .0130265    .3775427    .7628098    .8139464
---------+--------------------------------------------------------------------
    diff |           -.0884571    .0262611               -.1400023   -.0369119
------------------------------------------------------------------------------
    diff = mean(0) - mean(1)                                      t =  -3.3684
Ho: diff = 0                                     degrees of freedom =      838

    Ha: diff < 0                 Ha: diff != 0                 Ha: diff > 0
 Pr(T < t) = 0.0004         Pr(|T| > |t|) = 0.0008          Pr(T > t) = 0.9996

*/


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
/* NON-FIXED: Analyse Sessions 31-39 */
  
  preserve
  
  keep if session>30
  
  keep if auditrate==0
  
tab highsalary, missing
mean percevaded, over(perform_high)

ttest percevaded, by(perform_high)
/*

Two-sample t test with equal variances
------------------------------------------------------------------------------
   Group |     Obs        Mean    Std. Err.   Std. Dev.   [95% Conf. Interval]
---------+--------------------------------------------------------------------
       0 |     690    .5198847    .0169824    .4460903    .4865413    .5532281
       1 |     670    .8153175    .0141856    .3671843    .7874639    .8431711
---------+--------------------------------------------------------------------
combined |   1,360    .6654288    .0117914    .4348463    .6422974    .6885601
---------+--------------------------------------------------------------------
    diff |           -.2954328    .0221903               -.3389638   -.2519018
------------------------------------------------------------------------------
    diff = mean(0) - mean(1)                                      t = -13.3136
Ho: diff = 0                                     degrees of freedom =     1358

    Ha: diff < 0                 Ha: diff != 0                 Ha: diff > 0
 Pr(T < t) = 0.0000         Pr(|T| > |t|) = 0.0000          Pr(T > t) = 1.0000

*/



return list
matrix mu = (r(mu_1), r(mu_2))
matrix list mu

bs difference = (r(mu_1)-r(mu_2)), reps(1000) seed(01010) cluster(sujeto): ttest percevaded, by(perform_high)



/*  
                                (Replications based on 136 clusters in sujeto)
------------------------------------------------------------------------------
             |   Observed   Bootstrap                         Normal-based
             |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
  difference |  -.2954328    .062678    -4.71   0.000    -.4182795   -.1725861
------------------------------------------------------------------------------
*/

restore




*******************************
**** Gender Comparisons
*******************************

/* All Treatments*/

preserve
keep if  session <4 | session==15 | session==16 | session==17 | session==18 | session>24 & session <28 | session>18 & session<23| session>30

keep if auditrate==0


bs difference = (r(mu_1)-r(mu_2)), reps(1000) seed(01010) cluster(sujeto): ttest percevaded, by(gender)


/*
                                (Replications based on 420 clusters in sujeto)
------------------------------------------------------------------------------
             |   Observed   Bootstrap                         Normal-based
             |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
  difference |  -.0867425    .036054    -2.41   0.016    -.1574071    -.016078
------------------------------------------------------------------------------


*/

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

bs difference = (r(mu_1)-r(mu_2)), reps(1000) seed(01010) cluster(sujeto): ttest percevaded, by(gender)

/*                                 

                                 (Replications based on 72 clusters in sujeto)
------------------------------------------------------------------------------
             |   Observed   Bootstrap                         Normal-based
             |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
  difference |   .0412275    .090146     0.46   0.647    -.1354555    .2179105
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

bs difference = (r(mu_1)-r(mu_2)), reps(1000) seed(01010) cluster(sujeto): ttest percevaded, by(gender)
 
bs difference = (r(mu_1)-r(mu_2)), reps(1000) seed(01010) cluster(sujeto): ttest percevaded if highsalary==0, by(gender)

/*                                 


                                 (Replications based on 36 clusters in sujeto)
------------------------------------------------------------------------------
             |   Observed   Bootstrap                         Normal-based
             |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
  difference |  -.1908748   .0968787    -1.97   0.049    -.3807535   -.0009961
------------------------------------------------------------------------------


*/
 
 
 
 
bs difference = (r(mu_1)-r(mu_2)), reps(1000) seed(01010) cluster(sujeto): ttest percevaded if highsalary==1, by(gender)

/*                                 

                                 (Replications based on 36 clusters in sujeto)
------------------------------------------------------------------------------
             |   Observed   Bootstrap                         Normal-based
             |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
  difference |  -.0709805   .1158624    -0.61   0.540    -.2980666    .1561057
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
 
bs difference = (r(mu_1)-r(mu_2)), reps(1000) seed(01010) cluster(sujeto): ttest percevaded if receiveshock==0, by(gender)

/*                                 

                                 (Replications based on 56 clusters in sujeto)
------------------------------------------------------------------------------
             |   Observed   Bootstrap                         Normal-based
             |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
  difference |   .0012624   .1053577     0.01   0.990     -.205235    .2077598
------------------------------------------------------------------------------

*/


 
bs difference = (r(mu_1)-r(mu_2)), reps(1000) seed(01010) cluster(sujeto): ttest percevaded if receiveshock==1, by(gender)

/*                                 

                                 (Replications based on 56 clusters in sujeto)
------------------------------------------------------------------------------
             |   Observed   Bootstrap                         Normal-based
             |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
  difference |   .0068475   .0858573     0.08   0.936    -.1614298    .1751248
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

bs difference = (r(mu_1)-r(mu_2)), reps(1000) seed(01010) cluster(sujeto): ttest percevaded, by(gender)

/*                                 
                                (Replications based on 84 clusters in sujeto)
------------------------------------------------------------------------------
             |   Observed   Bootstrap                         Normal-based
             |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
  difference |  -.0471375   .0738773    -0.64   0.523    -.1919343    .0976593
------------------------------------------------------------------------------

*/

restore

**** Non-Fixed
/* NON-FIXED: Analyse Sessions 31-39 */
  
  preserve
  
  keep if session>30
  
  keep if auditrate==0
  
tab highsalary, missing
mean percevaded, over(perform_high)

ttest percevaded, by(perform_high)
return list
matrix mu = (r(mu_1), r(mu_2))
matrix list mu

bs difference = (r(mu_1)-r(mu_2)), reps(1000) seed(01010) cluster(sujeto): ttest percevaded, by(gender)



/*                                  
                                (Replications based on 136 clusters in sujeto)
------------------------------------------------------------------------------
             |   Observed   Bootstrap                         Normal-based
             |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
  difference |  -.1918694   .0643457    -2.98   0.003    -.3179846   -.0657541
------------------------------------------------------------------------------

*/

restore








log close
