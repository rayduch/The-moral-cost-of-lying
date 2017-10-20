**************************************************************************
**** Data prep
**** Who Cheats
**** File prepared by Denise Laroze on the basis of Hecor Solaz's  code
**** It provides information about how different variables are coded
**************************************************************************



cd "C:\Users\André Laroze\Dropbox\CESS-Santiago\Archive\Tax Compliance Experiments\Rep Material\\Laroze rep package"

use "MasterfileOxfordChile_20160506.dta", clear


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
