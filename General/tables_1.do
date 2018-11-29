local varlist = "ncorrect_rank i.male age i.offerdg_0 offerdg_frac i.tax_20 i.tax_30 i.tax_40 i.tax_50 i.deadweight i.mpcr i.shock i.status i.status_H i.non_fixed"
local varlist2 = " "
local varlist3 = "0.male 0.offerdg_0 0.tax_20 0.tax_30 0.tax_40 0.tax_50 0.deadweight 0.mpcr 0.shock 0.status 0.status_H 0.non_fixed 0.russia 0.uk"

local preh="&\multicolumn{8}{c|}{Mlogit, average marginal effects }&\multicolumn{2}{c}{OLS}\\"



file open mf using "$fname", write replace
file write mf "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}" _n
file write mf "\begin{tabular}{l|cccccccc|cc}" _n
file write mf "\hline\hline" _n
file close mf

	mlogit $vname `varlist' `varlist2' i.russia i.uk if include_data==1&period2==1, robust 
	est store m
	forval i=1/4 {
		est res m
		margins, dydx(*) predict(outcome(`i')) post 
			test 1.tax_20=1.tax_30
			estadd scalar t2030=r(p)
			local scals="t2030"
			test 1.tax_20=1.tax_40
			estadd scalar t2040=r(p)
			test 1.tax_20=1.tax_50
			estadd scalar t2050=r(p)
			test 1.tax_30=1.tax_40		
			estadd scalar t3040=r(p)
			test 1.tax_30=1.tax_50
			estadd scalar t3050=r(p)
			test 1.tax_40=1.tax_50
			estadd scalar t4050=r(p)
			test 1.russia=1.uk
			estadd scalar rusuk=r(p)
			local scals=`" "t2030 D20=D30" "t2040 D20=D40" "t2050 D20=D50" "t3040 D30=D40" "t3050 D30=D50" "t4050 D40=D50" "rusuk Russia=UK" "'
		*test status_H=status_L
		est store m`i'
	}

reg declared_part_av `varlist' i.russia i.uk if period2==1&include_data==1&declared_f_n>=$nnn
			test 1.tax_20=1.tax_30
			estadd scalar t2030=r(p)
			test 1.tax_20=1.tax_40
			estadd scalar t2040=r(p)
			test 1.tax_20=1.tax_50
			estadd scalar t2050=r(p)
			test 1.tax_30=1.tax_40		
			estadd scalar t3040=r(p)
			test 1.tax_30=1.tax_50
			estadd scalar t3050=r(p)
			test 1.tax_40=1.tax_50
			estadd scalar t4050=r(p)
			test 1.russia=1.uk
			estadd scalar rusuk=r(p)
est store m5

esttab m1 m2 m3 m4 m5 using "$fname", drop(`varlist3') scalars(`scals') label mtitle("Consistent maximal" "Consistent partial" "Consistently honest" "Other" "Partial lying") wide compress nonum title(`tt') append fragment  prehead(`preh') postfoot(\hline\hline) se star(* 0.10 ** 0.05 *** 0.01)
file open mf using "$fname", write append
file write mf "\multicolumn{11}{p{16.5cm}}{\tiny $note}\\" _n
file write mf "\multicolumn{11}{l}{\tiny \sym{*} \(p<0.1\), \sym{**} \(p<0.05\), \sym{***} \(p<0.01\)}\\" _n
file write mf "\end{tabular}"
file close mf

	coefplot m1, bylabel(Cons. Maximal) || m2, bylabel(Cons. Partial) || m3, bylabel(Cons. Honest) || m4, bylabel(Other) ||, drop(_cons) xline(0) byopts(row(1) note("The graph reports average marginal effects and 95% confidence intervals for multinomial logistic regression. The dependent variable is" "whether the subject is a consistent maximal liar, consistent partial liar, is consistently honest, or none of those. Robust standard errors." "RET rank is the national rank, between 0 and 1, of subject's national performance at the real effort task. RET Deviation is the difference" "between actual number of correct additions and one predicted from subject and period FE. DG frac is the fraction of the 1000 ECU donated" "in the dictator game.")) xsize(7)
	graph export "$fname_gr", as(eps) preview(off) replace
