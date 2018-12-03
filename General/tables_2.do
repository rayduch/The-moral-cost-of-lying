
file open mf using "$fname", write replace
file write mf "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}" _n
file write mf "\begin{tabular}{l|cccccc|cc}" _n
file write mf "\hline\hline" _n
file close mf

forval ii=$i1/$i2 {
	local cc `: word `ii' of $conds'
	local tt `: word `ii' of $titles'
	local ap `: word `ii' of $aplist'
	estimates clear
	di `ii'
	if `ii'==1 {
		local varlist="$varlist_1"
		local varlist1="$varlist1_1"
		local tcond="$tcond_1"
		local eadd="$eadd_1"
		local scals=`" $scals_1 "'
	} 
	else if `ii'==2 {
		local varlist="$varlist_2"
		local varlist1="$varlist1_2"
		local tcond="$tcond_2"
		local eadd="$eadd_2"
		local scals=`" $scals_2 "'
	} 
	else if `ii'==3 {	
		local varlist="$varlist_3"	
		local varlist1="$varlist1_3"
		local tcond="$tcond_3"
		local eadd="$eadd_3"
		local scals=`" $scals_3 "'
	} 
	else {
		local varlist="$varlist_4"
		local varlist1="$varlist1_4"
		local tcond="$tcond_4"
		local eadd="$eadd_4"
		local scals=`" $scals_4 "'
	}
	local n: word count `tcond'
	mlogit declared_cat `varlist' if `cc'&$include_cond, cluster(subj_id)
	
	
	est store m
	forval i=1/3 {
		est res m
		margins, dydx(*) predict(outcome(`i')) post		

		forval iii=1/`n' {
			local ttt `: word `iii' of `tcond''
			local eee `: word `iii' of `eadd''
			test `ttt'
			di "`eee'"
			estadd scalar `eee'
		}
		est store m`i'
	}
	
	reg declared_frac `varlist' if `cc'&$include_cond&declared_cat==2&$ind_typenewcond, cluster(subj_id)
		forval iii=1/`n' {
			local ttt `: word `iii' of `tcond''
			local eee `: word `iii' of `eadd''
			test `ttt'
			estadd scalar `eee'
		}
		est store m4
	
	esttab m1 m2 m3 m4 using "$fname", drop(`varlist1') scalars(`scals') label mtitle("Maximal lying" "Partial lying" "Honest" "Partial lying") wide compress nonum title(`tt') `ap' fragment  prehead(&\multicolumn{6}{c|}{\bf `tt'}&\multicolumn{2}{c}{\bf `tt'}\\ $preh) postfoot(\hline\hline) se star(* 0.10 ** 0.05 *** 0.01)
	}

file open mf using "$fname", write append
file write mf "\multicolumn{9}{p{16cm}}{\tiny `note'}\\" _n
file write mf "\multicolumn{9}{l}{\tiny \sym{*} \(p<0.1\), \sym{**} \(p<0.05\), \sym{***} \(p<0.01\)}\\" _n
file write mf "\end{tabular}"
file close mf


