
local uplim="1 10 20 30 40 50 60 70 80 90"
local clist="Chile Russia UK"
matrix M=J(9,10,.)

capture confirm variable near_temp
if (_rc == 0) {
	drop near_temp
}
gen near_temp=.

local i=1
local j=2
forval iii=1/3 {
forval ii=1/10 {
	local ul `: word `ii' of `uplim''
	quietly replace near_temp=0
	quietly replace near_temp=1 if declared>0&declared<=`ul'
	quietly tabstat near_temp if include_data==1&country_code==`iii', by($byvar) stats(mean sd n) save

	matrix l1=r(Stat1)
	matrix l2=r(Stat2)
	*local se=sqrt(l`i'[2,1]^2/l`i'[3,1]+l`j'[2,1]^2/l`j'[3,1])
	*local t=abs(l`i'[1,1]-l`j'[1,1])/`se'
	*local df=min(l`i'[3,1],l`j'[3,1])-1
	*local pp=tprob(`df',`t')
	*quietly cc $byvar near_temp if include_data==1&country_code==`iii', exact
	quietly reg  near_temp $byvar if include_data==1&country_code==`iii', clu(subj_id)
	mat eb=e(b)
	mat eV=e(V)
	local pp=ttail(e(df_r),abs(eb[1,1]/sqrt(eV[1,1])))*2
	
	*local pp=  r(p_exact)

	
	mat M[1+3*(`iii'-1),`ii']=l`i'[1,1]
	mat M[2+3*(`iii'-1),`ii']=l`j'[1,1]
	mat M[3+3*(`iii'-1),`ii']=`pp'
}
}


file open mf using "$fname", write replace
file write mf "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}" _n
file write mf "\begin{tabular}{|ll|cccccccccc|}" _n
file write mf "\hline\hline" _n
file write mf "&"
forval i=1/10 {
	local ul `: word `i' of `uplim''
	file write mf "&1-`ul' ECU"
}
file write mf "\\" _n
file write mf "\hline" _n
forval ii=1/3 {
	local cc `: word `ii' of `clist''
	forval iii=1/3 {
		local ll `: word `iii' of $llist'
		if `iii'==2 {
			file write mf "`cc'"
		}
		file write mf "&`ll'"
		forval j=1/10 {
			local mm=M[`iii'+3*(`ii'-1),`j']
			file write mf "&"
			file write mf %9.4f (`mm')
			
			
		}
		file write mf "\\" _n
	}
	file write mf "\hline"	
}
file write mf "\multicolumn{11}{p{15cm}}{\tiny For each country, the first two rows report the frequencies of declarations for two groups of subjects. The third row reports the p-value for the OLS regression where the dependent variable is 0 or 1 (if there is near-maximal cheating), and the independent variable is the dummy the subject group, and standard errors are clustered by subject. }\\" _n
file write mf "\end{tabular}"
file close mf

