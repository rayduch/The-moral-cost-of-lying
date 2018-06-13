egen declared_frac_lim=mean( dd), by(subj_id)
egen declared_frac_lim_sd=sd( dd), by(subj_id)
egen declared_frac_lim_n=count( dd), by(subj_id)
label variable declared_frac_lim "Average declared among 1-99"
label variable declared_frac_lim_sd "SD declared among 1-99"
label variable declared_frac_lim_n "Number declared among 1-99"

keep declared_frac_lim declared_frac_lim_sd declared_frac_lim_n subj_id
duplicates drop
save "Z:\home\alexei\Dropbox\tax_experiment\tables\Tax Compliance Moscow\Raw Data\limited.dta", replace
use "Z:\home\alexei\Dropbox\tax_experiment\tables\Tax Compliance Moscow\Raw Data\subjects.dta", clear
drop _merge
drop declared_frac_lim declared_frac_lim_n declared_frac_lim_sd
merge 1:1 subj_id using "Z:\home\alexei\Dropbox\tax_experiment\tables\Tax Compliance Moscow\Raw Data\limited.dta"
save "Z:\home\alexei\Dropbox\tax_experiment\tables\Tax Compliance Moscow\Raw Data\subjects.dta", replace

local minn=2

hist declared_frac_lim_sd if declared_frac_lim_n>=`minn', by(country_code)
graph export "Z:\home\alexei\Dropbox\tax_experiment\tables\Tax Compliance Moscow\Raw Data\limited_sd.eps", as(eps) preview(off) replace
hist declared_frac_lim if declared_frac_lim_n>=`minn'& declared_frac_lim_sd<.2 , by(country_code)
graph export "Z:\home\alexei\Dropbox\tax_experiment\tables\Tax Compliance Moscow\Raw Data\limited_mean.eps", as(eps) preview(off) replace

ksmirnov declared_frac_lim if declared_frac_lim_n>=2&inlist(country_code,2,3)&declared_frac_lim_sd<.2, by(country_code)
ksmirnov declared_frac_lim if declared_frac_lim_n>=2&inlist(country_code,1,3)&declared_frac_lim_sd<.2, by(country_code)
ksmirnov declared_frac_lim if declared_frac_lim_n>=2&inlist(country_code,1,2)&declared_frac_lim_sd<.2, by(country_code)
ksmirnov declared_frac_lim_sd if declared_frac_lim_n>=2&inlist(country_code,2,3), by(country_code)
ksmirnov declared_frac_lim_sd if declared_frac_lim_n>=2&inlist(country_code,1,3), by(country_code)
ksmirnov declared_frac_lim_sd if declared_frac_lim_n>=2&inlist(country_code,1,2), by(country_code)

hist declared_frac_lim if declared_frac_lim_n>=`minn'& declared_frac_lim_sd<.15 , by(country_code) xtitle("Average declarations in the 1-99 range, SD<.15")
graph export "Z:\home\alexei\Dropbox\tax_experiment\tables\Tax Compliance Moscow\Raw Data\limited_mean_sdunder15.eps", as(eps) preview(off) replace
