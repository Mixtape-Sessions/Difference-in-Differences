********************************************************************************
* name: castle_all.do
* author: scott cunningham (baylor)
* description: castle doctrine stata applications
* last updated: february 12, 2022
********************************************************************************

clear
capture log close

use "https://github.com/scunning1975/mixtape/raw/master/castle.dta", clear

** Construct five examples

* 1. Estimation with did_imputation of Borusyak et al. (2021)
did_imputation l_homicide sid year effyear, autosample 

* 3. Estimation with csdid of Callaway and Sant'Anna (2020)
csdid l_homicide, ivar(sid) time(year) gvar(effyear) notyet method(dripw) 
csdid_stats simple

* 4.  Estimation with eventstudyinteract of Sun and Abraham (2020)
sum effyear
gen lastcohort = effyear==r(max) // dummy for the latest- or never-treated cohort
forvalues l = 0/5 {
	gen L`l'event = time_til==`l'
}
forvalues l = 1/5 {
	gen F`l'event = time_til==-`l'
}
drop F1event // normalize K=-1 (and also K=-5) to zero
eventstudyinteract l_homicide L*event F*event, vce(cluster sid) absorb(sid year) cohort(effyear) control_cohort(lastcohort)

* 5. TWFE OLS estimation. 
reghdfe l_homicide F*event L*event, a(sid year) cluster(sid)

