****************************************************** Coding Sample *****************************************************

* To run code, follow the instructions from the README file in the code folder and hit run.

* This code is broken up by overall goal with more specific descriptions of what each block of code accomplishes.

**************************************************************************************************************************
*
* Steps
*
* 1. Create map of state expansion status by year
*   a) Extract state and year of expansion
*   b) Merge with polygon shapefile data, excluding Hawaii and Alaska
*   c) Construct map
*   d) Export chart
*
* 2. Generate relevant variables and construct tables of descriptive statistics
*   a) Load data, generate variables
*   b) Label relevant variables for table
*   c) Generate descriptive statistics table
*
* 3. Create variables for and conduct staggered differences-in-differences design
*   a) Generate relevant variables
*   b) Construct unweighted OLS DiD table
*   c) Construct weighted OLS DiD table
*
* 4. Observing Identifying assumption (parallel trends) for DiD
*   a) Collapse to mean of outcomes by effective year and year
*   b) Loop through each year in expansion panel, construct chart of average outcome by effective year across time
*
* 5. Observe divergence of estimated effect Medicaid expansion on uninsured rate relative to year of expansion
*   a) Construct indicators for year relative to expansion
*   b) Chart for beta coefficient divergence in weighted OLS model
*   c) Chart for beta coefficient divergence in unweighted OLS model
*
* 6. Generate simulation where treated states and panel are randomized
*   a) Generate program that randomizes treatment, panel, and records beta estimate
*   b) Run simulation 1000 times, display density of beta estimates
* 7. Stacked DiD
*
**************************************************************************************************************************

** Setting Global Directory

if c(username)=="johnmartin" {
	global dir "/Users/johnmartin/Documents/McNair Program/Medicaid"
}

***********************************************************
***** 1. Create map of state expansion status by year *****
***********************************************************

*   a) Extract state and year of expansion

use "${dir}/Cleaned/Medicaid.dta", clear

preserve

keep if year == 2016

encode state, g(state_id)

xtset state_id year

*   b) Merge with polygon shapefile data, excluding Hawaii and Alaska

merge 1:1 NAME using "state.dta"
sort _merge
keep if _merge == 3
drop if NAME == "Alaska" | NAME == "Hawaii"

*   c) Construct map

spmap effyear using statexy, id(id) fcolor(RdYlBu) legend(label(1 "Has not adopted")) legend(label(2 "2014")) legend(label(3 "2015-2016")) legend(label(4 "2017-pres")) legend(size(large))

*   d) Export chart

graph export "${dir}/Output/us_map.png", replace

*****************************************************************************************
***** 2. Generate relevant variables and construct tables of descriptive statistics *****
*****************************************************************************************

*   a) Load data, generate variables
use "${dir}/Cleaned/Medicaid.dta", clear

* Log Medicaid Enrollment rate
gen medenr_rt = 100000 * (medenr / popestimate)
gen ln_medenr_rt = ln(medenr_rt)

* Log Medicare Enrollment Rate
gen medicare_enr_rt = 100000 * (medicare_enr / popestimate)
gen ln_medicare_enr = ln(medicare_enr_rt)

* Average population estimate by state
bysort state: egen avgpop = mean(popestimate)

* Log Medicaid Costs Rate
gen med_spending = (dental_services + durable_medical_products + home_health_care + hospital_care + nondurable_products + nursing_home_care + personal_care + personal_health_care + phys_and_clin_services + professional_services + medicare_spending + phi_spending) / popestimate
gen ln_med_spending = ln(med_spending)


*   b) Label relevant variables for table

label variable medenr "Medicaid Enrollment"
label variable uninsr "Uninsured Rate"
label variable ln_uninsr "Log Uninsred Rate"
label variable ln_medicare_enr "Log Medicare Enrollment Rate"
label variable ln_phi_enr "Log Private Health Insurance Enrollment Rate"
label variable phi_enr "PHI Enrollment"
label variable medenr "Medicare Enrollment"
label variable uninsr "Uninsured Rate"
label variable poverty "Poverty Rate"

*   c) Generate descriptive statistics table

eststo clear
eststo: estpost tabstat poverty phi_enr medenr uninsr, c(stat) stat(mean sd) listwise
eststo: estpost tabstat poverty phi_enr medenr uninsr [aw=avgpop], c(stat) stat(mean sd) listwise

* Final Table
esttab using "${dir}/Output/summstats.tex", unstack nostar noobs nonumber title("Descriptive Statistics") cells(mean(fmt(2)) sd(par fmt(2))) compress gaps par collabels(none) label mtitles("Mean (Unweighted)" "Mean (Weighted by Population)") depvars replace addnotes("Notes: Each cell contains the mean with the standard deviation in parentheses. All variables have 969 observations. Data is measured by the U.S. Census Bureau and the Department of Labor Statistics.")


*******************************************************************************************
***** 3. Create variables for and conduct staggered differences-in-differences design *****
*******************************************************************************************

*   a) Generate relevant variables

sort state year

* Year relative to expansion
by state: gen relyear = year - effyear

* Log uninsured rate (outcome)
gen ln_uninsr = ln(uninsr)

* Year fixed effects
tabulate year, g(Iyear)

* State fixed effects
tabulate state, g(Istate)

* Two-year pre trend indicator
bys state: gen pre2_medic = (year - effyear == 1 | year - effyear == 2)

* State linear time trends
tabulate state, g(Itimelin)
forvalues i = 1(1)50 {
	replace Itimelin`i' = year - 2001 + 1 if Itimelin`i' == 1
}

* Region indicator
tab region, g(Iregion)

rename Iregion1 far_west
rename Iregion2 great_lakes
rename Iregion3 mideast
rename Iregion4 new_england
rename Iregion5 plains
rename Iregion6 rocky_mountains
rename Iregion7 southeast
rename Iregion8 southwest

* Label new variables
label variable medic "Medicaid Expansion"
label variable ln_medicare_enr "Log Medicare Enrollment Rate"
label variable ln_phi_enr "Log PHI Enrollment Rate"
label variable ln_uninsr "Log Uninsured Rate"
label variable pre2_medic "0 to 2 years prior to adoption"

*   b) Construct unweighted OLS DiD table

replace effyear = . if effyear > 2019

foreach outcome in ln_uninsr ln_phi_enr ln_medicare_enr ln_medenr_rt ln_med_spending {
	eststo clear
	eststo: reg `outcome' Istate* Iyear2-Iyear7 medic, vce(cluster state)
	eststo: reg `outcome' Istate* Iyear* far_west##i.year great_lakes##i.year mideast##i.year new_england##i.year plains##i.year rocky_mountains##i.year southeast##i.year southwest##i.year medic, vce(cluster state)
	eststo: reg `outcome' Istate* Iyear* far_west##i.year great_lakes##i.year mideast##i.year new_england##i.year plains##i.year rocky_mountains##i.year southeast##i.year southwest##i.year poverty medic, vce(cluster state)
	eststo: reg `outcome' Istate* Iyear* far_west##i.year great_lakes##i.year mideast##i.year new_england##i.year plains##i.year rocky_mountains##i.year southeast##i.year southwest##i.year pre2_medic poverty medic, vce(cluster state)
	eststo: reg `outcome' Istate* Iyear* far_west##i.year great_lakes##i.year mideast##i.year new_england##i.year plains##i.year rocky_mountains##i.year southeast##i.year southwest##i.year ln_medicare_enr poverty medic, vce(cluster state)
	eststo: reg `outcome' Istate* Iyear* far_west##i.year great_lakes##i.year mideast##i.year new_england##i.year plains##i.year rocky_mountains##i.year southeast##i.year southwest##i.year poverty Itimelin* medic, vce(cluster state)
	esttab using "${dir}/Output/unw_`outcome'.tex", keep(medic pre2_medic) nonumbers mtitles("A" "B" "C" "D" "E" "F") label style(tex) title(`: variable label `outcome'' — OLS Unweighted) se replace
}
 
*   c) Construct weighted OLS DiD table
 
foreach outcome in ln_uninsr ln_phi_enr ln_medicare_enr ln_medenr_rt ln_med_spending {
	eststo clear
	eststo: reg `outcome' Istate* Iyear2-Iyear7 medic [aw=avgpop], vce(cluster state)
	eststo: reg `outcome' Istate* Iyear* far_west##i.year great_lakes##i.year mideast##i.year new_england##i.year plains##i.year rocky_mountains##i.year southeast##i.year southwest##i.year medic [aw=avgpop], vce(cluster state)
	eststo: reg `outcome' Istate* Iyear* far_west##i.year great_lakes##i.year mideast##i.year new_england##i.year plains##i.year rocky_mountains##i.year southeast##i.year southwest##i.year poverty medic [aw=avgpop], vce(cluster state)
	eststo: reg `outcome' Istate* Iyear* far_west##i.year great_lakes##i.year mideast##i.year new_england##i.year plains##i.year rocky_mountains##i.year southeast##i.year southwest##i.year pre2_medic poverty medic [aw=avgpop], vce(cluster state)
	eststo: reg `outcome' Istate* Iyear* far_west##i.year great_lakes##i.year mideast##i.year new_england##i.year plains##i.year rocky_mountains##i.year southeast##i.year southwest##i.year ln_medicare_enr poverty medic [aw=avgpop], vce(cluster state)
	eststo: reg `outcome' Istate* Iyear* far_west##i.year great_lakes##i.year mideast##i.year new_england##i.year plains##i.year rocky_mountains##i.year southeast##i.year southwest##i.year poverty Itimelin* medic [aw=avgpop], vce(cluster state)
	esttab using "${dir}/Output/w_`outcome'.tex", keep(medic pre2_medic) nonumbers mtitles("A" "B" "C" "D" "E" "F") label style(tex) title(`: variable label `outcome'' — OLS Weighted) se replace addnotes("Weighted by Population")
}

*************************************************************************
***** 4. Observing Identifying assumption (parallel trends) for DiD *****
*************************************************************************

* Set scheme for a more visually appealing chart
set scheme plotplainblind

* Preserve state of data
preserve

*   a) Collapse to mean of outcomes by effective year and year

collapse (mean) ln_uninsr ln_medenr_enr ln_phi_enr, by(effyear year)

*   b) Loop through each year in expansion panel, construct chart of average outcome by effective year across time

forv y=2014/2019{
	local j = `y'-0.25
	foreach outcome in ln_uninsr ln_medenr_rt ln_phi_enr{
		twoway (connected `outcome' year if effyear==.)(connected `outcome' year if effyear==`y'), xline(`j') xtitle("Year", size(medium)) ytitle(`: variable label `outcome'', size(medium)) legend(label(1 "Control")) legend(label(2 "Treatment")) legend(size(medium)) name(`outcome'`y', replace)
		graph export "${dir}/Output/raw`outcome'_effyear`y'.png", replace
	}
}

* Restore data to original state
restore

************************************************************************************************************************
***** 5. Observe divergence of estimated effect Medicaid expansion on uninsured rate relative to year of expansion *****
************************************************************************************************************************

*   a) Construct indicators for year relative to expansion

gen neg = (relyear < 0)
gen relyearnew = abs(relyear)

* Prior to expansion
forv i = 9(-1)1{
	gen div_`i' = 0
	replace div_`i' = 1 if (relyearnew == `i' & neg == 1)
	label variable div_`i' "-`i'"
}

* After/during expansion
forv i = 0/5{
	gen div`i' = 0
	replace div`i' = 1 if (relyearnew == `i' & neg == 0)
	label variable div`i' "`i'"
}

* Include all post years in divergence nine or more years prior as one indicator

replace div_9 = 1 if relyear <= -9

*   b) Chart for beta coefficient divergence in weighted and unweighted OLS model

foreach outcome in ln_uninsr ln_phi_enr ln_medicare_enr ln_medenr_rt ln_med_spending {
	eststo clear
	eststo m1: reg `outcome' div_8 div_7 div_6 div_5 div_4 div_3 div_2 div_1 div0 div1 div2 div3 div4 div5 Istate* Iyear* far_west##i.year great_lakes##i.year mideast##i.year new_england##i.year plains##i.year rocky_mountains##i.year southeast##i.year southwest##i.year poverty

	coefplot m1, drop(_cons) keep(div_8 div_7 div_6 div_5 div_4 div_3 div_2 div_1 div0 div1 div2 div3 div4 div5)  vertical xtitle("Years Relative to Adoption of Medicaid") ytitle("Beta Values for Relative Medicaid Effective Year", margin(medium)) recast(connected) ci omitted xlabel(, valuelabel) xline(9)

	graph export "${dir}/Output/DiD_unweighted_`outcome'.png", replace
	
	eststo clear
	eststo m1: reg `outcome' div_9 div_8 div_7 div_6 div_5 div_4 div_3 div_2 div_1 div0 div1 div2 div3 div4 div5 Istate* Iyear* far_west##i.year great_lakes##i.year mideast##i.year new_england##i.year plains##i.year rocky_mountains##i.year southeast##i.year southwest##i.year poverty [aw=avgpop]

	coefplot m1, drop(_cons) keep(div_9 div_8 div_7 div_6 div_5 div_4 div_3 div_2 div_1 div0 div1 div2 div3 div4 div5)  vertical xtitle("Years Relative to Adoption of Medicaid") title("Weighted OLS") ytitle("Beta Values for Relative Medicaid Effective Year", margin(medium)) recast(connected) ci omitted xlabel(, valuelabel)
	graph export "${dir}/Output/DiD_weighted_`outcome'.png", replace

}

********************************************************************************
***** 6. Generate simulation where treated states and panel are randomized *****
********************************************************************************

*   a) Generate program that randomizes treatment, panel, and records beta estimate

program define myprog3
	* Pull data
	use "${dir}/Cleaned/Medicaid.dta", clear
	* Generate observation ID
	gen obs_id = _n
	* Generate state ID
	gen block_id = floor((obs_id - 1) / 19) + 1
	* Generate random number
	gen random_order = runiform()
	* Randomize with mean of random number generated
	bysort block_id: egen block_random = mean(random_order)
	* Sort by randomization
	sort block_random year
	* Generate new observation ID, distinct from original
	gen new_id = _n
	* Generate new state ID, distinct from original
	gen new_block_id = floor((new_id - 1) / 19) + 1
	* Sort by new block ID
	sort new_block_id year
	* Drop original observation ID, replacing with new one
	drop obs_id
	rename new_id obs_id
	* Merge with dataset only containing original order
	merge 1:1 obs_id using "${dir}/Cleaned/observations.dta"
	drop _merge
	
	* Generate new treatment variable
	gen med = .
	order med, after(medic_new)
	* Construct new potential start to treatment
	gen yr = runiformint(2001, 2013)
	* Shift panel to top if at the bottom of dataset
	replace med = medic_new[_n-yr+1995] if _n > yr - 1995
	* Shift by new panel if not
	replace med = medic_new[_N - yr +1995 + _n] if _n <= yr - 1995
	* Drop years beyond new panel
	drop if (year > yr + 6)
	* Run preferred model, obtaining beta estimate
	reg ln_uninsr Istate* Iyear* far_west##i.year great_lakes##i.year mideast##i.year new_england##i.year plains##i.year rocky_mountains##i.year southeast##i.year southwest##i.year med [aw = avgpop], vce(cluster state)
end

*   b) Run simulation 1000 times, display density of beta estimates

simulate _b, reps(1000): myprog3

* Construct density chart
kdensity _b_med, title("Density of Simulated Beta Coefficients") ///
    ylabel(, labsize(small)) ///
    xtitle("Beta Coefficients for Log Uninsured Rate") ytitle("Density") ///
    lwidth(medium)

* Export chart
graph export "${dir}/Output/sim.png", replace


**************************
***** 7. Stacked DiD *****
**************************

use "${dir}/Cleaned/Medicaid.dta", clear

keep effyear year state ln_uninsr

rename effyear adopt_year

egen statefip = group(state)

rename state st

/* Create sub-experiment data for stack */

* clear programs
capture program drop _all

* start new program
program create_sub_exp
syntax, ///
	timeID(string) ///
	groupID(string) ///
	adoptionTime(string) ///
	focalAdoptionTime(int) ///
	kappa_pre(numlist) ///
	kappa_post(numlist)
	* Suppress output
	qui{
		* Save dataset in memory, so we can call this function multiple times. 
		preserve

		* Determine earliest and latest time in the data. 
			* Used for feasibility check later
		sum `timeID'
		local minTime = r(min)
		local maxTime = r(max)

		
		*variable to label sub-experiment if treated in focalAdoptionTime, 
		gen sub_exp = `focalAdoptionTime' if `adoptionTime' == `focalAdoptionTime'
		
		*Now fill in this variable for states with adoptionTime > focalAdoptionTime + kappa_post
		*note, this will include never treated, because adopt_year is ., which stata counts as infinity
		replace sub_exp = `focalAdoptionTime' if `adoptionTime' > `focalAdoptionTime' + `kappa_post'
		
		*Keep only treated and clean controls
		keep if sub_exp != .
		
		*gen treat variable in subexperiment
		gen treat = `adoptionTime' == `focalAdoptionTime'
		
		*gen event_time and 
		gen event_time = year - sub_exp
		
		*gen post variable
		gen post = event_time >= 0
		
		*trim based on kappa's: -kappa_pre < event_time < kappa_post
		keep if inrange(event_time, -`kappa_pre', `kappa_post')
		
		*keep if event_time >= -`kappa_pre' & event_time <= `kappa_post'
		gen feasible = 0 
		replace feasible = 1 if !missing(`adoptionTime')
		replace feasible = 0 if `adoptionTime' < `minTime' + `kappa_pre' 
		replace feasible = 0 if `adoptionTime' > `maxTime' - `kappa_post' 
		drop if `adoptionTime' < `minTime' + `kappa_pre' 

		* Save dataset
		compress
		save "${dir}/subexp`focalAdoptionTime'", replace
		restore
	}
end

* Save dataset
preserve

* Run this function with focal year 2014
create_sub_exp, ///
	timeID(year) ///
	groupID( statefip) ///
	adoptionTime(adopt_year) ///
	focalAdoptionTime(2014) ///
	kappa_pre(3) ///
	kappa_post(2)

* Open temp dataset created with function
use "${dir}/subexp2014.dta", clear

* Summarize
sum statefip year adopt_year ln_uninsr  treat  post event_time feasible sub_exp 

* Restore dataset
restore

//create the sub-experimental data sets

levelsof adopt_year, local(alist)
di "`alist'"
qui{
// Loop over the events and make a data set for each one
foreach j of numlist `alist' { 
  // Preserve dataset
  preserve

  // run function
  create_sub_exp, ///
    timeID(year) ///
    groupID( statefip) ///
    adoptionTime(adopt_year) ///
    focalAdoptionTime(`j') ///
    kappa_pre(3) ///
    kappa_post(2)

  // restore dataset
  restore
}

// Append the stacks together, but only from feasible stacks
        * Determine earliest and latest time in the data. 
            * Used for feasibility check later
          sum year
          local minTime = r(min)
          local maxTime = r(max)
		  local kappa_pre = 3
		  local kappa_post= 2

gen feasible_year = adopt_year
replace feasible_year = . if adopt_year < `minTime' + `kappa_pre' 
replace feasible_year = . if adopt_year > `maxTime' - `kappa_post' 
sum feasible_year

local minadopt = r(min)
levelsof feasible_year, local(alist)
clear
foreach j of numlist `alist'  {
    display `j'
    if `j' == `minadopt' use "${dir}/subexp`j'", clear
    else append using "${dir}/subexp`j'"
}

// Clean up 
* erase temp/subexp`j'.dta
}

* Summarize
sum statefip year adopt_year ln_uninsr  treat  post event_time feasible sub_exp

* Treated, control, and total count by stack
preserve
keep if event_time == 0
gen N_treated = treat 
gen N_control = 1 - treat 
gen N_total = 1
collapse (sum) N_treated N_control N_total, by(sub_exp)
// list sub_exp N_treated N_control N_total in 1/4
/*
sumup treat if event_time == 0, s(N)
stacked_dtc[event_time==0, 
            .(N_treated = fsum(treat), 
              N_control = fsum(1-treat), 
              N_total = .N
              ), 
            by = sub_exp][order(sub_exp)]
*/
restore

/* Create Weights */
capture program drop _all
program compute_weights
syntax, ///
	treatedVar(string) ///
	eventTimeVar(string) ///
  groupID(string) ///
	subexpVar(string) 

  // Create weights
  bysort `subexpVar' `groupID': gen counter_treat = _n if `treatedVar' == 1
  egen n_treat_tot = total(counter_treat)
  by `subexpVar': egen n_treat_sub = total(counter_treat) 

  bysort `subexpVar'  `groupID': gen counter_control = _n if `treatedVar' == 0
  egen n_control_tot = total(counter_control)
  by `subexpVar': egen n_control_sub = total(counter_control) 


  gen stack_weight = 1 if `treatedVar' == 1
  replace stack_weight = (n_treat_sub/n_treat_tot)/(n_control_sub/n_control_tot) if `treatedVar' == 0
end

compute_weights, ///
	treatedVar(treat) ///
	eventTimeVar(event_time) ///
  groupID(statefip) ///
	subexpVar(sub_exp) 

* Summarize 
sumup stack_weight if treat == 0 & event_time == 0, by(sub_exp) s(mean) 

// Create dummy variables for event-time
char event_time[omit] -1
xi i.event_time

// Run regression
qui reghdfe ln_uninsr i.treat##i._I* [aw = stack_weight], cluster(statefip) absorb(treat event_time)
est sto weight_stack

// Show results
esttab weight_stack, keep(1.treat#1*) se

tab sub_exp, g(Isub_exp)

reghdfe ln_uninsr i.treat##i.sub_exp i._I*##i.sub_exp [aw = stack_weight], cluster(statefip) absorb(treat)

lincom (1.treat#1._Ievent_tim_4 + 1.treat#1._Ievent_tim_5 + 1.treat#1._Ievent_tim_6)/3
