


* adjust project paths based on user
global input_data_cd "E:\Lou_projects\striper_bluefish_RDM\input_data" /* Lou's local data path */
global output_data_cd "C:\Users\andrew.carr-harris\Desktop\Git\TLL_analysis" /* Lou's local data path */



u "E:\Lou_projects\flukeRDM\flukeRDM_iterative_data\directed_trip_calib_mrip_state_wave_total.dta", clear 
egen sum=sum(dtrip)
gen perc=dtrip/sum
gen n_needed=round(10000*perc)

keep state wave mode n_needed
tempfile key
save `key', replace


*Enter a directory with the expenditure survey data 
u "$input_data_cd\gulf_atl_2022.dta", clear
renvarlab *, lower


* As per Sabrina, run the following code before using the 2022 data. This code sets certain expenditure variables to missing depending on the trip mode. 
* For-Hire trips: set boat fuel and boat rental to missing
replace bfuelexp = . if mode == "For-Hire"
replace brentexp = . if mode == "For-Hire"

* Private Boat trips: set guide costs and crew tips to missing
replace guideexp = . if mode == "Private Boat"
replace crewexp  = . if mode == "Private Boat"

* Shore trips: set all of those to missing
replace bfuelexp = . if mode == "Shore"
replace crewexp  = . if mode == "Shore"
replace guideexp = . if mode == "Shore"
replace brentexp = . if mode == "Shore"


*keep only the states we need (MA-NC) 
keep if inlist(st, 25, 44, 9, 36, 34, 10, 24, 51, 37, 23, 33)

gen state="MA" if st==25
replace state="MD" if st==24
replace state="RI" if st==44
replace state="CT" if st==9
replace state="NY" if st==36
replace state="NJ" if st==34
replace state="DE" if st==10
replace state="VA" if st==51
replace state="NC" if st==37
replace state="ME" if st==23
replace state="NH" if st==33

mvencode afuelexp arentexp ptransexp lodgexp grocexp restexp baitexp iceexp parkexp bfuelexp brentexp guideexp crewexp procexp feesexp giftsexp  othexp, mv(0) override


*replace some non-trip expenses included in "other" category as zero
replace othexp=0 if inlist(oth_cat, "2 LICENSES", "BOAT REPAIR", "Boat Towing", "CART", "FISHING LICENSE")
replace othexp=0 if inlist(oth_cat,"LICENSE", "LICENSES", "MONEY SPENT AT CASINO", "NEW ROD", "SEATOW", "SPA", "HAT")


* Compute total trip expenditure
egen total_exp=rowtotal(afuelexp arentexp ptransexp lodgexp grocexp restexp baitexp iceexp parkexp bfuelexp brentexp guideexp crewexp procexp feesexp giftsexp othexp) 
svyset psu_id [pweight= sample_wt], strata(var_id) singleunit(certainty)




*Sabrina's definition of for-hire mode include both headboat and charter boats
*Survey mode definitions:
	*3=shore
	*4=headboat
	*5=charter
	*7=private boat
/*
svy: tabstat total_exp, stat(mean sd) by(state)
svy: mean total_exp if state=="MA"
svy: mean total_exp if state=="RI"
svy: mean total_exp if state=="CT"
svy: mean total_exp if state=="NY"
svy: mean total_exp if state=="NJ"
svy: mean total_exp if state=="DE"
svy: mean total_exp if state=="MD"
svy: mean total_exp if state=="VA"
svy: mean total_exp if state=="NC"
*/
/*
mat b=e(b)'
mat v= e(V)

clear 
svmat b
rename b1 mean
svmat v
rename v1 st_error
replace st_error=sqrt(st_error)
*/

gen mode1="sh" if inlist(mode_fx, "1", "2", "3")
replace mode1="fh" if inlist(mode_fx, "4", "5")
replace mode1="pr" if inlist(mode_fx,  "7")

global inflation_expansion=1.13 // CHECK

*Adjust for inflation
replace total_exp = total_exp*$inflation_expansion

tostring wave, gen(wave1)

gen domain=state+"_"+mode1+"_"+wave1
encode domain, gen(domain2)


preserve
keep domain domain2
duplicates drop 
tempfile domains
save `domains', replace 
restore


preserve
svy: mean total_exp, over(domain2)  

xsvmat, from(r(table)') rownames(rname) names(col) norestor
split rname, parse("@")
drop rname1
split rname2, parse(.)
drop rname2 rname22
rename rname21 domain2
destring domain2, replace
merge 1:1 domain2 using `domains'

drop rname domain2 _merge 
order domain

split domain, parse(_)
rename domain1 state
rename domain2 mode
rename domain3 wave

renam b cost 
keep state mode wave cost se  ll ul
order state mode wave cost se  ll ul
tempfile observed 
save `observed', replace 
restore


*Two-part ("hurdle") simulation with a calibrated lognormal for positive costs, by state×mode domain.
drop domain
egen str5 domain = concat(state mode1 wave), punct("_")
encode domain, gen(dom2)

*keep if domain=="CT_pr"
svy: mean total_exp, over(dom2)
gen cost=total_exp

* Observed cap (e.g., 99th percentile) for positive costs
/*
preserve
keep if cost>0 & !missing(cost, dom2)

tempfile caps
postfile C int dom2 double cap99 using `caps', replace

levelsof dom2, local(domlist)
foreach d of local domlist {
     _pctile cost [pw=sample_wt] if dom2==`d', p(99)
    scalar cap =  r(r1)
    post C (`d') (cap)
}
postclose C
use `caps', clear
save `caps', replace
restore
*/
*----------------------------
* Cost indicators
*----------------------------
gen byte pos_cost = cost > 0 if !missing(cost)

gen double lncost  = ln(cost)  if cost > 0
gen double lncost2 = lncost^2  if cost > 0

svy: mean pos_cost, over(dom2)



*Estimate the mean positive cost by domain (survey-weighted)
*used to calibrate the lognormal so the simulated positive-cost mean matches the survey positive-cost mean.
preserve
keep if cost>0 & !missing(dom2)

tempfile meanpos
postfile M int dom2 double mean_pos using `meanpos', replace

levelsof dom2, local(domlist)
foreach d of local domlist {
    quietly svy, subpop(if dom2==`d'): mean cost
    matrix b = e(b)
    post M (`d') (b[1,1])
}
postclose M
use `meanpos', clear
save `meanpos', replace
restore



*estimate survey conditional mean of positive costs by domain
*provides Bernoulli probability used later in simulation: spend = (runiform() < p_hat)
preserve
tempfile p_pos
postfile P int dom2 str8 domain double p_hat se_p long N using `p_pos', replace

levelsof dom2, local(domlist)

foreach d of local domlist {
    quietly svy, subpop(if dom2==`d'): mean pos_cost
    matrix b = e(b)
    matrix V = e(V)

    scalar p  = b[1,1]
    scalar se = sqrt(V[1,1])

    quietly count if dom2==`d'
    local domname : label (dom2) `d'

    post P (`d') ("`domname'") (p) (se) (r(N))
}
postclose P
restore


*Estimate lognormal dispersion for positive costs by domain (survey-weighted)
*gives the shape/variance of the positive-cost distribution on the log scale.
preserve
keep if cost > 0 & !missing(dom2, lncost, lncost2)

tempfile ln_parms
postfile L int dom2 str8 domain double mu_hat m2_hat sig2_hat double v11 v22 v12 long N using `ln_parms', replace

levelsof dom2, local(domlist)

foreach d of local domlist {
    quietly svy, subpop(if dom2==`d'): mean lncost lncost2
    matrix b = e(b)
    matrix V = e(V)

    scalar mu  = b[1,1]
    scalar m2  = b[1,2]
    scalar s2  = m2 - mu^2
    if (s2 < 1e-10) scalar s2 = 1e-10

    quietly count if dom2==`d'
    local domname : label (dom2) `d'

    post L (`d') ("`domname'") ///
        (mu) (m2) (s2) ///
        (V[1,1]) (V[2,2]) (V[1,2]) ///
        (r(N))
}
postclose L
restore

use `p_pos', clear
merge 1:1 dom2 using `ln_parms', nogen
*merge 1:1 dom2 using `caps', nogen
merge 1:1 dom2 using `meanpos', nogen

*calibrate the lognormal mean to match mean_pos
*simulated positive-cost mean should line up with the survey-estimated positive-cost mean (up to Monte Carlo error), while keeping the estimated log-variance sig2_hat
gen double mu_adj = ln(mean_pos) - 0.5*sig2_hat

*keep if domain=="DE_fh"
local n_draws = 1000

expand `n_draws'
bysort dom2: gen long draw = _n


*Simulate trip costs 
*Part A - zero costs:
gen byte spend = runiform() < p_hat

* Part B  - Positive costs
gen double cost_sim = 0
replace cost_sim = exp(rnormal(mu_adj, sqrt(sig2_hat))) if spend==1

* Check mass at zero
by dom2: egen share_zero = mean(cost_sim==0)
list dom2 domain p_hat share_zero in 1/10
*replace cost_sim = cap99 if cost_sim > cap99 & spend==1

split domain, parse(_)
rename domain1 state
rename domain2 mode
rename domain3 wave

rename draw tripid
keep mode cost tripid state wave
compress

format cost %9.2f
order state mode wave tripid cost

*compare simulated versus observed
/*
collapse (mean) cost_sim=cost (sd) sd_cost=cost, by(state mode)
merge 1:1 state mode using `observed'
gen se_sim=sqrt(sd)

order state mode cost_sim cost se_sim se
gen pct_dif=((cost_sim-cost)/cost)*100

su pct_dif
*/

*save "$input_data_cd\trip_costs.dta", replace 
merge m:1 state wave mode using `key'
drop if _merge==1
drop tripid
drop _merge
bysort state wave mode: gen tripid=_n
drop if tripid>n_
gen extra=_n
drop if extra==10001
drop extra
destring wave, replace            

tempfile costs
save `costs', replace 

*********Angler demographics (age and avidity - # trips past 12 months) *************

	* Ages and avidity come from the fishing effort survey 12 MONTH files. 
	* These data are NOT publicly available and the data have not been processeed for QA/QC like the publicly available 2-month files. 
	* Data from 2018-2023 was delivered by Lucas Johanssen on 4/23/2025. A few notes/caveats from Lucas:
		* "FES QC processes focus on the 2-month reference periods, and we do very little evaluation and editing of 12-month effort responses.  
		* Responses for these fields are essentially unedited, raw data.  
		* The final weight trimming procedures focus on reducing the impacts of outlier values on wave-level estimates. 
		* The data may include records that are highly influential with respect to 12-month effort and any estimates may be highly variable.  
		* Wave data will produce independent estimates of 12-month effort."
		
*I will use the most recent year of FES survey data available (2023)


global dems
local wvs 1 2 3 4 5 6
foreach w of local wvs{
u  "$input_data_cd\fes_person_final_2023`w'.dta", clear 


gen state="MA" if st==25
replace state="MD" if st==24
replace state="RI" if st==44
replace state="CT" if st==9
replace state="NY" if st==36
replace state="NJ" if st==34
replace state="DE" if st==10
replace state="VA" if st==51
replace state="NC" if st==37
replace state="ME" if st==23
replace state="NH" if st==33

keep if state!=""

tempfile dems`w'
save `dems`w'', replace
global dems "$dems "`dems`w''" " 

}
clear
dsconcat $dems

gen total_trips_12=boat_trips_12+shore_trips_12
gen total_trips_2=boat_trips+shore_trips

* Lou's QA/QC on the FES data 

keep if age>=16 // drop anglers below the minimum age required for license to align the age distribution with choice experiment sampling frame, which is based on licensees (16+)

replace total_trips_2=round(total_trips_2)
replace total_trips_12=round(total_trips_12)
drop if total_trips_2>total_trips_12 // drop if total trips 2 months>total trips 12 months

drop if total_trips_2>=62 // drop if total trips 2 months>60 
drop if total_trips_12>=365 // drop if total trips 12 months>365 

replace final=final/100 // sum of weights is almost 300 million, so I proportionally reduce the weights so my Stata doesn't blow up
replace final=round(final)

expand final 
su total_trips_12, detail  

egen p9995 = pctile(total_trips_12), p(99.95) // drop total_trips_12 above the 99.95 percentile
drop if total_trips_12>p9995

levelsof state, local(sts)

tempfile new
save `new', replace

clear
tempfile master
save `master', emptyok
		
foreach s of local sts{
	u `new clear '
	
	keep if state=="`s'"
	tempfile new1
	save `new1', replace
	
	levelsof wave, local(wvs)
	foreach w of local wvs{
		u `new1', clear
		sample 1000, count
	gen tripid=_n
	expand 3
	bysort tripid: gen id=_n
	gen mode="pr" if id==1
	replace mode="fh" if id==2
	replace mode="sh" if id==3
	drop id tripid
	append using `master'
    save `master', replace
    clear        
	}

	
	
                    
}

use `master', clear
bysort mode state wave: gen tripid=_n
compress   
destring wave, replace            

keep age total_trips_12 wave state mode tripid
merge 1:1 wave state mode tripid using `costs'

*save "$input_data_cd\angler_dems.dta", replace 




* Catch draws  - 2024

u "E:\Lou_projects\flukeRDM\flukeRDM_iterative_data\directed_trip_calib_mrip_state_wave_total.dta", clear 
egen sum=sum(dtrip)
gen perc=dtrip/sum
gen n_needed=round(10000*perc)

keep state wave mode n_needed
tempfile key
save `key', replace


import delimited using "$output_data_cd\catch_draws_2024.csv", clear 

gen double date_num = date(date, "DMY")
format date_num %td
gen byte   month    = month(date_num)
gen byte   wave     = cond(inlist(month,1,2),1, ///
                        cond(inlist(month,3,4),2, ///
                        cond(inlist(month,5,6),3, ///
                        cond(inlist(month,7,8),4, ///
                        cond(inlist(month,9,10),5,6)))))
tostring wave, replace


merge m:1 state wave mode using `key'

bysort state wave mode: gen tripid=_n
drop if tripid>n_
expand 3 if _n==9998

keep sf_catch
rename  sf_catch Catch
gen tripid=_n

preserve
import delimited using "$output_data_cd\TripData.csv", clear 
drop catch 
tempfile base
save `base', replace
restore

merge 1:1 tripid using `base'
drop _merge
rename tripid TripID
rename hrs HrsFished

export delimited using "$output_data_cd\TripData_new.csv", replace
su Catch
return list
