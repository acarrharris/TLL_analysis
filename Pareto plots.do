
*cd "Z:\Rago analysis"
*import excel using "model_summary_output.xlsx", clear firstrow

cd "C:\Users\andrew.carr-harris\Desktop\Git\TLL_analysis"
import delimited using "Model.experiments_4.csv", clear

gen pct_trips_bag_out = triptruncbag if tll=="NA"
replace pct_trips_bag_out=pct_trips_bag_out/50000000 if tll=="NA"
replace pct_trips_bag_out=triptrunctll if tll!="NA"

gen disc_land_ratio_num=discnum/landnum

gen byte is_tll = (tll != "NA")   // adjust to match your coding


gen ration_avewtmat = avewtmatinit/avewtmatfin
gen avg_wt_landing = landwt/landnum
gen landings_per_trip=  landnum/50000000

*gen byte minsize_grp = .
*replace minsize_grp = minsize if is_tll == 0
gen minsize2=minsize if minsize!="NA"
destring minsize2, replace
label define minsz ///
    12 "Min. size 12" ///
    15 "Min. size 15" ///
    17 "Min. size 17" ///
    19 "Min. size 19" ///
    21 "Min. size 21" ///
    23 "Min. size 23"

label values minsize2 minsz

*SSB ratio vs total landing 
twoway ///
    (scatter ssbratio landwt if is_tll==0, ///
        msymbol(circle) msize(tiny) ///
        mcolor(navy%60) ///
        mlabel(v1) mlabcolor(navy) mlabsize(vsmall) mlabpos(0)) ///
    (scatter ssbratio landwt if is_tll==1, ///
        msymbol(circle_hollow) msize(tiny) ///
        mcolor(maroon%70) ///
        mlabel(v1) mlabcolor(maroon) mlabsize(vsmall) mlabpos(0)) , ///
    legend(order(1 "Traditional bag/size policies" 2 "TLL policies") pos(2) ring(0) cols(1)) ///
    xtitle("Total landings (weight)") ///
    ytitle("Initial to post-fishery SSB ratio") ///
    title("SSB ratio vs. total landing") 
	
*SSB ratio vs mean CS
twoway ///
    (scatter ssbratio mean_dcs if is_tll==0, ///
        msymbol(circle) msize(small) ///
        mcolor(navy%60) ) ///
    (scatter ssbratio mean_dcs if is_tll==1, ///
        msymbol(circle) msize(small) ///
        mcolor(maroon%70) ) , /// 
    legend(order(1 "Traditional bag/size policies" 2 "TLL policies") pos(6) ring(1) cols(2)) ///
    xtitle("CV ($) ") ///
    ytitle("Initial to post-fishery SSB ratio") ///
    title("SSB ratio vs. CV")
	
twoway ///
    (scatter ssbratio landwt if is_tll==0, ///
        msymbol(circle) msize(vsmall) ///
        mcolor(navy%60)) ///
    (scatter ssbratio landwt if is_tll==1, ///
        msymbol(circle) msize(vsmall) ///
        mcolor(maroon%70) ) , ///
    legend(order(1 "Traditional bag/size policies" 2 "TLL policies") pos(2) ring(0) cols(1)) ///
    xtitle("Total landings (weight)") ///
    ytitle("Initial to post-fishery SSB ratio") ///
    title("SSB ratio vs. total landing")
	
	
twoway ///
    (scatter ssbratio landwt if is_tll==0 & minsize2==12, ///
        mcolor(navy%65) msymbol(circle) msize(small) ) ///
    (scatter ssbratio landwt if is_tll==0 & minsize2==15, ///
        mcolor(teal%65) msymbol(circle) msize(small) ) ///
    (scatter ssbratio landwt if is_tll==0 & minsize2==17, ///
        mcolor(forest_green%65) msymbol(circle) msize(small)) ///
    (scatter ssbratio landwt if is_tll==0 & minsize2==19, ///
        mcolor(orange%70) msymbol(circle) msize(small)) ///
    (scatter ssbratio landwt if is_tll==0 & minsize2==21, ///
        mcolor(cranberry%70) msymbol(circle) msize(small)) ///
    (scatter ssbratio landwt if is_tll==0 & minsize2==23, ///
        mcolor(purple%70) msymbol(circle) msize(small) ) ///
    (scatter ssbratio landwt if is_tll==1, ///
        mcolor(maroon%70) msymbol(circle) msize(small) )  , ///
    legend(order(1 "Min size 12" 2 "Min size 15" 3 "Min size 17" ///
                 4 "Min size 19" 5 "Min size 21" 6 "Min size 23" ///
                 7 "TLL policies") ///
           cols(4) pos(6) size(small)) ///
    xtitle("Total landings (weight)") ///
    ytitle("Initial to post-fishery SSB ratio") ///
    title("SSB ratio vs. total landing")
	
	
*Pareto plots figures
* biological metrics: ssbratio, ration_avewtmat, popratiowt
* fishing outcome metrics: landings_per_trip, avg_wt_landing, mean_dcs
	
twoway ///
    (scatter ssbratio landings_per_trip if is_tll==0 & minsize2==12, ///
        mcolor(navy%65) msymbol(circle) msize(small) ) ///
    (scatter ssbratio landings_per_trip if is_tll==0 & minsize2==15, ///
        mcolor(teal%65) msymbol(circle) msize(small) ) ///
    (scatter ssbratio landings_per_trip if is_tll==0 & minsize2==17, ///
        mcolor(forest_green%65) msymbol(circle) msize(small)) ///
    (scatter ssbratio landings_per_trip if is_tll==0 & minsize2==19, ///
        mcolor(orange%70) msymbol(circle) msize(small)) ///
    (scatter ssbratio landings_per_trip if is_tll==0 & minsize2==21, ///
        mcolor(cranberry%70) msymbol(circle) msize(small)) ///
    (scatter ssbratio landings_per_trip if is_tll==0 & minsize2==23, ///
        mcolor(purple%70) msymbol(circle) msize(small) ) ///
    (scatter ssbratio landings_per_trip if is_tll==1, ///
        mcolor(maroon%70) msymbol(circle) msize(small) )  , ///
    legend(order(1 "Min size 12" 2 "Min size 15" 3 "Min size 17" ///
                 4 "Min size 19" 5 "Min size 21" 6 "Min size 23" ///
                 7 "TLL policies") ///
           cols(4) pos(6) size(small)) ///
    xtitle("Landings per trip (numbers)") ///
    ytitle("Initial to post-fishery SSB ratio") ///
    title("SSB ratio vs. landings-per-trip")
	
	
	
	
*----------------------------
* 1. Define variable lists
*----------------------------
rename ssbratio ssbrat
rename ration_avewtmat wtmatrat
rename popratiowt poprat
rename landings_per_trip landtrip
rename avg_wt_landing wtland
rename mean_dcs cs

local yvars ssbrat wtmatrat poprat
local xvars landtrip wtland cs

* Nice axis titles
local ytitle_ssbrat       "Initial:post SSB"
local ytitle_wtmatrat    `"Initial:post avg. wt."' `"of mature fish"'
local ytitle_poprat       `"Initial:post total"' `"biomass (weight)"'

local xtitle_landtrip 	"Landings per trip (numbers)"
local xtitle_wtland    "Average weight of landed fish"
local xtitle_cs          "Average compensating variation per trip"

*----------------------------
* 2. Loop over all 9 plots
*----------------------------
foreach y of local yvars {
    foreach x of local xvars {

        twoway ///
            (scatter `y' `x' if is_tll==0 & minsize2==12, ///
                mcolor(navy%65) msymbol(circle) msize(small)) ///
            (scatter `y' `x' if is_tll==0 & minsize2==15, ///
                mcolor(teal%65) msymbol(circle) msize(small)) ///
            (scatter `y' `x' if is_tll==0 & minsize2==17, ///
                mcolor(forest_green%65) msymbol(circle) msize(small)) ///
            (scatter `y' `x' if is_tll==0 & minsize2==19, ///
                mcolor(orange%70) msymbol(circle) msize(small)) ///
            (scatter `y' `x' if is_tll==0 & minsize2==21, ///
                mcolor(cranberry%70) msymbol(circle) msize(small)) ///
            (scatter `y' `x' if is_tll==0 & minsize2==23, ///
                mcolor(purple%70) msymbol(circle) msize(small)) ///
            (scatter `y' `x' if is_tll==1, ///
                mcolor(maroon%70) msymbol(circle) msize(small)), ///
            ///
            legend(order(1 "Min size 12" 2 "Min size 15" 3 "Min size 17" ///
                         4 "Min size 19" 5 "Min size 21" 6 "Min size 23" ///
                         7 "TLL policies") ///
                   rows(1) pos(6) size(small)) ///
            xtitle(`"`xtitle_`x''"', size(small)) ///
            ytitle(`"`ytitle_`y''"', size(small)) ///
            name(g_`y'_`x', replace)
    }
}

*----------------------------
* 3. Combine into 3 x 3 panel
*----------------------------
grc1leg  ///
    g_ssbrat_landtrip ///
    g_ssbrat_wtland ///
    g_ssbrat_cs ///
    g_wtmatrat_landtrip ///
    g_wtmatrat_wtland ///
    g_wtmatrat_cs ///
    g_poprat_landtrip ///
    g_poprat_wtland ///
    g_poprat_cs, ///
    cols(3) ///
    imargin(small) ///
    xsize(14) ysize(12)	
	