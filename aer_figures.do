/*==================================================================================================
Project:       All Eyes on Them: A Field Experiment on Citizen Oversight and Electoral Integrity
Author:        Mateo Montenegro 
Purpose:       This code produces the figures in the main text and the appendix.
----------------------------------------------------------------------------------------------------
Index:		   A. Defining globals and locals
			   B. Figures in main text
			   C. Figures in online appendix
===================================================================================================*/

est clear
set more off

/*===============================================================================================
===============================================================================================
===============================================================================================
								A. Defining globals and locals
===============================================================================================
===============================================================================================
===============================================================================================*/

* Defining control sets:

#delimit ;
loc political n_cand_real pct_santos_2p2014 pct_zuluaga_2p2014 win_margin_a2015 part_c2018 pct_blanco_c2018
pct_cambioradical_c2018 pct_centrodem_c2018 pct_conservador_c2018 pct_decentes_c2018
pct_liberal_c2018 pct_partU_c2018 pct_polo_c2018 pct_verde_c2018 ;
#delimit cr

loc violence homicidios_pc

loc development  indruralildad2017  nbi2005 pib_num_pc2016 

loc other l_pop2018 penetration_fb_pre reports_any_MOE_c2018 reports_any_MOE_a2015 z_index_second_a2015 sig95_max_second_a2015

loc candidates l_age female  incumbent_loose incumbent citizen_group coalition winner_c


global controls "`political'  `violence' `development'  `other' ss* rr*"
global controls_can "`political'  `violence' `development'  `other' `candidates' n_respuestas_pre ss* rr*"

/*===============================================================================================
===============================================================================================
===============================================================================================
								B. Figures in main text
===============================================================================================
===============================================================================================
===============================================================================================*/


/*===============================================================================================
=================================================================================================
								Figure 4: Impacts on Reports
=================================================================================================
===============================================================================================*/

use "$final_data/main_data_municipal_level.dta", clear 

global outcomes "reportMOE_any d_reportMOE_any"

local i=0

foreach y of global outcomes{


quietly: rlasso `y'  $controls , cluster(ID)
local ySel `e(selected)'

quietly: rlasso T_FB  $controls , cluster(ID)
local xSel0 `e(selected)' 

quietly: rlasso TT_FB  $controls , cluster(ID)
local xSel1 `e(selected)' 

quietly: rlasso T_FB_call  $controls , cluster(ID)
local xSel2 `e(selected)' 

quietly: rlasso T_FB_info  $controls , cluster(ID)
local xSel3 `e(selected)' 

quietly: rlasso T_FB_both  $controls , cluster(ID)
local xSel4 `e(selected)' 

quietly: rlasso T_Letter  $controls , cluster(ID)
local xSel5 `e(selected)'

quietly: rlasso T_Letter_no_sj  $controls , cluster(ID)
local xSel6 `e(selected)' 

quietly: rlasso T_Letter_sj  $controls , cluster(ID)
local xSel7 `e(selected)' 


local vSel : list ySel | xSel0
local vSel : list vSel | xSel1
local vSel : list vSel | xSel2
local vSel : list vSel | xSel3
local vSel : list vSel | xSel4
local vSel : list vSel | xSel5
local vSel : list vSel | xSel6
local vSel : list vSel | xSel7



* Any Treat:

matrix pexact=J(1,3,.) 

gen group=1
replace group=2 if _n==1

sum `y' if T_FB==0
local control=`r(mean)'

gen est= `control' if group==1

reg `y' T_FB  `vSel' , cluster(ID)

replace est= `control'+_b[T_FB] if group==2
gen ci_low= est- 1.96*_se[T_FB] if group==2
gen ci_high= est+ 1.96*_se[T_FB] if group==2



* Letters:

gen group1=1
replace group1=2 if _n==1
replace group1=3 if _n==2

gen est1= `control' if group1==1


reg `y' i.Treat_Letter `vSel' , cluster(ID)

replace est1= `control'+_b[1.Treat_Letter] if group1==2
gen ci_low1= est1- 1.96*_se[1.Treat_Letter] if group1==2
gen ci_high1= est1+ 1.96*_se[1.Treat_Letter] if group1==2

replace est1= `control'+_b[2.Treat_Letter] if group1==3
replace ci_low1= est1- 1.96*_se[2.Treat_Letter] if group1==3
replace ci_high1= est1+ 1.96*_se[2.Treat_Letter] if group1==3

egen ci_high2=rowmax(ci_high ci_high1)
sum ci_high2
local max=r(max)+0.07
format est %6.2f
format est1 %6.2f

* Graphs:

twoway (bar est group if group==1, barwidth(0.8) color(gs14) horizontal ) (bar est group if group==2, barwidth(0.8) color(gs10) horizontal) ///
 (rcap ci_low ci_high group, color(gs0) horizontal ) (scatter group est, msymbol(none) mlabel(est) mlabsize(medium) mlabposition(1)) ///
, scheme( s1mono ) xlabel(0(0.2)`max') legend(off)  ///
		 graphregion(color(white)) ylabel(1 "Control Mean" 2 "Any Treatment" , ang(360) labsize(medium) ) ytitle("") xtitle("")
		 
graph export "$out/graph_any_effect_`y'_nw.pdf", replace

twoway (bar est1 group1 if group1==1, barwidth(0.8) color(gs14) horizontal ) (bar est1 group1 if group1==2, barwidth(0.8) color(gs10) horizontal) ///
 (bar est1 group1 if group1==3, barwidth(0.8) color(gs8) horizontal) (rcap ci_low1 ci_high1 group1, color(gs0) horizontal ) ///
 (scatter group1 est1, msymbol(none) mlabel(est1) mlabsize(medium) mlabposition(1)) ///
, scheme( s1mono ) xlabel(0(0.2)`max') legend(off)  ///
		 graphregion(color(white)) ylabel(1 "Control Mean"  2 "No Letter - Any Ad" 3 "Letter - Any Ad" , ang(360) labsize(medium) ) ytitle("") xtitle("")
		 
graph export "$out/graph_letter_effect_`y'_nw.pdf", replace
drop  group* est* ci_* 
}


/*===============================================================================================
=================================================================================================
								Figure 5: Impacts on Media-based Irregularities
=================================================================================================
===============================================================================================*/

use "$final_data/main_data_municipal_level.dta", clear 


global outcomes "d_media_irreg"

local i=0

foreach y of global outcomes{


quietly: rlasso `y'  $controls , cluster(ID)
local ySel `e(selected)'

quietly: rlasso T_FB  $controls , cluster(ID)
local xSel0 `e(selected)' 

quietly: rlasso TT_FB  $controls , cluster(ID)
local xSel1 `e(selected)' 

quietly: rlasso T_FB_call  $controls , cluster(ID)
local xSel2 `e(selected)' 

quietly: rlasso T_FB_info  $controls , cluster(ID)
local xSel3 `e(selected)' 

quietly: rlasso T_FB_both  $controls , cluster(ID)
local xSel4 `e(selected)' 

quietly: rlasso T_Letter  $controls , cluster(ID)
local xSel5 `e(selected)'

quietly: rlasso T_Letter_no_sj  $controls , cluster(ID)
local xSel6 `e(selected)' 

quietly: rlasso T_Letter_sj  $controls , cluster(ID)
local xSel7 `e(selected)' 


local vSel : list ySel | xSel0
local vSel : list vSel | xSel1
local vSel : list vSel | xSel2
local vSel : list vSel | xSel3
local vSel : list vSel | xSel4
local vSel : list vSel | xSel5
local vSel : list vSel | xSel6
local vSel : list vSel | xSel7


* Any Treat:

matrix pexact=J(1,3,.) 

gen group=1
replace group=2 if _n==1

sum `y' if T_FB==0
local control=`r(mean)'

gen est= `control' if group==1

reg `y' T_FB  `vSel' , cluster(ID)

replace est= `control'+_b[T_FB] if group==2
gen ci_low= est- 1.96*_se[T_FB] if group==2
gen ci_high= est+ 1.96*_se[T_FB] if group==2



* Letters:

gen group1=1
replace group1=2 if _n==1
replace group1=3 if _n==2

gen est1= `control' if group1==1


reg `y' i.Treat_Letter `vSel' , cluster(ID)

replace est1= `control'+_b[1.Treat_Letter] if group1==2
gen ci_low1= est1- 1.96*_se[1.Treat_Letter] if group1==2
gen ci_high1= est1+ 1.96*_se[1.Treat_Letter] if group1==2

replace est1= `control'+_b[2.Treat_Letter] if group1==3
replace ci_low1= est1- 1.96*_se[2.Treat_Letter] if group1==3
replace ci_high1= est1+ 1.96*_se[2.Treat_Letter] if group1==3

egen ci_high2=rowmax(ci_high ci_high1)
sum ci_high2
local max=r(max)+0.01
format est %6.2f
format est1 %6.2f

* Graphs:

twoway (bar est group if group==1, barwidth(0.8) color(gs14) horizontal ) (bar est group if group==2, barwidth(0.8) color(gs10) horizontal) ///
 (rcap ci_low ci_high group, color(gs0) horizontal ) (scatter group est, msymbol(none) mlabel(est) mlabsize(medium) mlabposition(1)) ///
, scheme( s1mono ) xlabel(0(0.05)`max') legend(off)  ///
		 graphregion(color(white)) ylabel(1 "Control Mean" 2 "Any Treatment" , ang(360) labsize(medium) ) ytitle("") xtitle("")
		 
graph export "$out/graph_any_effect_`y'_nw.pdf", replace

twoway (bar est1 group1 if group1==1, barwidth(0.8) color(gs14) horizontal ) (bar est1 group1 if group1==2, barwidth(0.8) color(gs10) horizontal) ///
 (bar est1 group1 if group1==3, barwidth(0.8) color(gs8) horizontal) (rcap ci_low1 ci_high1 group1, color(gs0) horizontal ) ///
 (scatter group1 est1, msymbol(none) mlabel(est1) mlabsize(medium) mlabposition(1)) ///
, scheme( s1mono ) xlabel(0(0.05)`max') legend(off)  ///
		 graphregion(color(white)) ylabel(1 "Control Mean"  2 "No Letter - Any Ad" 3 "Letter - Any Ad" , ang(360) labsize(medium) ) ytitle("") xtitle("")
		 
graph export "$out/graph_letter_effect_`y'_nw.pdf", replace
drop  group* est* ci_* 

}


/*===============================================================================================
=================================================================================================
								Figure 6: Impacts on Deviations from Benford’s 2nd Digit Law
=================================================================================================
===============================================================================================*/

use "$final_data/main_data_municipal_level.dta", clear 

global outcomes "sig95_max_second "

local i=0

foreach y of global outcomes{


quietly: rlasso `y'  $controls , cluster(ID)
local ySel `e(selected)'

quietly: rlasso T_FB  $controls , cluster(ID)
local xSel0 `e(selected)' 

quietly: rlasso TT_FB  $controls , cluster(ID)
local xSel1 `e(selected)' 

quietly: rlasso T_FB_call  $controls , cluster(ID)
local xSel2 `e(selected)' 

quietly: rlasso T_FB_info  $controls , cluster(ID)
local xSel3 `e(selected)' 

quietly: rlasso T_FB_both  $controls , cluster(ID)
local xSel4 `e(selected)' 

quietly: rlasso T_Letter  $controls , cluster(ID)
local xSel5 `e(selected)'

quietly: rlasso T_Letter_no_sj  $controls , cluster(ID)
local xSel6 `e(selected)' 

quietly: rlasso T_Letter_sj  $controls , cluster(ID)
local xSel7 `e(selected)' 


local vSel : list ySel | xSel0
local vSel : list vSel | xSel1
local vSel : list vSel | xSel2
local vSel : list vSel | xSel3
local vSel : list vSel | xSel4
local vSel : list vSel | xSel5
local vSel : list vSel | xSel6
local vSel : list vSel | xSel7


* Any Treat:

matrix pexact=J(1,3,.) 

gen group=1
replace group=2 if _n==1

sum `y' if T_FB==0
local control=`r(mean)'

gen est= `control' if group==1

reg `y' T_FB  `vSel' , cluster(ID)

replace est= `control'+_b[T_FB] if group==2
gen ci_low= est- 1.96*_se[T_FB] if group==2
gen ci_high= est+ 1.96*_se[T_FB] if group==2



* Letters:

gen group1=1
replace group1=2 if _n==1
replace group1=3 if _n==2

gen est1= `control' if group1==1


reg `y' i.Treat_Letter `vSel' , cluster(ID)

replace est1= `control'+_b[1.Treat_Letter] if group1==2
gen ci_low1= est1- 1.96*_se[1.Treat_Letter] if group1==2
gen ci_high1= est1+ 1.96*_se[1.Treat_Letter] if group1==2

replace est1= `control'+_b[2.Treat_Letter] if group1==3
replace ci_low1= est1- 1.96*_se[2.Treat_Letter] if group1==3
replace ci_high1= est1+ 1.96*_se[2.Treat_Letter] if group1==3

egen ci_high2=rowmax(ci_high ci_high1)
sum ci_high2
local max=`control'+0.04
format est %6.2f
format est1 %6.2f

* Graphs:

twoway (bar est group if group==1, barwidth(0.8) color(gs14) horizontal ) (bar est group if group==2, barwidth(0.8) color(gs10) horizontal) ///
 (rcap ci_low ci_high group, color(gs0) horizontal ) (scatter group est, msymbol(none) mlabel(est) mlabsize(medium) mlabposition(1)) ///
, scheme( s1mono ) xlabel(0(0.1)`max') legend(off)  ///
		 graphregion(color(white)) ylabel(1 "Control Mean" 2 "Any Treatment" , ang(360) labsize(medium) ) ytitle("") xtitle("")
		 
graph export "$out/graph_any_effect_`y'_nw.pdf", replace

twoway (bar est1 group1 if group1==1, barwidth(0.8) color(gs14) horizontal ) (bar est1 group1 if group1==2, barwidth(0.8) color(gs10) horizontal) ///
 (bar est1 group1 if group1==3, barwidth(0.8) color(gs8) horizontal) (rcap ci_low1 ci_high1 group1, color(gs0) horizontal ) ///
 (scatter group1 est1, msymbol(none) mlabel(est1) mlabsize(medium) mlabposition(1)) ///
, scheme( s1mono ) xlabel(0(0.1)`max') legend(off)  ///
		 graphregion(color(white)) ylabel(1 "Control Mean"  2 "No Letter - Any Ad" 3 "Letter - Any Ad" , ang(360) labsize(medium) ) ytitle("") xtitle("")
		 
graph export "$out/graph_letter_effect_`y'_nw.pdf", replace
drop  group* est* ci_* 

}



/*===============================================================================================
===============================================================================================
===============================================================================================
									C. Figures in online appendix
===============================================================================================
===============================================================================================
===============================================================================================*/



/*===============================================================================================
=================================================================================================
				Figure A1: Electoral irregularities Reported in 2015 Elections to the MOE
=================================================================================================
===============================================================================================*/

import delim "$raw_data/moe_reportes_alcaldia_2015_short_window.csv", clear encoding("utf-8")

ren codciud ID

ren reportes_ciudadanos reports_any_moe_a2015_window
ren reportes_delito422ydisc39 reports_moe_a2015_p_campaining
ren reportes_delito399yelect19 reports_moe_a2015_reg_fraud
ren reportes_elect13 reports_moe_a2015_ill_advert
ren reportes_delito390 reports_moe_a2015_v_buying
ren reportes_delito387 reports_moe_a2015_intimidation
ren reportes_delito386 reports_moe_a2015_disruption

* Summing all reports in 2015 in same window of time as the dates of the intervention:
gen one=1
collapse (sum) reports_moe_a2015_p_campaining reports_moe_a2015_reg_fraud reports_moe_a2015_ill_advert ///
reports_moe_a2015_v_buying reports_moe_a2015_intimidation reports_moe_a2015_disruption reports_any_moe_a2015_window, by(one)

* Computing number of types of reports as proportion of total reports:
foreach var in reports_moe_a2015_p_campaining reports_moe_a2015_reg_fraud reports_moe_a2015_ill_advert reports_moe_a2015_v_buying reports_moe_a2015_intimidation reports_moe_a2015_disruption {
gen p_`var' = `var'*100/reports_any_moe_a2015_window 
}

graph hbar p_reports_moe_a2015_p_campaining p_reports_moe_a2015_reg_fraud p_reports_moe_a2015_ill_advert p_reports_moe_a2015_v_buying ///
p_reports_moe_a2015_intimidation p_reports_moe_a2015_disruption, scheme(s1mono) ylab(0(5)30, nogrid) blabel(bar, format(%12.2f)) ///
plotregion(style(none)) bargap(80) 	 showyvars legend(off) ytitle("% of total reports") yvaroptions(sort(1) descending ///
relabel(1 "Campaigning by public servants" 2 "Fraud in voter registration" 3 "Illicit advertising" 4 "Vote-buying" 5 ///
"Voter intimidation" 6"Election disruption") label(labsize(medium) angle(0)))  saving("$out/graph_A1.pdf", replace) 
graph export "$out/graph_A1.pdf", replace


/*===============================================================================================
=================================================================================================
								  Figure A4: Covariate Balance
=================================================================================================
===============================================================================================*/

use "$final_data/main_data_municipal_level.dta", clear 

label var  part_c2018 "Turnout 2018 (%)"
label var  pct_blanco_c2018 "Share Blank Votes 2018 (%)"
label var  nbi2005 "Poor 2005 (%)"
label var  sig95_max_second_a2015 "P-val From Forensics <0.05 2015"
label var  pct_Reach "Population Reached by Ads (%)"

global controls_balance "`political'  `violence' `development'  `other' rr1 rr2 rr3 rr4 rr5 rr6 n_respuestas_pre pct_Reach Reach no_Reach"


matrix C=J(1,4,.)

foreach y of global controls_balance{


matrix A=J(1,4,.)


* 1. Control Mean
reg `y' if T_FB==0, r
matrix A[1,1]=_b[_cons]
matrix rownames A= "`: var label `y''"

* 2. Any Treatment vs. Control
reg `y' T_FB  , r
matrix A[1,2]=2*ttail(`e(df_r)', abs(_b[T_FB]/_se[T_FB]))
				

* 3. No Letter - Any Ad vs Control
reg `y' T_FB  if T_Letter==0, r
matrix A[1,3]=2*ttail(`e(df_r)', abs(_b[T_FB]/_se[T_FB]))

* 4. Letter - Any Ad vs Control
reg `y' T_Letter  if TT_FB==0, r
matrix A[1,4]=2*ttail(`e(df_r)', abs(_b[T_Letter]/_se[T_Letter]))		
	
	
matrix C=C\A	
}

matrix C=C[2...,1....]

matrix colnames C= "Control Group Mean" "Any Treatment vs Control" "No Letter - Any Ad vs Control" "Letter - Any Ad vs Control"

putexcel set "$out_balance/balance_pvals.xlsx", replace
putexcel A1=matrix(C), names 

