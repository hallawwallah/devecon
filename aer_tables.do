/*==================================================================================================
Project:       All Eyes on Them: A Field Experiment on Citizen Oversight and Electoral Integrity
Author:        Mateo Montenegro 
Purpose:       This code produces the tables in the main text and the appendix.
----------------------------------------------------------------------------------------------------
Index:		   A. Defining globals and locals
			   B. Tables in main text
			   C. Tables in online appendix
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


local iter=1000 // Defines number of iterations for randomization-inference draws


/*===============================================================================================
===============================================================================================
===============================================================================================
								B. Tables in main text
===============================================================================================
===============================================================================================
===============================================================================================*/

/*===============================================================================================
=================================================================================================
							Table 2: Scale of Ad Campaign
=================================================================================================
===============================================================================================*/

use "$final_data/main_data_municipal_level.dta", clear 

replace Reach=Reach*1000

gen sample=1 if T_FB!=.
keep if sample==1

gen n_mun=sample
gen n_mun_only_info=1 if T_FB_info==1 | T_FB_both==1
gen pop_18older_only_info=pop2018_18older if n_mun_only_info==1

collapse (sum) Reach impressions uniqueoutboundclicks postreactions postshares postcomments pop2018_18older pop_18older_only_info n_mun n_mun_only_info, by(sample)

global metrics1 "Reach impressions"

foreach y of global metrics1{
gen `y'_pm=`y'/n_mun
gen `y'_pc=`y'/pop2018_18older
}

global metrics2 "postreactions postshares postcomments"

foreach y of global metrics2{
gen `y'_pm=`y'/n_mun
gen `y'_pc=`y'*1000/pop2018_18older
}

* Only considering municipalities with information ads for outbound clicks:
gen uniqueoutboundclicks_pm=uniqueoutboundclicks/n_mun_only_info
gen uniqueoutboundclicks_pc=uniqueoutboundclicks*1000/pop_18older_only_info

matrix table2=J(6,3,.) 

local i=0

global metrics "Reach impressions uniqueoutboundclicks postreactions postshares postcomments"


foreach y of global metrics{
local i=`i'+1

sum `y' 
matrix table2[`i',1]=r(mean) 

sum `y'_pm
matrix table2[`i',2]=r(mean) 

sum `y'_pc
matrix table2[`i',3]=r(mean) 
}

matrix rownames table2 = "Viewers of the Ad" "Times the Ad Appeared" "People Clicking" "Users Reacting" "Post Shares" "Comments on Ad"
matrix colnames table2 = "Total" "Per municipality" "Per Population $>$ 18 years"

frmttable using "$out/table2.tex", statmat(table2) sdec(2)  tex  replace fragment

preserve
clear
set obs 1
local tex "$out/table2.tex"

generate strL s = fileread("`tex'") if fileexists("`tex'")
assert filereaderror(s)==0
replace s = subinstr(s,"\begin{center}","",1)   
replace s = subinstr(s,"\begin{tabular}{lccc}","",1)   
replace s = subinstr(s,"\hline \noalign{\smallskip}","",1)   
replace s = subinstr(s,"\noalign{\smallskip}\hline \noalign{\smallskip}","",1) 
replace s = subinstr(s,"\noalign{\smallskip}\hline\end{tabular}\\","",1)   
replace s = subinstr(s,"\end{center}","",1)   
gen byte fw = filewrite("`tex'",s,1)
restore

/*===============================================================================================
=================================================================================================
							Table 3 & Table A24: Impacts on Reports
=================================================================================================
===============================================================================================*/


use "$final_data/main_data_municipal_level.dta", clear 

global outcomes "d_reportMOE_any reportMOE_any d_reportMOE_any_quality reportMOE_any_quality"

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


* Panel A: Any Treatment

matrix pexact=J(1,1,.) 

local i=`i'+1

eststo A`i': reg `y'  T_FB `vSel' , cluster(ID)


ritest T_FB _b[T_FB], strata(strata) reps(`iter') seed($seed) force: reg `y' T_FB `vSel' 

matrix pexact[1,1]=r(p) 

mat colnames pexact = T_FB
est restore A`i'
estadd matrix pexact = pexact
sum `y' if T_FB==0
estadd  scalar c_mean = r(mean)


* Panel B: Different Ads

matrix pexact=J(1,3,.) 

eststo B`i': reg `y' i.Treat_Ads  `vSel' , cluster(ID)

test 1.Treat_Ads=2.Treat_Ads
estadd scalar p_diff_a=r(p) 

test 1.Treat_Ads=3.Treat_Ads
estadd scalar p_diff_b=r(p) 

test 3.Treat_Ads=2.Treat_Ads
estadd scalar p_diff_c=r(p) 

ritest Treat_Ads _b[1.Treat_Ads], strata(strata) reps(`iter') seed($seed) fixlevels(2 3) force: reg `y' i.Treat_Ads `vSel' 
matrix pexact[1,1]=r(p) 

ritest Treat_Ads _b[2.Treat_Ads], strata(strata) reps(`iter') seed($seed) fixlevels(1 3) force: reg `y' i.Treat_Ads `vSel' 
matrix pexact[1,2]=r(p) 

ritest Treat_Ads _b[3.Treat_Ads], strata(strata) reps(`iter') seed($seed) fixlevels(1 2) force: reg `y' i.Treat_Ads `vSel' 
matrix pexact[1,3]=r(p) 


mat colnames pexact = 1.Treat_Ads 2.Treat_Ads 3.Treat_Ads
est restore B`i'
estadd matrix pexact = pexact
sum `y' if T_FB==0
estadd  scalar c_mean = r(mean)


* Panel C: Letter - No Letter

matrix pexact=J(1,2,.) 


eststo C`i': reg `y'  i.Treat_Letter `vSel' , cluster(ID)
test 1.Treat_Letter=2.Treat_Letter
estadd scalar p_diff=r(p) 


ritest Treat_Letter _b[1.Treat_Letter] , strata(strata) reps(`iter') seed($seed) fixlevels(2) force: reg `y'  i.Treat_Letter `vSel' 
matrix pexact[1,1]=r(p) 

ritest Treat_Letter _b[2.Treat_Letter] , strata(strata) reps(`iter') seed($seed) fixlevels(1 ) force : reg `y'  i.Treat_Letter `vSel' 
matrix pexact[1,2]=r(p) 

mat colnames pexact = 1.Treat_Letter 2.Treat_Letter
est restore C`i'
estadd matrix pexact = pexact
sum `y' if T_FB==0
estadd  scalar c_mean = r(mean)


* Table A24: Different Letters

matrix pexact=J(1,3,.) 

eststo D`i': reg `y' i.Treat_Letters  `vSel' , cluster(ID)


test 1.Treat_Letters=2.Treat_Letters
estadd scalar p_diff_1=r(p) 

test 1.Treat_Letters=3.Treat_Letters
estadd scalar p_diff_2=r(p) 

test 3.Treat_Letters=2.Treat_Letters
estadd scalar p_diff_3=r(p) 

ritest Treat_Letters _b[1.Treat_Letters], strata(strata) reps(`iter') seed($seed) fixlevels(2 3) force: reg `y' i.Treat_Letters `vSel' 
matrix pexact[1,1]=r(p) 

ritest Treat_Letters _b[2.Treat_Letters], strata(strata) reps(`iter') seed($seed) fixlevels(1 3) force: reg `y' i.Treat_Letters `vSel' 
matrix pexact[1,2]=r(p) 

ritest Treat_Letters _b[3.Treat_Letters], strata(strata) reps(`iter') seed($seed) fixlevels(1 2) force: reg `y' i.Treat_Letters `vSel' 
matrix pexact[1,3]=r(p) 


mat colnames pexact = 1.Treat_Letters 2.Treat_Letters 3.Treat_Letters
est restore D`i'
estadd matrix pexact = pexact
sum `y' if T_FB==0
estadd  scalar c_mean = r(mean)



}


#d ;
estout A* using "$out/MOEreport_table_A.tex",  replace style(tex)
varlabels(T_FB "$\left[T\right]$ Any treatment",elist(T_FB \midrule))
cells(b(star fmt(%9.3f))  se(par fmt(%9.3f)) pexact(par([ ]) fmt(%9.3f)))  
keep(T_FB) order(T_FB)
collabels(none) stardrop(*) nolabel  mlabels(none);
#d cr

#d ;
estout B* using "$out/MOEreport_table_B.tex",  replace style(tex)
varlabels(1.Treat_Ads "$\left[IA\right]$ Information Ad" 2.Treat_Ads "$\left[CA\right]$ Call-to-Action Ad" 3.Treat_Ads "$\left[I+CA\right]$ Info + Call-to-Action Ad",elist(3.Treat_Ads \midrule))
cells(b(star fmt(%9.3f))  se(par fmt(%9.3f)) pexact(par([ ]) fmt(%9.3f)))  keep(1.Treat_Ads 2.Treat_Ads 3.Treat_Ads) 
order(1.Treat_Ads 2.Treat_Ads 3.Treat_Ads) stats(p_diff_a p_diff_b p_diff_c , fmt(%9.2f %9.2f %9.2f)
labels("Test $ IA=CA$, p-value" "Test $ IA=I+CA$, p-value" "Test $ CA=I+CA$, p-value"))
collabels(none) stardrop(*) nolabel  mlabels(none);
#d cr


#d ;
estout C* using "$out/MOEreport_table_C.tex",  replace style(tex)
varlabels(1.Treat_Letter "$\left[NL\right]$ No Letter - Any Ad" 2.Treat_Letter "$\left[L\right]$ Letter - Any Ad" ,elist(2.Treat_Letter \midrule))
cells(b(star fmt(%9.3f))  se(par fmt(%9.3f)) pexact(par([ ]) fmt(%9.3f)))  keep(1.Treat_Letter 2.Treat_Letter) 
order(1.Treat_Letter 2.Treat_Letter) stats(p_diff j c_mean N, fmt(%9.2f %9.2f %9.2f %9.0f) 
labels("Test $ NL=L$, p-value" "\midrule" "Control Mean" "Sample Size"))
collabels(none) stardrop(*) nolabel  mlabels(none);
#d cr


#d ;
estout D* using "$out/MOEreport_table_D.tex",  replace style(tex)
varlabels(1.Treat_Letters "$\left[NL\right]$ No Letter - Any Ad" 2.Treat_Letters "$\left[FL\right]$ Full Knowledge Letter - Any Ad" 3.Treat_Letters "$\left[PL\right]$ Partial Knowledge Letter - Any Ad" ,elist(2.Treat_Letters \midrule))
cells(b(star fmt(%9.3f))  se(par fmt(%9.3f)) pexact(par([ ]) fmt(%9.3f)))  keep(1.Treat_Letters 2.Treat_Letters 3.Treat_Letters) 
order(1.Treat_Letters 3.Treat_Letters 2.Treat_Letters) stats( p_diff_2 p_diff_1  p_diff_3 j c_mean N , fmt( %9.2f %9.2f %9.2f %9.2f %9.2f %9.0f)
labels( "Test $ NL=PL$, p-value" "Test $ NL=FL$, p-value" "Test $ PL=FL$, p-value" "\midrule" "Control Mean" "Sample Size"))
collabels(none) stardrop(*) nolabel  mlabels(none);
#d cr
est clear

/*===============================================================================================
=================================================================================================
						Table 4 & Table A25: Impacts on Irregularity Measures
=================================================================================================
===============================================================================================*/

use "$final_data/main_data_municipal_level.dta", clear 


global outcomes "d_media_irreg media_irreg z_index_second sig95_max_second"
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


* Panel A: Any Treatment

matrix pexact=J(1,1,.) 

local i=`i'+1

eststo A`i': reg `y'  T_FB `vSel' , cluster(ID)


ritest T_FB _b[T_FB], strata(strata) reps(`iter') seed($seed) force: reg `y' T_FB `vSel' 

matrix pexact[1,1]=r(p) 

mat colnames pexact = T_FB
est restore A`i'
estadd matrix pexact = pexact
sum `y' if T_FB==0
estadd  scalar c_mean = r(mean)


* Panel B: Different Ads

matrix pexact=J(1,3,.) 

eststo B`i': reg `y' i.Treat_Ads  `vSel' , cluster(ID)

test 1.Treat_Ads=2.Treat_Ads
estadd scalar p_diff_a=r(p) 

test 1.Treat_Ads=3.Treat_Ads
estadd scalar p_diff_b=r(p) 

test 3.Treat_Ads=2.Treat_Ads
estadd scalar p_diff_c=r(p) 

ritest Treat_Ads _b[1.Treat_Ads], strata(strata) reps(`iter') seed($seed) fixlevels(2 3) force: reg `y' i.Treat_Ads `vSel' 
matrix pexact[1,1]=r(p) 

ritest Treat_Ads _b[2.Treat_Ads], strata(strata) reps(`iter') seed($seed) fixlevels(1 3) force: reg `y' i.Treat_Ads `vSel' 
matrix pexact[1,2]=r(p) 

ritest Treat_Ads _b[3.Treat_Ads], strata(strata) reps(`iter') seed($seed) fixlevels(1 2) force: reg `y' i.Treat_Ads `vSel' 
matrix pexact[1,3]=r(p) 


mat colnames pexact = 1.Treat_Ads 2.Treat_Ads 3.Treat_Ads
est restore B`i'
estadd matrix pexact = pexact
sum `y' if T_FB==0
estadd  scalar c_mean = r(mean)


* Panel C: Letter - No Letter

matrix pexact=J(1,2,.) 


eststo C`i': reg `y'  i.Treat_Letter `vSel' , cluster(ID)
test 1.Treat_Letter=2.Treat_Letter
estadd scalar p_diff=r(p) 


ritest Treat_Letter _b[1.Treat_Letter] , strata(strata) reps(`iter') seed($seed) fixlevels(2) force: reg `y'  i.Treat_Letter `vSel' 
matrix pexact[1,1]=r(p) 

ritest Treat_Letter _b[2.Treat_Letter] , strata(strata) reps(`iter') seed($seed) fixlevels(1 ) force : reg `y'  i.Treat_Letter `vSel' 
matrix pexact[1,2]=r(p) 

mat colnames pexact = 1.Treat_Letter 2.Treat_Letter
est restore C`i'
estadd matrix pexact = pexact
sum `y' if T_FB==0
estadd  scalar c_mean = r(mean)


* Table A25: Different Letters

matrix pexact=J(1,3,.) 

eststo D`i': reg `y' i.Treat_Letters  `vSel' , cluster(ID)


test 1.Treat_Letters=2.Treat_Letters
estadd scalar p_diff_1=r(p) 

test 1.Treat_Letters=3.Treat_Letters
estadd scalar p_diff_2=r(p) 

test 3.Treat_Letters=2.Treat_Letters
estadd scalar p_diff_3=r(p) 

ritest Treat_Letters _b[1.Treat_Letters], strata(strata) reps(`iter') seed($seed) fixlevels(2 3) force: reg `y' i.Treat_Letters `vSel' 
matrix pexact[1,1]=r(p) 

ritest Treat_Letters _b[2.Treat_Letters], strata(strata) reps(`iter') seed($seed) fixlevels(1 3) force: reg `y' i.Treat_Letters `vSel' 
matrix pexact[1,2]=r(p) 

ritest Treat_Letters _b[3.Treat_Letters], strata(strata) reps(`iter') seed($seed) fixlevels(1 2) force: reg `y' i.Treat_Letters `vSel' 
matrix pexact[1,3]=r(p) 


mat colnames pexact = 1.Treat_Letters 2.Treat_Letters 3.Treat_Letters
est restore D`i'
estadd matrix pexact = pexact
sum `y' if T_FB==0
estadd  scalar c_mean = r(mean)



}



#d ;
estout A* using "$out/irregularities_table_A.tex",  replace style(tex)
varlabels(T_FB "$\left[T\right]$ Any treatment",elist(T_FB \midrule))
cells(b(star fmt(%9.3f))  se(par fmt(%9.3f)) pexact(par([ ]) fmt(%9.3f)))  
keep(T_FB) order(T_FB)
collabels(none) stardrop(*) nolabel  mlabels(none);
#d cr

#d ;
estout B* using "$out/irregularities_table_B.tex",  replace style(tex)
varlabels(1.Treat_Ads "$\left[IA\right]$ Information Ad" 2.Treat_Ads "$\left[CA\right]$ Call-to-Action Ad" 3.Treat_Ads "$\left[I+CA\right]$ Info + Call-to-Action Ad",elist(3.Treat_Ads \midrule))
cells(b(star fmt(%9.3f))  se(par fmt(%9.3f)) pexact(par([ ]) fmt(%9.3f)))  keep(1.Treat_Ads 2.Treat_Ads 3.Treat_Ads) 
order(1.Treat_Ads 2.Treat_Ads 3.Treat_Ads) stats(p_diff_a p_diff_b p_diff_c , fmt(%9.2f %9.2f %9.2f)
labels("Test $ IA=CA$, p-value" "Test $ IA=I+CA$, p-value" "Test $ CA=I+CA$, p-value"))
collabels(none) stardrop(*) nolabel  mlabels(none);
#d cr


#d ;
estout C* using "$out/irregularities_table_C.tex",  replace style(tex)
varlabels(1.Treat_Letter "$\left[NL\right]$ No Letter - Any Ad" 2.Treat_Letter "$\left[L\right]$ Letter - Any Ad" ,elist(2.Treat_Letter \midrule))
cells(b(star fmt(%9.3f))  se(par fmt(%9.3f)) pexact(par([ ]) fmt(%9.3f)))  keep(1.Treat_Letter 2.Treat_Letter) 
order(1.Treat_Letter 2.Treat_Letter) stats(p_diff j c_mean N, fmt(%9.2f %9.2f %9.2f %9.0f) 
labels("Test $ NL=L$, p-value" "\midrule" "Control Mean" "Sample Size"))
collabels(none) stardrop(*) nolabel  mlabels(none);
#d cr


#d ;
estout D* using "$out/irregularities_table_D.tex",  replace style(tex)
varlabels(1.Treat_Letters "$\left[NL\right]$ No Letter - Any Ad" 2.Treat_Letters "$\left[FL\right]$ Full Knowledge Letter - Any Ad" 3.Treat_Letters "$\left[PL\right]$ Partial Knowledge Letter - Any Ad" ,elist(2.Treat_Letters \midrule))
cells(b(star fmt(%9.3f))  se(par fmt(%9.3f)) pexact(par([ ]) fmt(%9.3f)))  keep(1.Treat_Letters 2.Treat_Letters 3.Treat_Letters) 
order(1.Treat_Letters 3.Treat_Letters 2.Treat_Letters) stats( p_diff_2 p_diff_1  p_diff_3 j c_mean N , fmt( %9.2f %9.2f %9.2f %9.2f %9.2f %9.0f)
labels( "Test $ NL=PL$, p-value" "Test $ NL=FL$, p-value" "Test $ PL=FL$, p-value" "\midrule" "Control Mean" "Sample Size"))
collabels(none) stardrop(*) nolabel  mlabels(none);
#d cr
est clear



/*===============================================================================================
=================================================================================================
					Table 5 & Table A26: Impacts on Vote Share of Candidates 
								Likely to Engage in Irregularities
=================================================================================================
===============================================================================================*/

use "$final_data/main_data_candidate_level.dta", clear

global outcomes "pct_vote"
global interactions "p_d_misdeed d_mean_p_d_misdeed above_mean_p_d_misdeed cuestionado"


foreach y of global outcomes{

foreach z of global interactions{

gen zz=`z'


quietly: rlasso `y'  $controls_can , cluster(ID)
local ySel `e(selected)'

quietly: rlasso T_FB  $controls_can , cluster(ID)
local xSel0 `e(selected)' 

quietly: rlasso TT_FB  $controls_can , cluster(ID)
local xSel1 `e(selected)' 

quietly: rlasso T_FB_call  $controls_can , cluster(ID)
local xSel2 `e(selected)' 

quietly: rlasso T_FB_info  $controls_can , cluster(ID)
local xSel3 `e(selected)' 

quietly: rlasso T_FB_both  $controls_can , cluster(ID)
local xSel4 `e(selected)' 

quietly: rlasso T_Letter  $controls_can , cluster(ID)
local xSel5 `e(selected)'

quietly: rlasso T_Letter_no_sj  $controls_can , cluster(ID)
local xSel6 `e(selected)' 

quietly: rlasso T_Letter_sj  $controls_can , cluster(ID)
local xSel7 `e(selected)' 


local vSel : list ySel | xSel0
local vSel : list vSel | xSel1
local vSel : list vSel | xSel2
local vSel : list vSel | xSel3
local vSel : list vSel | xSel4
local vSel : list vSel | xSel5
local vSel : list vSel | xSel6
local vSel : list vSel | xSel7


* Panel A: Any Treatment

matrix pexact=J(1,1,.) 

local i=`i'+1

eststo A`i': reg `y'  i.T_FB##c.zz `vSel' , cluster(ID)
estadd  scalar n_mun = e(N_clust)


ritest T_FB _b[1.T_FB#c.zz], strata(strata) cluster(ID) reps(`iter') seed($seed) force: reg `y' i.T_FB##c.zz `vSel' 

matrix pexact[1,1]=r(p) 

mat colnames pexact = 1.T_FB#c.zz
est restore A`i'
estadd matrix pexact = pexact
sum `y' if T_FB==0
estadd  scalar c_mean = r(mean)


* Panel B: Different Ads

matrix pexact=J(1,3,.) 


eststo B`i': reg `y'  i.Treat_Ads##c.zz  `vSel'  , cluster(ID)
estadd  scalar n_mun = e(N_clust)

test 1.Treat_Ads#c.zz=2.Treat_Ads#c.zz
estadd scalar p_diff_az=r(p) 

test 1.Treat_Ads#c.zz=3.Treat_Ads#c.zz
estadd scalar p_diff_bz=r(p) 

test 3.Treat_Ads#c.zz=2.Treat_Ads#c.zz
estadd scalar p_diff_cz=r(p) 

ritest Treat_Ads _b[1.Treat_Ads#c.zz] , strata(strata) cluster(ID) reps(`iter') seed($seed) fixlevels(2 3) force: reg `y'  i.Treat_Ads##c.zz `vSel' 
matrix pexact[1,1]=r(p) 

ritest Treat_Ads _b[2.Treat_Ads#c.zz] , strata(strata) cluster(ID) reps(`iter') seed($seed) fixlevels(1 3) force : reg `y'  i.Treat_Ads##c.zz `vSel' 
matrix pexact[1,2]=r(p) 

ritest Treat_Ads _b[3.Treat_Ads#c.zz] , strata(strata) cluster(ID) reps(`iter') seed($seed) fixlevels(1 2) force : reg `y'  i.Treat_Ads##c.zz `vSel' 
matrix pexact[1,3]=r(p) 

mat colnames pexact = 1.Treat_Ads#c.zz 2.Treat_Ads#c.zz 3.Treat_Ads#c.zz 
est restore B`i'
estadd matrix pexact = pexact
sum `y' if T_FB==0
estadd  scalar c_mean = r(mean)


* Panel C: Letter - No Letter

matrix pexact=J(1,2,.) 


eststo C`i': reg `y'  i.Treat_Letter##c.zz  `vSel'  , cluster(ID)
estadd  scalar n_mun = e(N_clust)
test 1.Treat_Letter#c.zz =2.Treat_Letter#c.zz 
estadd scalar p_diff1=r(p) 


ritest Treat_Letter _b[1.Treat_Letter#c.zz] , strata(strata) cluster(ID) reps(`iter') seed($seed) fixlevels(2) force: reg `y'  i.Treat_Letter##c.zz `vSel' 
matrix pexact[1,1]=r(p) 

ritest Treat_Letter _b[2.Treat_Letter#c.zz] , strata(strata) cluster(ID) reps(`iter') seed($seed) fixlevels(1) force : reg `y'  i.Treat_Letter##c.zz `vSel' 
matrix pexact[1,2]=r(p) 

mat colnames pexact = 1.Treat_Letter#c.zz 2.Treat_Letter#c.zz 
est restore C`i'
estadd matrix pexact = pexact
sum `y' if T_FB==0
estadd  scalar c_mean = r(mean)


* Table A26: Different Letters

matrix pexact=J(1,3,.) 


eststo D`i': reg `y'  i.Treat_Letters##c.zz  `vSel'  , cluster(ID)
estadd  scalar n_mun = e(N_clust)

test 1.Treat_Letters#c.zz=2.Treat_Letters#c.zz
estadd scalar p_diff_1z=r(p) 

test 1.Treat_Letters#c.zz=3.Treat_Letters#c.zz
estadd scalar p_diff_2z=r(p) 

test 3.Treat_Letters#c.zz=2.Treat_Letters#c.zz
estadd scalar p_diff_3z=r(p) 

ritest Treat_Letters _b[1.Treat_Letters#c.zz] , strata(strata) cluster(ID) reps(`iter') seed($seed) fixlevels(2 3) force: reg `y'  i.Treat_Letters##c.zz `vSel' 
matrix pexact[1,1]=r(p) 

ritest Treat_Letters _b[2.Treat_Letters#c.zz] , strata(strata) cluster(ID) reps(`iter') seed($seed) fixlevels(1 3) force : reg `y'  i.Treat_Letters##c.zz `vSel' 
matrix pexact[1,2]=r(p) 

ritest Treat_Letters _b[3.Treat_Letters#c.zz] , strata(strata) cluster(ID) reps(`iter') seed($seed) fixlevels(1 2) force : reg `y'  i.Treat_Letters##c.zz `vSel' 
matrix pexact[1,3]=r(p) 

mat colnames pexact = 1.Treat_Letters#c.zz 2.Treat_Letters#c.zz 3.Treat_Letters#c.zz 
est restore D`i'
estadd matrix pexact = pexact
sum `y' if T_FB==0
estadd  scalar c_mean = r(mean)

drop *zz*
}
}


#d ;
estout A* using "$out/elections_table_A.tex",  replace style(tex)
varlabels(1.T_FB "$\left[T\right]$ Any treatment" 1.T_FB#c.zz "$\left[T\times Z \right]$ Any treatment $\times Z$",elist(1.T_FB#c.zz \midrule))
cells(b(star fmt(%9.3f))  se(par fmt(%9.3f)) pexact(par([ ]) fmt(%9.3f)))  
keep(1.T_FB#c.zz) order(1.T_FB#c.zz)
collabels(none) stardrop(*) nolabel  mlabels(none);
#d cr

#d ;
estout B* using "$out/elections_table_B.tex",  replace style(tex)
varlabels(1.Treat_Ads "$\left[IA\right]$ Information Ad" 2.Treat_Ads "$\left[CA\right]$ Call-to-Action Ad" 3.Treat_Ads "$\left[I+CA\right]$ Info + Call-to-Action Ad"
1.Treat_Ads#c.zz "$\left[IA\times Z \right]$ Information Ad $\times Z$" 2.Treat_Ads#c.zz "$\left[CA \times Z \right]$ Call-to-Action Ad $\times Z$" 3.Treat_Ads#c.zz "$\left[I+CA \times Z \right]$ Info + Call-to-Action Ad $\times Z$"
,elist(3.Treat_Ads#c.zz \midrule))
cells(b(star fmt(%9.3f))  se(par fmt(%9.3f)) pexact(par([ ]) fmt(%9.3f)))  keep(1.Treat_Ads#c.zz 2.Treat_Ads#c.zz 3.Treat_Ads#c.zz) 
order(1.Treat_Ads#c.zz 2.Treat_Ads#c.zz 3.Treat_Ads#c.zz) stats( p_diff_az p_diff_bz p_diff_cz , fmt( %9.2f %9.2f %9.2f)
labels("Test $ IA \times Z=CA \times Z$, p-value" "Test $ IA \times Z=I+CA \times Z$, p-value" "Test $ CA \times Z=I+CA \times Z$, p-value"))
collabels(none) stardrop(*) nolabel  mlabels(none);
#d cr


#d ;
estout C* using "$out/elections_table_C.tex",  replace style(tex)
varlabels(1.Treat_Letter#c.zz "$\left[NL\times Z \right]$ No Letter - Any Ad $\times Z$" 2.Treat_Letter#c.zz "$\left[L\times Z \right]$ Letter - Any Ad $\times Z$" 
1.Treat_Letter "$\left[NL \right]$ No Letter - Any Ad" 2.Treat_Letter "$\left[L \right]$ Letter - Any Ad " ,elist(2.Treat_Letter#c.zz \midrule))
cells(b(star fmt(%9.3f))  se(par fmt(%9.3f)) pexact(par([ ]) fmt(%9.3f)))  keep( 1.Treat_Letter#c.zz  2.Treat_Letter#c.zz) 
order(1.Treat_Letter 2.Treat_Letter 1.Treat_Letter#c.zz 2.Treat_Letter#c.zz) stats( p_diff1 j c_mean N n_mun , fmt( %9.2f %9.2f %9.2f  %9.0f %9.0f)
labels("Test $ NL \times Z=L \times Z$, p-value" "\midrule" "Control Mean" "Sample Size" "N. Municipalities"))
collabels(none) stardrop(*) nolabel  mlabels(none);
#d cr


#d ;
estout D* using "$out/elections_table_D.tex",  replace style(tex)
varlabels(1.Treat_Letters "$\left[NL\right]$ No Letter - Any Ad" 2.Treat_Letters "$\left[FL\right]$ Full Knowledge Letter - Any Ad" 3.Treat_Letters "$\left[PL\right]$ Partial Knowledge Letter - Any Ad" 
1.Treat_Letters#c.zz "$\left[NL \times Z \right]$ No Letter - Any Ad $\times Z$" 2.Treat_Letters#c.zz "$\left[FL \times Z \right]$ Full Knowledge Letter - Any Ad $\times Z$" 3.Treat_Letters#c.zz "$\left[PL \times Z \right]$ Partial Knowledge Letter - Any Ad $\times Z$" 
,elist(3.Treat_Letters#c.zz \midrule))
cells(b(star fmt(%9.3f))  se(par fmt(%9.3f)) pexact(par([ ]) fmt(%9.3f)))  keep(1.Treat_Letters#c.zz 2.Treat_Letters#c.zz 3.Treat_Letters#c.zz) 
order(1.Treat_Letters#c.zz 2.Treat_Letters#c.zz 3.Treat_Letters#c.zz) stats( p_diff_2z p_diff_1z  p_diff_3z j c_mean N n_mun , fmt( %9.2f %9.2f %9.2f %9.2f  %9.0f %9.0f)
labels("Test $ NL \times Z=PL \times Z$, p-value" "Test $ NL \times Z=FL \times Z$, p-value" "Test $ PL \times Z=FL \times Z$, p-value" "\midrule" "Control Mean" "Sample Size" "N. Municipalities"))
collabels(none) stardrop(*) nolabel  mlabels(none);
#d cr
est clear


/*===============================================================================================
					Bootstrap P-Values for Tables 5 & A26
===============================================================================================*/

if $confidential == 1 {
do "$analysis_code/aer_wildbootstrap.do"
}

/*===============================================================================================
===============================================================================================
===============================================================================================
									C. Tables in online appendix
===============================================================================================
===============================================================================================
===============================================================================================*/

/*===============================================================================================
=================================================================================================
			Table A1: Summary Statistics Comparing Study Sample to Average Municipality
=================================================================================================
===============================================================================================*/

use "$final_data/main_data_municipal_level.dta", clear 

gen sample=1 if T_FB!=.
replace pop2018=pop2018/1000

global vars "pop2018 penetration_fb_pre pib_num_pc2016 indruralildad2017 nbi2005 reports_any_MOE_c2018 reports_any_MOE_a2015"

matrix table_a1=J(7,6,.) 


local i=0
foreach y of global vars{

local i=`i'+1

sum `y' if sample==1
matrix table_a1[`i',1]=r(mean)
matrix table_a1[`i',2]=r(min)
matrix table_a1[`i',3]=r(max)

sum `y' 
matrix table_a1[`i',4]=r(mean)
matrix table_a1[`i',5]=r(min)
matrix table_a1[`i',6]=r(max)    

}

matrix table_a1[2,4]=0.61
matrix table_a1[2,5]=.
matrix table_a1[2,6]=.


matrix rownames table_a1 = "Population 2018" "Facebook Penetration 2018" "Per Capita GDP" "Rural Population 2017" "Poor 2005" "Reports to MOE 2018" "Reports to MOE 2015"
matrix colnames table_a1 = "Mean" "Min" "Max" "Mean" "Min" "Max"

frmttable using "$out/table_a1.tex", statmat(table_a1) sdec(2)  tex  replace fragment

preserve
clear
set obs 1
local tex "$out/table_a1.tex"

generate strL s = fileread("`tex'") if fileexists("`tex'")
assert filereaderror(s)==0
replace s = subinstr(s,"\begin{center}","",1)   
replace s = subinstr(s,"\begin{tabular}{lcccccc}","",1)   
replace s = subinstr(s,"\hline \noalign{\smallskip}","",1)   
replace s = subinstr(s,"\noalign{\smallskip}\hline \noalign{\smallskip}","",1) 
replace s = subinstr(s,"\noalign{\smallskip}\hline\end{tabular}\\","",1)   
replace s = subinstr(s,"\end{center}","",1)   
gen byte fw = filewrite("`tex'",s,1)
restore


/*===============================================================================================
=================================================================================================
							Table A2: Covariate Balance
=================================================================================================
===============================================================================================*/

use "$final_data/main_data_municipal_level.dta", clear 

global controls_balance "`political'  `violence' `development'  `other' rr1 rr2 rr3 rr4 rr5 rr6 n_respuestas_pre pct_Reach Reach no_Reach"

foreach y of global controls_balance{

matrix mean=J(3,1,.)
matrix A=J(1,24,.)

* 1. Control Mean
reg `y' if T_FB==0, r
matrix mean[1,1]=_b[_cons]
matrix rownames A= "`: var label `y''"

* 2. Any Treatment vs. Control
reg `y' T_FB  , r
matrix A[1,1]=_b[T_FB]
matrix A[1,2]=_se[T_FB]
ritest T_FB _b[T_FB], strata(strata) reps(`iter') seed($seed) force: reg `y' T_FB
matrix A[1,3]=r(p) 
			

* 3. Information vs. Control
reg `y' T_FB_info  if T_FB_call==0 & T_FB_both==0, r
matrix A[1,4]=_b[T_FB_info]
matrix A[1,5]=_se[T_FB_info]
ritest T_FB_info _b[T_FB_info], strata(strata) reps(`iter') seed($seed) force: reg `y' T_FB_info  if T_FB_call==0 & T_FB_both==0
matrix A[1,6]=r(p) 	

* 4. Call-to-action vs. Control
reg `y' T_FB_call  if T_FB_info==0 & T_FB_both==0, r
matrix A[1,7]=_b[T_FB_call]
matrix A[1,8]=_se[T_FB_call]
ritest T_FB_call _b[T_FB_call], strata(strata) reps(`iter') seed($seed) force: reg `y' T_FB_call  if T_FB_info==0 & T_FB_both==0
matrix A[1,9]=r(p) 	

* 5. Info + Call-to-action vs. Control
reg `y' T_FB_both  if T_FB_call==0 & T_FB_info==0, r
matrix A[1,10]=_b[T_FB_both]
matrix A[1,11]=_se[T_FB_both]
ritest T_FB_both _b[T_FB_both], strata(strata) reps(`iter') seed($seed) force: reg `y' T_FB_both  if T_FB_call==0 & T_FB_info==0
matrix A[1,12]=r(p) 	
	
* 6. Any Letter vs. No Letter
reg `y' T_Letter  if T_FB==1, r
matrix A[1,13]=_b[T_Letter]
matrix A[1,14]=_se[T_Letter]
ritest T_Letter _b[T_Letter], strata(strata) reps(`iter') seed($seed) force: reg `y'  T_Letter  if T_FB==1
matrix A[1,15]=r(p) 

* 7. Letter P. Knowledge vs. No Letter
reg `y' T_Letter_no_sj  if T_FB==1 & T_Letter_sj==0, r
matrix A[1,16]=_b[T_Letter_no_sj]
matrix A[1,17]=_se[T_Letter_no_sj]
ritest T_Letter_no_sj _b[T_Letter_no_sj], strata(strata) reps(`iter') seed($seed) force: reg `y' T_Letter_no_sj  if T_FB==1 & T_Letter_sj==0
matrix A[1,18]=r(p) 

* 8. Letter F. Knowledge vs. No Letter
reg `y' T_Letter_sj  if T_FB==1 & T_Letter_no_sj==0, r
matrix A[1,19]=_b[T_Letter_sj]
matrix A[1,20]=_se[T_Letter_sj]
ritest T_Letter_sj _b[T_Letter_sj], strata(strata) reps(`iter') seed($seed) force: reg `y' T_Letter_sj  if T_FB==1 & T_Letter_no_sj==0
matrix A[1,21]=r(p) 

* 9. Letter F. Knowledge vs. Letter P. Knowledge
reg `y' T_Letter_sj  if T_Letter==1 , r
matrix A[1,22]=_b[T_Letter_sj]
matrix A[1,23]=_se[T_Letter_sj]
ritest T_Letter_sj _b[T_Letter_sj], strata(strata) reps(`iter') seed($seed) force: reg `y' T_Letter_sj  if T_Letter==1 
matrix A[1,24]=r(p) 	

frmttable, statmat(mean) sdec(3) tex  replace store(mean)

frmttable using "$out_balance/balance_`y'.tex", statmat(A) sdec(3) substat(2)  tex  replace merge(mean) fragment


preserve
clear
set obs 1
local tex "$out_balance/balance_`y'.tex"


generate strL s = fileread("`tex'") if fileexists("`tex'")
assert filereaderror(s)==0
replace s = subinstr(s,"\begin{center}","",1)   
replace s = subinstr(s,"\begin{tabular}{lccccccccc}","",1)   
replace s = subinstr(s,"\hline \noalign{\smallskip}","",1)   
replace s = subinstr(s,"\noalign{\smallskip}\hline\end{tabular}\\","",1)   
replace s = subinstr(s,"\end{center}","",1)   
gen byte fw = filewrite("`tex'",s,1)
restore
}




/*===============================================================================================
=================================================================================================
					Table A3: Balance on Pre-Treat Survey Respondent Characteristics
=================================================================================================
===============================================================================================*/

if $confidential == 1 {

use "$intermediate_data/pre_svy_respondent_vars_clean.dta", clear

* Merging with treatment indicators:

merge n:1 ID using "$intermediate_data/treatment_indicators.dta"
drop _m 

label var  female "Female (=1)"
label var  birth_year "Age"
label var  high_school "High School or Less (=1)"
label var  high_school_more "More than High School (=1)"

global controlsdem "female birth_year  high_school high_school_more"


foreach y of global controlsdem{

matrix mean=J(3,1,.)
matrix A=J(1,24,.)

* 1. Control Mean
reg `y' if T_FB==0, r
matrix mean[1,1]=_b[_cons]
matrix rownames A= "`: var label `y''"

* 2. Any Treatment vs. Control
reg `y' T_FB  , r
matrix A[1,1]=_b[T_FB]
matrix A[1,2]=_se[T_FB]
ritest T_FB _b[T_FB], strata(strata) reps(`iter') seed($seed) force: reg `y' T_FB
matrix A[1,3]=r(p) 
			

* 3. Information vs. Control
reg `y' T_FB_info  if T_FB_call==0 & T_FB_both==0, r
matrix A[1,4]=_b[T_FB_info]
matrix A[1,5]=_se[T_FB_info]
ritest T_FB_info _b[T_FB_info], strata(strata) reps(`iter') seed($seed) force: reg `y' T_FB_info  if T_FB_call==0 & T_FB_both==0
matrix A[1,6]=r(p) 	

* 4. Call-to-action vs. Control
reg `y' T_FB_call  if T_FB_info==0 & T_FB_both==0, r
matrix A[1,7]=_b[T_FB_call]
matrix A[1,8]=_se[T_FB_call]
ritest T_FB_call _b[T_FB_call], strata(strata) reps(`iter') seed($seed) force: reg `y' T_FB_call  if T_FB_info==0 & T_FB_both==0
matrix A[1,9]=r(p) 	

* 5. Info + Call-to-action vs. Control
reg `y' T_FB_both  if T_FB_call==0 & T_FB_info==0, r
matrix A[1,10]=_b[T_FB_both]
matrix A[1,11]=_se[T_FB_both]
ritest T_FB_both _b[T_FB_both], strata(strata) reps(`iter') seed($seed) force: reg `y' T_FB_both  if T_FB_call==0 & T_FB_info==0
matrix A[1,12]=r(p) 	
	
* 6. Any Letter vs. No Letter
reg `y' T_Letter  if T_FB==1, r
matrix A[1,13]=_b[T_Letter]
matrix A[1,14]=_se[T_Letter]
ritest T_Letter _b[T_Letter], strata(strata) reps(`iter') seed($seed) force: reg `y'  T_Letter  if T_FB==1
matrix A[1,15]=r(p) 

* 7. Letter P. Knowledge vs. No Letter
reg `y' T_Letter_no_sj  if T_FB==1 & T_Letter_sj==0, r
matrix A[1,16]=_b[T_Letter_no_sj]
matrix A[1,17]=_se[T_Letter_no_sj]
ritest T_Letter_no_sj _b[T_Letter_no_sj], strata(strata) reps(`iter') seed($seed) force: reg `y' T_Letter_no_sj  if T_FB==1 & T_Letter_sj==0
matrix A[1,18]=r(p) 

* 8. Letter F. Knowledge vs. No Letter
reg `y' T_Letter_sj  if T_FB==1 & T_Letter_no_sj==0, r
matrix A[1,19]=_b[T_Letter_sj]
matrix A[1,20]=_se[T_Letter_sj]
ritest T_Letter_sj _b[T_Letter_sj], strata(strata) reps(`iter') seed($seed) force: reg `y' T_Letter_sj  if T_FB==1 & T_Letter_no_sj==0
matrix A[1,21]=r(p) 

* 9. Letter F. Knowledge vs. Letter P. Knowledge
reg `y' T_Letter_sj  if T_Letter==1 , r
matrix A[1,22]=_b[T_Letter_sj]
matrix A[1,23]=_se[T_Letter_sj]
ritest T_Letter_sj _b[T_Letter_sj], strata(strata) reps(`iter') seed($seed) force: reg `y' T_Letter_sj  if T_Letter==1 
matrix A[1,24]=r(p) 	

frmttable, statmat(mean) sdec(3) tex  replace store(mean)

frmttable using "$out_balance/balance_svy_`y'.tex", statmat(A) sdec(3) substat(2)  tex  replace merge(mean) fragment


preserve
clear
set obs 1
local tex "$out_balance/balance_svy_`y'.tex"


generate strL s = fileread("`tex'") if fileexists("`tex'")
assert filereaderror(s)==0
replace s = subinstr(s,"\begin{center}","",1)   
replace s = subinstr(s,"\begin{tabular}{lccccccccc}","",1)   
replace s = subinstr(s,"\hline \noalign{\smallskip}","",1)   
replace s = subinstr(s,"\noalign{\smallskip}\hline\end{tabular}\\","",1)   
replace s = subinstr(s,"\end{center}","",1)   
gen byte fw = filewrite("`tex'",s,1)
restore
}
}
 
/*===============================================================================================
=================================================================================================
					Table A4: Impacts on Reports After the Intervention
=================================================================================================
===============================================================================================*/

use "$final_data/main_data_municipal_level.dta", clear 


global outcomes "d_reportMOE_late_any reportMOE_late_any d_reportMOE_late_any_quality reportMOE_late_any_quality"
local i=0

foreach y of global outcomes{


quietly: rlasso `y'  $controls  , cluster(ID)
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


* Panel A: Any Treatment

matrix pexact=J(1,1,.) 

local i=`i'+1

eststo A`i': reg `y'  T_FB `vSel' , cluster(ID)


ritest T_FB _b[T_FB], strata(strata) reps(`iter') seed($seed) force: reg `y' T_FB `vSel' 

matrix pexact[1,1]=r(p) 

mat colnames pexact = T_FB
est restore A`i'
estadd matrix pexact = pexact
sum `y' if T_FB==0
estadd  scalar c_mean = r(mean)


* Panel B: Different Ads

matrix pexact=J(1,3,.) 

eststo B`i': reg `y' i.Treat_Ads  `vSel' , cluster(ID)

test 1.Treat_Ads=2.Treat_Ads
estadd scalar p_diff_a=r(p) 

test 1.Treat_Ads=3.Treat_Ads
estadd scalar p_diff_b=r(p) 

test 3.Treat_Ads=2.Treat_Ads
estadd scalar p_diff_c=r(p) 

ritest Treat_Ads _b[1.Treat_Ads], strata(strata) reps(`iter') seed($seed) fixlevels(2 3) force: reg `y' i.Treat_Ads `vSel' 
matrix pexact[1,1]=r(p) 

ritest Treat_Ads _b[2.Treat_Ads], strata(strata) reps(`iter') seed($seed) fixlevels(1 3) force: reg `y' i.Treat_Ads `vSel' 
matrix pexact[1,2]=r(p) 

ritest Treat_Ads _b[3.Treat_Ads], strata(strata) reps(`iter') seed($seed) fixlevels(1 2) force: reg `y' i.Treat_Ads `vSel' 
matrix pexact[1,3]=r(p) 


mat colnames pexact = 1.Treat_Ads 2.Treat_Ads 3.Treat_Ads
est restore B`i'
estadd matrix pexact = pexact
sum `y' if T_FB==0
estadd  scalar c_mean = r(mean)


* Panel C: Letter - No Letter

matrix pexact=J(1,2,.) 


eststo C`i': reg `y'  i.Treat_Letter `vSel' , cluster(ID)
test 1.Treat_Letter=2.Treat_Letter
estadd scalar p_diff=r(p) 


ritest Treat_Letter _b[1.Treat_Letter] , strata(strata) reps(`iter') seed($seed) fixlevels(2) force: reg `y'  i.Treat_Letter `vSel' 
matrix pexact[1,1]=r(p) 

ritest Treat_Letter _b[2.Treat_Letter] , strata(strata) reps(`iter') seed($seed) fixlevels(1 ) force : reg `y'  i.Treat_Letter `vSel' 
matrix pexact[1,2]=r(p) 

mat colnames pexact = 1.Treat_Letter 2.Treat_Letter
est restore C`i'
estadd matrix pexact = pexact
sum `y' if T_FB==0
estadd  scalar c_mean = r(mean)

}

#d ;
estout A* using "$out/lateMOEreports_table_A.tex",  replace style(tex)
varlabels(T_FB "$\left[T\right]$ Any treatment",elist(T_FB \midrule))
cells(b(star fmt(%9.3f))  se(par fmt(%9.3f)) pexact(par([ ]) fmt(%9.3f)))  
keep(T_FB) order(T_FB)
collabels(none) stardrop(*) nolabel  mlabels(none);
#d cr

#d ;
estout B* using "$out/lateMOEreports_table_B.tex",  replace style(tex)
varlabels(1.Treat_Ads "$\left[IA\right]$ Information Ad" 2.Treat_Ads "$\left[CA\right]$ Call-to-Action Ad" 3.Treat_Ads "$\left[I+CA\right]$ Info + Call-to-Action Ad",elist(3.Treat_Ads \midrule))
cells(b(star fmt(%9.3f))  se(par fmt(%9.3f)) pexact(par([ ]) fmt(%9.3f)))  keep(1.Treat_Ads 2.Treat_Ads 3.Treat_Ads) 
order(1.Treat_Ads 2.Treat_Ads 3.Treat_Ads) stats(p_diff_a p_diff_b p_diff_c , fmt(%9.2f %9.2f %9.2f)
labels("Test $ IA=CA$, p-value" "Test $ IA=I+CA$, p-value" "Test $ CA=I+CA$, p-value"))
collabels(none) stardrop(*) nolabel  mlabels(none);
#d cr


#d ;
estout C* using "$out/lateMOEreports_table_C.tex",  replace style(tex)
varlabels(1.Treat_Letter "$\left[NL\right]$ No Letter - Any Ad" 2.Treat_Letter "$\left[L\right]$ Letter - Any Ad" ,elist(2.Treat_Letter \midrule))
cells(b(star fmt(%9.3f))  se(par fmt(%9.3f)) pexact(par([ ]) fmt(%9.3f)))  keep(1.Treat_Letter 2.Treat_Letter) 
order(1.Treat_Letter 2.Treat_Letter) stats(p_diff j c_mean N, fmt(%9.2f %9.2f %9.2f %9.0f) 
labels("Test $ NL=L$, p-value" "\midrule" "Control Mean" "Sample Size"))
collabels(none) stardrop(*) nolabel  mlabels(none);
#d cr

est clear

/*===============================================================================================
=================================================================================================
				Table A5: Impacts on the Media-Based Irregularity Measures:
						Including News Coming From MOE Reports
=================================================================================================
===============================================================================================*/

use "$final_data/main_data_municipal_level.dta", clear 

global outcomes "d_media_irreg_inclMOE media_irreg_inclMOE"

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


* Panel A: Any Treatment

matrix pexact=J(1,1,.) 

local i=`i'+1

eststo A`i': reg `y'  T_FB `vSel' , cluster(ID)


ritest T_FB _b[T_FB], strata(strata) reps(`iter') seed($seed) force: reg `y' T_FB `vSel' 

matrix pexact[1,1]=r(p) 

mat colnames pexact = T_FB
est restore A`i'
estadd matrix pexact = pexact
sum `y' if T_FB==0
estadd  scalar c_mean = r(mean)


* Panel B: Different Ads

matrix pexact=J(1,3,.) 

eststo B`i': reg `y' i.Treat_Ads  `vSel' , cluster(ID)

test 1.Treat_Ads=2.Treat_Ads
estadd scalar p_diff_a=r(p) 

test 1.Treat_Ads=3.Treat_Ads
estadd scalar p_diff_b=r(p) 

test 3.Treat_Ads=2.Treat_Ads
estadd scalar p_diff_c=r(p) 

ritest Treat_Ads _b[1.Treat_Ads], strata(strata) reps(`iter') seed($seed) fixlevels(2 3) force: reg `y' i.Treat_Ads `vSel' 
matrix pexact[1,1]=r(p) 

ritest Treat_Ads _b[2.Treat_Ads], strata(strata) reps(`iter') seed($seed) fixlevels(1 3) force: reg `y' i.Treat_Ads `vSel' 
matrix pexact[1,2]=r(p) 

ritest Treat_Ads _b[3.Treat_Ads], strata(strata) reps(`iter') seed($seed) fixlevels(1 2) force: reg `y' i.Treat_Ads `vSel' 
matrix pexact[1,3]=r(p) 


mat colnames pexact = 1.Treat_Ads 2.Treat_Ads 3.Treat_Ads
est restore B`i'
estadd matrix pexact = pexact
sum `y' if T_FB==0
estadd  scalar c_mean = r(mean)


* Panel C: Letter - No Letter

matrix pexact=J(1,2,.) 


eststo C`i': reg `y'  i.Treat_Letter `vSel' , cluster(ID)
test 1.Treat_Letter=2.Treat_Letter
estadd scalar p_diff=r(p) 


ritest Treat_Letter _b[1.Treat_Letter] , strata(strata) reps(`iter') seed($seed) fixlevels(2) force: reg `y'  i.Treat_Letter `vSel' 
matrix pexact[1,1]=r(p) 

ritest Treat_Letter _b[2.Treat_Letter] , strata(strata) reps(`iter') seed($seed) fixlevels(1 ) force : reg `y'  i.Treat_Letter `vSel' 
matrix pexact[1,2]=r(p) 

mat colnames pexact = 1.Treat_Letter 2.Treat_Letter
est restore C`i'
estadd matrix pexact = pexact
sum `y' if T_FB==0
estadd  scalar c_mean = r(mean)


}


#d ;
estout A* using "$out/media_inclMOE_table_A.tex",  replace style(tex)
varlabels(T_FB "$\left[T\right]$ Any treatment",elist(T_FB \midrule))
cells(b(star fmt(%9.3f))  se(par fmt(%9.3f)) pexact(par([ ]) fmt(%9.3f)))  
keep(T_FB) order(T_FB)
collabels(none) stardrop(*) nolabel  mlabels(none);
#d cr

#d ;
estout B* using "$out/media_inclMOE_table_B.tex",  replace style(tex)
varlabels(1.Treat_Ads "$\left[IA\right]$ Information Ad" 2.Treat_Ads "$\left[CA\right]$ Call-to-Action Ad" 3.Treat_Ads "$\left[I+CA\right]$ Info + Call-to-Action Ad",elist(3.Treat_Ads \midrule))
cells(b(star fmt(%9.3f))  se(par fmt(%9.3f)) pexact(par([ ]) fmt(%9.3f)))  keep(1.Treat_Ads 2.Treat_Ads 3.Treat_Ads) 
order(1.Treat_Ads 2.Treat_Ads 3.Treat_Ads) stats(p_diff_a p_diff_b p_diff_c , fmt(%9.2f %9.2f %9.2f)
labels("Test $ IA=CA$, p-value" "Test $ IA=I+CA$, p-value" "Test $ CA=I+CA$, p-value"))
collabels(none) stardrop(*) nolabel  mlabels(none);
#d cr


#d ;
estout C* using "$out/media_inclMOE_table_C.tex",  replace style(tex)
varlabels(1.Treat_Letter "$\left[NL\right]$ No Letter - Any Ad" 2.Treat_Letter "$\left[L\right]$ Letter - Any Ad" ,elist(2.Treat_Letter \midrule))
cells(b(star fmt(%9.3f))  se(par fmt(%9.3f)) pexact(par([ ]) fmt(%9.3f)))  keep(1.Treat_Letter 2.Treat_Letter) 
order(1.Treat_Letter 2.Treat_Letter) stats(p_diff j c_mean N, fmt(%9.2f %9.2f %9.2f %9.0f) 
labels("Test $ NL=L$, p-value" "\midrule" "Control Mean" "Sample Size"))
collabels(none) stardrop(*) nolabel  mlabels(none);
#d cr

est clear




/*===============================================================================================
=================================================================================================
		Table A6: Impacts on Media-Based Irregularity Measures - By Type of Irregularity
=================================================================================================
===============================================================================================*/

use "$final_data/main_data_municipal_level.dta", clear 


global outcomes "d_media_irreg_compra d_media_irreg_disturbio d_media_irreg_cand_int d_media_irreg_intimidacion d_media_irreg_trashumancia d_media_irreg_intervencion d_media_irreg_fraud d_media_irreg_otro"

local i=0

foreach y of global outcomes{


quietly: rlasso `y'  $controls  , cluster(ID)
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


* Panel A: Any Treatment

matrix pexact=J(1,1,.) 

local i=`i'+1

eststo A`i': reg `y'  T_FB `vSel' , cluster(ID)


ritest T_FB _b[T_FB], strata(strata) reps(`iter') seed($seed) force: reg `y' T_FB `vSel' 

matrix pexact[1,1]=r(p) 

mat colnames pexact = T_FB
est restore A`i'
estadd matrix pexact = pexact
sum `y' if T_FB==0
estadd  scalar c_mean = r(mean)


* Panel B: Different Ads

matrix pexact=J(1,3,.) 

eststo B`i': reg `y' i.Treat_Ads  `vSel' , cluster(ID)

test 1.Treat_Ads=2.Treat_Ads
estadd scalar p_diff_a=r(p) 

test 1.Treat_Ads=3.Treat_Ads
estadd scalar p_diff_b=r(p) 

test 3.Treat_Ads=2.Treat_Ads
estadd scalar p_diff_c=r(p) 

ritest Treat_Ads _b[1.Treat_Ads], strata(strata) reps(`iter') seed($seed) fixlevels(2 3) force: reg `y' i.Treat_Ads `vSel' 
matrix pexact[1,1]=r(p) 

ritest Treat_Ads _b[2.Treat_Ads], strata(strata) reps(`iter') seed($seed) fixlevels(1 3) force: reg `y' i.Treat_Ads `vSel' 
matrix pexact[1,2]=r(p) 

ritest Treat_Ads _b[3.Treat_Ads], strata(strata) reps(`iter') seed($seed) fixlevels(1 2) force: reg `y' i.Treat_Ads `vSel' 
matrix pexact[1,3]=r(p) 


mat colnames pexact = 1.Treat_Ads 2.Treat_Ads 3.Treat_Ads
est restore B`i'
estadd matrix pexact = pexact
sum `y' if T_FB==0
estadd  scalar c_mean = r(mean)


* Panel C: Letter - No Letter

matrix pexact=J(1,2,.) 


eststo C`i': reg `y'  i.Treat_Letter `vSel' , cluster(ID)
test 1.Treat_Letter=2.Treat_Letter
estadd scalar p_diff=r(p) 


ritest Treat_Letter _b[1.Treat_Letter] , strata(strata) reps(`iter') seed($seed) fixlevels(2) force: reg `y'  i.Treat_Letter `vSel' 
matrix pexact[1,1]=r(p) 

ritest Treat_Letter _b[2.Treat_Letter] , strata(strata) reps(`iter') seed($seed) fixlevels(1 ) force : reg `y'  i.Treat_Letter `vSel' 
matrix pexact[1,2]=r(p) 

mat colnames pexact = 1.Treat_Letter 2.Treat_Letter
est restore C`i'
estadd matrix pexact = pexact
sum `y' if T_FB==0
estadd  scalar c_mean = r(mean)


}

#d ;
estout A* using "$out/media_bytype_table_A.tex",  replace style(tex)
varlabels(T_FB "$\left[T\right]$ Any treatment",elist(T_FB \midrule))
cells(b(star fmt(%9.3f))  se(par fmt(%9.3f)) pexact(par([ ]) fmt(%9.3f)))  
keep(T_FB) order(T_FB)
collabels(none) stardrop(*) nolabel  mlabels(none);
#d cr

#d ;
estout B* using "$out/media_bytype_table_B.tex",  replace style(tex)
varlabels(1.Treat_Ads "$\left[IA\right]$ Information Ad" 2.Treat_Ads "$\left[CA\right]$ Call-to-Action Ad" 3.Treat_Ads "$\left[I+CA\right]$ Info + Call-to-Action Ad",elist(3.Treat_Ads \midrule))
cells(b(star fmt(%9.3f))  se(par fmt(%9.3f)) pexact(par([ ]) fmt(%9.3f)))  keep(1.Treat_Ads 2.Treat_Ads 3.Treat_Ads) 
order(1.Treat_Ads 2.Treat_Ads 3.Treat_Ads) stats(p_diff_a p_diff_b p_diff_c , fmt(%9.2f %9.2f %9.2f)
labels("Test $ IA=CA$, p-value" "Test $ IA=I+CA$, p-value" "Test $ CA=I+CA$, p-value"))
collabels(none) stardrop(*) nolabel  mlabels(none);
#d cr


#d ;
estout C* using "$out/media_bytype_table_C.tex",  replace style(tex)
varlabels(1.Treat_Letter "$\left[NL\right]$ No Letter - Any Ad" 2.Treat_Letter "$\left[L\right]$ Letter - Any Ad" ,elist(2.Treat_Letter \midrule))
cells(b(star fmt(%9.3f))  se(par fmt(%9.3f)) pexact(par([ ]) fmt(%9.3f)))  keep(1.Treat_Letter 2.Treat_Letter) 
order(1.Treat_Letter 2.Treat_Letter) stats(p_diff j c_mean N, fmt(%9.2f %9.2f %9.2f %9.0f) 
labels("Test $ NL=L$, p-value" "\midrule" "Control Mean" "Sample Size"))
collabels(none) stardrop(*) nolabel  mlabels(none);
#d cr

est clear



/*===============================================================================================
=================================================================================================
				Table A7: Robustness of the Media-Based Irregularity Measures: 
									Leave-one-out Estimates
=================================================================================================
===============================================================================================*/

 
use "$final_data/main_data_municipal_level.dta", clear 

global outcomes "d_media_irreg_sin_compra d_media_irreg_sin_disturbio d_media_irreg_sin_cand_int d_media_irreg_sin_intimidacion d_media_irreg_sin_trashumancia d_media_irreg_sin_intervencion d_media_irreg_sin_fraud  d_media_irreg_sin_otro"

local i=0

foreach y of global outcomes{


quietly: rlasso `y'  $controls  , cluster(ID)
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


* Panel A: Any Treatment

matrix pexact=J(1,1,.) 

local i=`i'+1

eststo A`i': reg `y'  T_FB `vSel' , cluster(ID)


ritest T_FB _b[T_FB], strata(strata) reps(`iter') seed($seed) force: reg `y' T_FB `vSel' 

matrix pexact[1,1]=r(p) 

mat colnames pexact = T_FB
est restore A`i'
estadd matrix pexact = pexact
sum `y' if T_FB==0
estadd  scalar c_mean = r(mean)


* Panel B: Different Ads

matrix pexact=J(1,3,.) 

eststo B`i': reg `y' i.Treat_Ads  `vSel' , cluster(ID)

test 1.Treat_Ads=2.Treat_Ads
estadd scalar p_diff_a=r(p) 

test 1.Treat_Ads=3.Treat_Ads
estadd scalar p_diff_b=r(p) 

test 3.Treat_Ads=2.Treat_Ads
estadd scalar p_diff_c=r(p) 

ritest Treat_Ads _b[1.Treat_Ads], strata(strata) reps(`iter') seed($seed) fixlevels(2 3) force: reg `y' i.Treat_Ads `vSel' 
matrix pexact[1,1]=r(p) 

ritest Treat_Ads _b[2.Treat_Ads], strata(strata) reps(`iter') seed($seed) fixlevels(1 3) force: reg `y' i.Treat_Ads `vSel' 
matrix pexact[1,2]=r(p) 

ritest Treat_Ads _b[3.Treat_Ads], strata(strata) reps(`iter') seed($seed) fixlevels(1 2) force: reg `y' i.Treat_Ads `vSel' 
matrix pexact[1,3]=r(p) 


mat colnames pexact = 1.Treat_Ads 2.Treat_Ads 3.Treat_Ads
est restore B`i'
estadd matrix pexact = pexact
sum `y' if T_FB==0
estadd  scalar c_mean = r(mean)


* Panel C: Letter - No Letter

matrix pexact=J(1,2,.) 


eststo C`i': reg `y'  i.Treat_Letter `vSel' , cluster(ID)
test 1.Treat_Letter=2.Treat_Letter
estadd scalar p_diff=r(p) 


ritest Treat_Letter _b[1.Treat_Letter] , strata(strata) reps(`iter') seed($seed) fixlevels(2) force: reg `y'  i.Treat_Letter `vSel' 
matrix pexact[1,1]=r(p) 

ritest Treat_Letter _b[2.Treat_Letter] , strata(strata) reps(`iter') seed($seed) fixlevels(1 ) force : reg `y'  i.Treat_Letter `vSel' 
matrix pexact[1,2]=r(p) 

mat colnames pexact = 1.Treat_Letter 2.Treat_Letter
est restore C`i'
estadd matrix pexact = pexact
sum `y' if T_FB==0
estadd  scalar c_mean = r(mean)


}

#d ;
estout A* using "$out/media_app_table_A.tex",  replace style(tex)
varlabels(T_FB "$\left[T\right]$ Any treatment",elist(T_FB \midrule))
cells(b(star fmt(%9.3f))  se(par fmt(%9.3f)) pexact(par([ ]) fmt(%9.3f)))  
keep(T_FB) order(T_FB)
collabels(none) stardrop(*) nolabel  mlabels(none);
#d cr


#d ;
estout B* using "$out/media_app_table_B.tex",  replace style(tex)
varlabels(1.Treat_Ads "$\left[IA\right]$ Information Ad" 2.Treat_Ads "$\left[CA\right]$ Call-to-Action Ad" 3.Treat_Ads "$\left[I+CA\right]$ Info + Call-to-Action Ad",elist(3.Treat_Ads \midrule))
cells(b(star fmt(%9.3f))  se(par fmt(%9.3f)) pexact(par([ ]) fmt(%9.3f)))  keep(1.Treat_Ads 2.Treat_Ads 3.Treat_Ads) 
order(1.Treat_Ads 2.Treat_Ads 3.Treat_Ads) stats(p_diff_a p_diff_b p_diff_c , fmt(%9.2f %9.2f %9.2f)
labels("Test $ IA=CA$, p-value" "Test $ IA=I+CA$, p-value" "Test $ CA=I+CA$, p-value"))
collabels(none) stardrop(*) nolabel  mlabels(none);
#d cr



#d ;
estout C* using "$out/media_app_table_C.tex",  replace style(tex)
varlabels(1.Treat_Letter "$\left[NL\right]$ No Letter - Any Ad" 2.Treat_Letter "$\left[L\right]$ Letter - Any Ad" ,elist(2.Treat_Letter \midrule))
cells(b(star fmt(%9.3f))  se(par fmt(%9.3f)) pexact(par([ ]) fmt(%9.3f)))  keep(1.Treat_Letter 2.Treat_Letter) 
order(1.Treat_Letter 2.Treat_Letter) stats(p_diff j c_mean N, fmt(%9.2f %9.2f %9.2f %9.0f) 
labels("Test $ NL=L$, p-value" "\midrule" "Control Mean" "Sample Size"))
collabels(none) stardrop(*) nolabel  mlabels(none);
#d cr

est clear


/*===============================================================================================
=================================================================================================
	Table A8: Correlation Between Forensic and Media-Based Electoral Irregularity Measures
=================================================================================================
===============================================================================================*/

use "$final_data/main_data_municipal_level.dta", clear 
 

global outcomes "z_index_second sig95_max_second "


sum d_media_irreg
gen z_d_media_irreg=(d_media_irreg-`r(mean)')/`r(sd)'

sum media_irreg
gen z_media_irreg=(d_media_irreg-`r(mean)')/`r(sd)'

local i=0
foreach y of global outcomes{
local i=`i'+1
sum `y'
gen z_`y'=(`y'-`r(mean)')/`r(sd)'

eststo A`i': reg  z_`y' z_d_media_irreg if T_FB==0 , r

local i=`i'+1
eststo A`i': reg z_`y' z_media_irreg if T_FB==0 , r


}


#d ;
estout A* using "$out/correlation_media_forensics_table.tex",  replace style(tex)
varlabels( z_d_media_irreg "Media Irregularities (=1)" z_media_irreg "Number of Media Irregularities" , elist(z_media_irreg \midrule))
cells(b(star fmt(%9.3f))  se(par fmt(%9.3f)) pexact(par([ ]) fmt(%9.3f)))  keep(z_d_media_irreg z_media_irreg) 
order(z_d_media_irreg z_media_irreg) stats( N , fmt( %9.0f)
labels( "Sample Size"))
collabels(none) stardrop(*) nolabel  mlabels(none);
#d cr
est clear


/*===============================================================================================
=================================================================================================
 Table A9: Correlation Between Forensic and Media-Based Electoral Irregularity Measures by Type
=================================================================================================
===============================================================================================*/

use "$final_data/main_data_municipal_level.dta", clear 
 

global outcomes "d_media_irreg_compra d_media_irreg_disturbio d_media_irreg_cand_int d_media_irreg_intimidacion d_media_irreg_trashumancia d_media_irreg_intervencion d_media_irreg_fraud d_media_irreg_otro"


sum sig95_max_second
gen z_sig95_max_second=(sig95_max_second-`r(mean)')/`r(sd)'

local i=0
foreach y of global outcomes{

sum `y'
gen z_`y'=(`y'-`r(mean)')/`r(sd)'

local j=1
local i=`i'+1
eststo A`i'`j': reg  z_index_second z_`y' if T_FB==0 , r
local j=2
eststo A`i'`j': reg  z_sig95_max_second z_`y' if T_FB==0 , r
}



#d ;
estout A1* using "$out/correlation_media_forensics_bytype_tableA.tex",  replace style(tex)
varlabels( z_d_media_irreg_compra "Vote Buying in Media (=1) (z-score)")
cells(b(star fmt(%9.3f))  se(par fmt(%9.3f)) pexact(par([ ]) fmt(%9.3f)))  keep(z_d_media_irreg_compra) 
collabels(none) stardrop(*) nolabel  mlabels(none);
#d cr

#d ;
estout A2* using "$out/correlation_media_forensics_bytype_tableB.tex",  replace style(tex)
varlabels( z_d_media_irreg_disturbio "Riot in Media (=1) (z-score)")
cells(b(star fmt(%9.3f))  se(par fmt(%9.3f)) pexact(par([ ]) fmt(%9.3f)))  keep(z_d_media_irreg_disturbio) 
collabels(none) stardrop(*) nolabel  mlabels(none);
#d cr

#d ;
estout A3* using "$out/correlation_media_forensics_bytype_tableC.tex",  replace style(tex)
varlabels( z_d_media_irreg_cand_int "Candidate Intimidation in Media (=1) (z-score)")
cells(b(star fmt(%9.3f))  se(par fmt(%9.3f)) pexact(par([ ]) fmt(%9.3f)))  keep(z_d_media_irreg_cand_int) 
collabels(none) stardrop(*) nolabel  mlabels(none);
#d cr

#d ;
estout A4* using "$out/correlation_media_forensics_bytype_tableD.tex",  replace style(tex)
varlabels( z_d_media_irreg_intimidacion "Voter Intimidation in Media (=1) (z-score)")
cells(b(star fmt(%9.3f))  se(par fmt(%9.3f)) pexact(par([ ]) fmt(%9.3f)))  keep(z_d_media_irreg_intimidacion) 
collabels(none) stardrop(*) nolabel  mlabels(none);
#d cr

#d ;
estout A5* using "$out/correlation_media_forensics_bytype_tableE.tex",  replace style(tex)
varlabels( z_d_media_irreg_trashumancia "Registration Fraud in Media (=1) (z-score)")
cells(b(star fmt(%9.3f))  se(par fmt(%9.3f)) pexact(par([ ]) fmt(%9.3f)))  keep(z_d_media_irreg_trashumancia) 
collabels(none) stardrop(*) nolabel  mlabels(none);
#d cr

#d ;
estout A6* using "$out/correlation_media_forensics_bytype_tableF.tex",  replace style(tex)
varlabels( z_d_media_irreg_intervencion "Public Servant Campaigning in Media (=1) (z-score)")
cells(b(star fmt(%9.3f))  se(par fmt(%9.3f)) pexact(par([ ]) fmt(%9.3f)))  keep(z_d_media_irreg_intervencion) 
collabels(none) stardrop(*) nolabel  mlabels(none);
#d cr

#d ;
estout A7* using "$out/correlation_media_forensics_bytype_tableG.tex",  replace style(tex)
varlabels( z_d_media_irreg_fraud "Electoral Fraud in Media (=1) (z-score)" , elist(z_d_media_irreg_fraud \midrule))
cells(b(star fmt(%9.3f))  se(par fmt(%9.3f)) pexact(par([ ]) fmt(%9.3f)))  keep(z_d_media_irreg_fraud) 
stats( N , fmt( %9.0f) labels( "Sample Size"))
collabels(none) stardrop(*) nolabel  mlabels(none);
#d cr
est clear


/*===============================================================================================
=================================================================================================
          Table A10: Impacts on Deviations from Benford’s Second Digit Law - By Test
=================================================================================================
===============================================================================================*/

use "$final_data/main_data_municipal_level.dta", clear 

global outcomes "z_chi2_second sig95_chi2_second z_kolmo_second sig95_kolmo_second z_kuiper_second sig95_kuiper_second"


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


* Panel A: Any Treatment

matrix pexact=J(1,1,.) 

local i=`i'+1

eststo A`i': reg `y'  T_FB `vSel' , cluster(ID)


ritest T_FB _b[T_FB], strata(strata) reps(`iter') seed($seed) force: reg `y' T_FB `vSel' 

matrix pexact[1,1]=r(p) 

mat colnames pexact = T_FB
est restore A`i'
estadd matrix pexact = pexact
sum `y' if T_FB==0
estadd  scalar c_mean = r(mean)


* Panel B: Different Ads

matrix pexact=J(1,3,.) 

eststo B`i': reg `y' i.Treat_Ads  `vSel' , cluster(ID)

test 1.Treat_Ads=2.Treat_Ads
estadd scalar p_diff_a=r(p) 

test 1.Treat_Ads=3.Treat_Ads
estadd scalar p_diff_b=r(p) 

test 3.Treat_Ads=2.Treat_Ads
estadd scalar p_diff_c=r(p) 

ritest Treat_Ads _b[1.Treat_Ads], strata(strata) reps(`iter') seed($seed) fixlevels(2 3) force: reg `y' i.Treat_Ads `vSel' 
matrix pexact[1,1]=r(p) 

ritest Treat_Ads _b[2.Treat_Ads], strata(strata) reps(`iter') seed($seed) fixlevels(1 3) force: reg `y' i.Treat_Ads `vSel' 
matrix pexact[1,2]=r(p) 

ritest Treat_Ads _b[3.Treat_Ads], strata(strata) reps(`iter') seed($seed) fixlevels(1 2) force: reg `y' i.Treat_Ads `vSel' 
matrix pexact[1,3]=r(p) 


mat colnames pexact = 1.Treat_Ads 2.Treat_Ads 3.Treat_Ads
est restore B`i'
estadd matrix pexact = pexact
sum `y' if T_FB==0
estadd  scalar c_mean = r(mean)


* Panel C: Letter - No Letter

matrix pexact=J(1,2,.) 


eststo C`i': reg `y'  i.Treat_Letter `vSel' , cluster(ID)
test 1.Treat_Letter=2.Treat_Letter
estadd scalar p_diff=r(p) 


ritest Treat_Letter _b[1.Treat_Letter] , strata(strata) reps(`iter') seed($seed) fixlevels(2) force: reg `y'  i.Treat_Letter `vSel' 
matrix pexact[1,1]=r(p) 

ritest Treat_Letter _b[2.Treat_Letter] , strata(strata) reps(`iter') seed($seed) fixlevels(1 ) force : reg `y'  i.Treat_Letter `vSel' 
matrix pexact[1,2]=r(p) 

mat colnames pexact = 1.Treat_Letter 2.Treat_Letter
est restore C`i'
estadd matrix pexact = pexact
sum `y' if T_FB==0
estadd  scalar c_mean = r(mean)

}


#d ;
estout A* using "$out/forensics_app_table_A.tex",  replace style(tex)
varlabels(T_FB "$\left[T\right]$ Any treatment",elist(T_FB \midrule))
cells(b(star fmt(%9.3f))  se(par fmt(%9.3f)) pexact(par([ ]) fmt(%9.3f)))  
keep(T_FB) order(T_FB)
collabels(none) stardrop(*) nolabel  mlabels(none);
#d cr

#d ;
estout B* using "$out/forensics_app_table_B.tex",  replace style(tex)
varlabels(1.Treat_Ads "$\left[IA\right]$ Information Ad" 2.Treat_Ads "$\left[CA\right]$ Call-to-Action Ad" 3.Treat_Ads "$\left[I+CA\right]$ Info + Call-to-Action Ad",elist(3.Treat_Ads \midrule))
cells(b(star fmt(%9.3f))  se(par fmt(%9.3f)) pexact(par([ ]) fmt(%9.3f)))  keep(1.Treat_Ads 2.Treat_Ads 3.Treat_Ads) 
order(1.Treat_Ads 2.Treat_Ads 3.Treat_Ads) stats(p_diff_a p_diff_b p_diff_c , fmt(%9.2f %9.2f %9.2f)
labels("Test $ IA=CA$, p-value" "Test $ IA=I+CA$, p-value" "Test $ CA=I+CA$, p-value"))
collabels(none) stardrop(*) nolabel  mlabels(none);
#d cr


#d ;
estout C* using "$out/forensics_app_table_C.tex",  replace style(tex)
varlabels(1.Treat_Letter "$\left[NL\right]$ No Letter - Any Ad" 2.Treat_Letter "$\left[L\right]$ Letter - Any Ad" ,elist(2.Treat_Letter \midrule))
cells(b(star fmt(%9.3f))  se(par fmt(%9.3f)) pexact(par([ ]) fmt(%9.3f)))  keep(1.Treat_Letter 2.Treat_Letter) 
order(1.Treat_Letter 2.Treat_Letter) stats(p_diff j c_mean N, fmt(%9.2f %9.2f %9.2f %9.0f) 
labels("Test $ NL=L$, p-value" "\midrule" "Control Mean" "Sample Size"))
collabels(none) stardrop(*) nolabel  mlabels(none);
#d cr

est clear


/*===============================================================================================
=================================================================================================
          Table A11: Impacts on Forensic Tests Suggested by Beber and Scacco (2012)
=================================================================================================
===============================================================================================*/


use "$final_data/main_data_municipal_level.dta", clear 

global outcomes "z_index_last  sig95_max_last d_repeat_digits d_adjecent_digits"

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


* Panel A: Any Treatment

matrix pexact=J(1,1,.) 

local i=`i'+1

eststo A`i': reg `y'  T_FB `vSel' , cluster(ID)


ritest T_FB _b[T_FB], strata(strata) reps(`iter') seed($seed) force: reg `y' T_FB `vSel' 

matrix pexact[1,1]=r(p) 

mat colnames pexact = T_FB
est restore A`i'
estadd matrix pexact = pexact
sum `y' if T_FB==0
estadd  scalar c_mean = r(mean)


* Panel B: Different Ads

matrix pexact=J(1,3,.) 

eststo B`i': reg `y' i.Treat_Ads  `vSel' , cluster(ID)

test 1.Treat_Ads=2.Treat_Ads
estadd scalar p_diff_a=r(p) 

test 1.Treat_Ads=3.Treat_Ads
estadd scalar p_diff_b=r(p) 

test 3.Treat_Ads=2.Treat_Ads
estadd scalar p_diff_c=r(p) 

ritest Treat_Ads _b[1.Treat_Ads], strata(strata) reps(`iter') seed($seed) fixlevels(2 3) force: reg `y' i.Treat_Ads `vSel' 
matrix pexact[1,1]=r(p) 

ritest Treat_Ads _b[2.Treat_Ads], strata(strata) reps(`iter') seed($seed) fixlevels(1 3) force: reg `y' i.Treat_Ads `vSel' 
matrix pexact[1,2]=r(p) 

ritest Treat_Ads _b[3.Treat_Ads], strata(strata) reps(`iter') seed($seed) fixlevels(1 2) force: reg `y' i.Treat_Ads `vSel' 
matrix pexact[1,3]=r(p) 


mat colnames pexact = 1.Treat_Ads 2.Treat_Ads 3.Treat_Ads
est restore B`i'
estadd matrix pexact = pexact
sum `y' if T_FB==0
estadd  scalar c_mean = r(mean)


* Panel C: Letter - No Letter

matrix pexact=J(1,2,.) 


eststo C`i': reg `y'  i.Treat_Letter `vSel' , cluster(ID)
test 1.Treat_Letter=2.Treat_Letter
estadd scalar p_diff=r(p) 


ritest Treat_Letter _b[1.Treat_Letter] , strata(strata) reps(`iter') seed($seed) fixlevels(2) force: reg `y'  i.Treat_Letter `vSel' 
matrix pexact[1,1]=r(p) 

ritest Treat_Letter _b[2.Treat_Letter] , strata(strata) reps(`iter') seed($seed) fixlevels(1 ) force : reg `y'  i.Treat_Letter `vSel' 
matrix pexact[1,2]=r(p) 

mat colnames pexact = 1.Treat_Letter 2.Treat_Letter
est restore C`i'
estadd matrix pexact = pexact
sum `y' if T_FB==0
estadd  scalar c_mean = r(mean)

}


#d ;
estout A* using "$out/forensics_lastdig_table_A.tex",  replace style(tex)
varlabels(T_FB "$\left[T\right]$ Any treatment",elist(T_FB \midrule))
cells(b(star fmt(%9.3f))  se(par fmt(%9.3f)) pexact(par([ ]) fmt(%9.3f)))  
keep(T_FB) order(T_FB)
collabels(none) stardrop(*) nolabel  mlabels(none);
#d cr

#d ;
estout B* using "$out/forensics_lastdig_table_B.tex",  replace style(tex)
varlabels(1.Treat_Ads "$\left[IA\right]$ Information Ad" 2.Treat_Ads "$\left[CA\right]$ Call-to-Action Ad" 3.Treat_Ads "$\left[I+CA\right]$ Info + Call-to-Action Ad",elist(3.Treat_Ads \midrule))
cells(b(star fmt(%9.3f))  se(par fmt(%9.3f)) pexact(par([ ]) fmt(%9.3f)))  keep(1.Treat_Ads 2.Treat_Ads 3.Treat_Ads) 
order(1.Treat_Ads 2.Treat_Ads 3.Treat_Ads) stats(p_diff_a p_diff_b p_diff_c , fmt(%9.2f %9.2f %9.2f)
labels("Test $ IA=CA$, p-value" "Test $ IA=I+CA$, p-value" "Test $ CA=I+CA$, p-value"))
collabels(none) stardrop(*) nolabel  mlabels(none);
#d cr


#d ;
estout C* using "$out/forensics_lastdig_table_C.tex",  replace style(tex)
varlabels(1.Treat_Letter "$\left[NL\right]$ No Letter - Any Ad" 2.Treat_Letter "$\left[L\right]$ Letter - Any Ad" ,elist(2.Treat_Letter \midrule))
cells(b(star fmt(%9.3f))  se(par fmt(%9.3f)) pexact(par([ ]) fmt(%9.3f)))  keep(1.Treat_Letter 2.Treat_Letter) 
order(1.Treat_Letter 2.Treat_Letter) stats(p_diff j c_mean N, fmt(%9.2f %9.2f %9.2f %9.0f)
labels("Test $ NL=L$, p-value" "\midrule" "Control Mean" "Sample Size"))
collabels(none) stardrop(*) nolabel  mlabels(none);
#d cr

est clear


/*===============================================================================================
=================================================================================================
						Table A12: Covariate Balance - Candidate Level Data
=================================================================================================
===============================================================================================*/

use "$final_data/main_data_candidate_level.dta", clear

* Keeping candidates with survey responses:
keep if p_d_misdeed!=.

label var  p_d_misdeed "Will do irregs (frac)"
label var  d_mean_p_d_misdeed "Demean Will do irregs (frac)"
label var  above_mean_p_d_misdeed "Above Av Will do irregs (=1)"
label var  winner_c "Will win (frac respondents)"

global controls_can_balance "p_d_misdeed d_mean_p_d_misdeed above_mean_p_d_misdeed cuestionado `political'  `violence' `development' `other' `candidates' rr1 rr2 rr3 rr4 rr5 rr6 n_respuestas_pre pct_Reach Reach no_Reach"


foreach y of global controls_can_balance{

matrix mean=J(3,1,.)
matrix A=J(1,24,.)

* 1. Control Mean
reg `y' if T_FB==0, cluster(ID)
matrix mean[1,1]=_b[_cons]
matrix rownames A= "`: var label `y''"

* 2. Any Treatment vs. Control
reg `y' T_FB  , cluster(ID)
matrix A[1,1]=_b[T_FB]
matrix A[1,2]=_se[T_FB]
ritest T_FB _b[T_FB], strata(strata) cluster(ID) reps(`iter') seed($seed) force: reg `y' T_FB
matrix A[1,3]=r(p) 			

* 3. Information vs. Control
reg `y' T_FB_info  if T_FB_call==0 & T_FB_both==0, cluster(ID)
matrix A[1,4]=_b[T_FB_info]
matrix A[1,5]=_se[T_FB_info]
ritest T_FB_info _b[T_FB_info], strata(strata) cluster(ID) reps(`iter') seed($seed) force: reg `y' T_FB_info  if T_FB_call==0 & T_FB_both==0
matrix A[1,6]=r(p) 		

* 4. Call-to-action vs. Control
reg `y' T_FB_call  if T_FB_info==0 & T_FB_both==0, r
matrix A[1,7]=_b[T_FB_call]
matrix A[1,8]=_se[T_FB_call]
ritest T_FB_call _b[T_FB_call], strata(strata) cluster(ID) reps(`iter') seed($seed) force: reg `y' T_FB_call  if T_FB_info==0 & T_FB_both==0
matrix A[1,9]=r(p) 	

* 5. Info + Call-to-action vs. Control
reg `y' T_FB_both  if T_FB_call==0 & T_FB_info==0, r
matrix A[1,10]=_b[T_FB_both]
matrix A[1,11]=_se[T_FB_both]
ritest T_FB_both _b[T_FB_both], strata(strata) cluster(ID) reps(`iter') seed($seed) force: reg `y' T_FB_both  if T_FB_call==0 & T_FB_info==0
matrix A[1,12]=r(p) 	
	
* 6. Any Letter vs. No Letter
reg `y' T_Letter  if T_FB==1, r
matrix A[1,13]=_b[T_Letter]
matrix A[1,14]=_se[T_Letter]
ritest T_Letter _b[T_Letter], strata(strata) cluster(ID) reps(`iter') seed($seed) force: reg `y'  T_Letter  if T_FB==1
matrix A[1,15]=r(p) 

* 7. Letter P. Knowledge vs. No Letter
reg `y' T_Letter_no_sj  if T_FB==1 & T_Letter_sj==0, r
matrix A[1,16]=_b[T_Letter_no_sj]
matrix A[1,17]=_se[T_Letter_no_sj]
ritest T_Letter_no_sj _b[T_Letter_no_sj], strata(strata) cluster(ID) reps(`iter') seed($seed) force: reg `y' T_Letter_no_sj  if T_FB==1 & T_Letter_sj==0
matrix A[1,18]=r(p) 

* 8. Letter F. Knowledge vs. No Letter
reg `y' T_Letter_sj  if T_FB==1 & T_Letter_no_sj==0, r
matrix A[1,19]=_b[T_Letter_sj]
matrix A[1,20]=_se[T_Letter_sj]
ritest T_Letter_sj _b[T_Letter_sj], strata(strata) cluster(ID) reps(`iter') seed($seed) force: reg `y' T_Letter_sj  if T_FB==1 & T_Letter_no_sj==0
matrix A[1,21]=r(p) 

* 9. Letter F. Knowledge vs. Letter P. Knowledge
reg `y' T_Letter_sj  if T_Letter==1 , r
matrix A[1,22]=_b[T_Letter_sj]
matrix A[1,23]=_se[T_Letter_sj]
ritest T_Letter_sj _b[T_Letter_sj], strata(strata) cluster(ID) reps(`iter') seed($seed) force: reg `y' T_Letter_sj  if T_Letter==1 
matrix A[1,24]=r(p) 	

frmttable, statmat(mean) sdec(3) tex  replace store(mean)

frmttable using "$out_balance/balance_can_`y'.tex", statmat(A) sdec(3) substat(2)  tex  replace merge(mean) fragment


preserve
clear
set obs 1
local tex "$out_balance/balance_can_`y'.tex"


generate strL s = fileread("`tex'") if fileexists("`tex'")
assert filereaderror(s)==0
replace s = subinstr(s,"\begin{center}","",1)   
replace s = subinstr(s,"\begin{tabular}{lccccccccc}","",1)   
replace s = subinstr(s,"\hline \noalign{\smallskip}","",1)   
replace s = subinstr(s,"\noalign{\smallskip}\hline\end{tabular}\\","",1)   
replace s = subinstr(s,"\end{center}","",1)   
gen byte fw = filewrite("`tex'",s,1)
restore
}


/*===============================================================================================
=================================================================================================
				Table A13: Correlation Between Past Malfeasance and Survey 
					Measures of Likelihood to Engage in Irregularities
=================================================================================================
===============================================================================================*/

use "$final_data/main_data_candidate_level.dta", clear

global outcomes "p_d_misdeed d_mean_p_d_misdeed above_mean_p_d_misdeed"


sum cuestionado
gen z_cuestionado=(cuestionado-`r(mean)')/`r(sd)'

local i=0
foreach y of global outcomes{
local i=`i'+1
sum `y'
gen z_`y'=(`y'-`r(mean)')/`r(sd)'

eststo A`i': reg z_cuestionado z_`y', r

distinct ID if cuestionado!=. 
estadd  scalar n_mun = r(ndistinct)

}

 

#d ;
estout A* using "$out/correlation_cuestionado_table.tex",  replace style(tex)
varlabels( z_p_d_misdeed "\shortstack{Candidate will \\ engage in irregularities \\ (z-score)}" z_d_mean_p_d_misdeed "\shortstack{Demeaned \\\ Candidate will \\ engage in irregularities  \\ (z-score)}"
z_above_mean_p_d_misdeed "\shortstack{Above Average \\\ Candidate will \\ engage in irregularities  \\ (z-score)} " ,elist(z_above_mean_p_d_misdeed \midrule))
cells(b(star fmt(%9.3f))  se(par fmt(%9.3f)) pexact(par([ ]) fmt(%9.3f)))  keep(z_p_d_misdeed z_d_mean_p_d_misdeed z_above_mean_p_d_misdeed) 
order(z_p_d_misdeed z_d_mean_p_d_misdeed z_above_mean_p_d_misdeed) stats( N n_mun, fmt( %9.0f %9.0f)
labels( "Sample Size" "N. Municipalities"))
collabels(none) stardrop(*) nolabel  mlabels(none);
#d cr
est clear


/*===============================================================================================
=================================================================================================
					  Table A14: Correlation Between Measures of Candidate 
						Engagement in Irregularities and Other Covariates
=================================================================================================
===============================================================================================*/

use "$final_data/main_data_candidate_level.dta", clear


global outcomes "winner_c incumbent incumbent_loose "

sum p_d_misdeed
gen z_p_d_misdeed=(p_d_misdeed-`r(mean)')/`r(sd)'


sum d_mean_p_d_misdeed
gen z_d_mean_p_d_misdeed=(d_mean_p_d_misdeed-`r(mean)')/`r(sd)'

sum above_mean_p_d_misdeed
gen z_above_mean_p_d_misdeed=(above_mean_p_d_misdeed-`r(mean)')/`r(sd)'

local i=0
foreach y of global outcomes{
local i=`i'+1
sum `y'
gen z_`y'=(`y'-`r(mean)')/`r(sd)'

eststo A`i': reg  z_`y' z_p_d_misdeed , r

distinct ID if e(sample)==1
estadd  scalar n_mun = r(ndistinct)

local i=`i'+1
eststo A`i': reg z_`y' z_d_mean_p_d_misdeed , r

distinct ID if e(sample)==1
estadd  scalar n_mun = r(ndistinct)

local i=`i'+1
eststo A`i': reg z_`y' z_above_mean_p_d_misdeed , r

distinct ID if e(sample)==1
estadd  scalar n_mun = r(ndistinct)

}



#d ;
estout A* using "$out/correlation_can_variables_table.tex",  replace style(tex)
varlabels( z_p_d_misdeed "\shortstack{Candidate will \\ engage in irregularities \\ (z-score)}" z_d_mean_p_d_misdeed "\shortstack{Demeaned \\\ Candidate will \\ engage in irregularities  \\ (z-score)}"
z_above_mean_p_d_misdeed "\shortstack{Above Average \\\ Candidate will \\ engage in irregularities  \\ (z-score)} " ,elist(z_above_mean_p_d_misdeed \midrule))
cells(b(star fmt(%9.3f))  se(par fmt(%9.3f)) pexact(par([ ]) fmt(%9.3f)))  keep(z_p_d_misdeed z_d_mean_p_d_misdeed z_above_mean_p_d_misdeed) 
order(z_p_d_misdeed z_d_mean_p_d_misdeed z_above_mean_p_d_misdeed) stats( N n_mun, fmt( %9.0f %9.0f)
labels( "Sample Size" "N. Municipalities"))
collabels(none) stardrop(*) nolabel  mlabels(none);
#d cr
est clear


/*===============================================================================================
=================================================================================================
				Table A15: Impacts on Vote Share of More Popular Candidates
=================================================================================================
===============================================================================================*/

use "$final_data/main_data_candidate_level.dta", clear

keep if p_d_misdeed!=.


global outcomes "pct_vote"
global interactions "winner_c incumbent incumbent_loose"


foreach y of global outcomes{

foreach z of global interactions{

gen zz=`z'


quietly: rlasso `y'  $controls_can , cluster(ID)
local ySel `e(selected)'

quietly: rlasso T_FB  $controls_can , cluster(ID)
local xSel0 `e(selected)' 

quietly: rlasso TT_FB  $controls_can , cluster(ID)
local xSel1 `e(selected)' 

quietly: rlasso T_FB_call  $controls_can , cluster(ID)
local xSel2 `e(selected)' 

quietly: rlasso T_FB_info  $controls_can , cluster(ID)
local xSel3 `e(selected)' 

quietly: rlasso T_FB_both  $controls_can , cluster(ID)
local xSel4 `e(selected)' 

quietly: rlasso T_Letter  $controls_can , cluster(ID)
local xSel5 `e(selected)'

quietly: rlasso T_Letter_no_sj  $controls_can , cluster(ID)
local xSel6 `e(selected)' 

quietly: rlasso T_Letter_sj  $controls_can , cluster(ID)
local xSel7 `e(selected)' 


local vSel : list ySel | xSel0
local vSel : list vSel | xSel1
local vSel : list vSel | xSel2
local vSel : list vSel | xSel3
local vSel : list vSel | xSel4
local vSel : list vSel | xSel5
local vSel : list vSel | xSel6
local vSel : list vSel | xSel7


* Panel A: Any Treatment

matrix pexact=J(1,1,.) 

local i=`i'+1

eststo A`i': reg `y'  i.T_FB##c.zz `vSel' , cluster(ID)
estadd  scalar n_mun = e(N_clust)


ritest T_FB _b[1.T_FB#c.zz], strata(strata) cluster(ID) reps(`iter') seed($seed) force: reg `y' i.T_FB##c.zz `vSel' 

matrix pexact[1,1]=r(p) 

mat colnames pexact = 1.T_FB#c.zz
est restore A`i'
estadd matrix pexact = pexact
sum `y' if T_FB==0
estadd  scalar c_mean = r(mean)


* Panel B: Different Ads

matrix pexact=J(1,3,.) 


eststo B`i': reg `y'  i.Treat_Ads##c.zz  `vSel'  , cluster(ID)
estadd  scalar n_mun = e(N_clust)

test 1.Treat_Ads#c.zz=2.Treat_Ads#c.zz
estadd scalar p_diff_az=r(p) 

test 1.Treat_Ads#c.zz=3.Treat_Ads#c.zz
estadd scalar p_diff_bz=r(p) 

test 3.Treat_Ads#c.zz=2.Treat_Ads#c.zz
estadd scalar p_diff_cz=r(p) 

ritest Treat_Ads _b[1.Treat_Ads#c.zz] , strata(strata) cluster(ID) reps(`iter') seed($seed) fixlevels(2 3) force: reg `y'  i.Treat_Ads##c.zz `vSel' 
matrix pexact[1,1]=r(p) 

ritest Treat_Ads _b[2.Treat_Ads#c.zz] , strata(strata) cluster(ID) reps(`iter') seed($seed) fixlevels(1 3) force : reg `y'  i.Treat_Ads##c.zz `vSel' 
matrix pexact[1,2]=r(p) 

ritest Treat_Ads _b[3.Treat_Ads#c.zz] , strata(strata) cluster(ID) reps(`iter') seed($seed) fixlevels(1 2) force : reg `y'  i.Treat_Ads##c.zz `vSel' 
matrix pexact[1,3]=r(p) 

mat colnames pexact = 1.Treat_Ads#c.zz 2.Treat_Ads#c.zz 3.Treat_Ads#c.zz 
est restore B`i'
estadd matrix pexact = pexact
sum `y' if T_FB==0
estadd  scalar c_mean = r(mean)


* Panel C: Letter - No Letter

matrix pexact=J(1,2,.) 


eststo C`i': reg `y'  i.Treat_Letter##c.zz  `vSel'  , cluster(ID)
estadd  scalar n_mun = e(N_clust)
test 1.Treat_Letter#c.zz =2.Treat_Letter#c.zz 
estadd scalar p_diff1=r(p) 


ritest Treat_Letter _b[1.Treat_Letter#c.zz] , strata(strata) cluster(ID) reps(`iter') seed($seed) fixlevels(2) force: reg `y'  i.Treat_Letter##c.zz `vSel' 
matrix pexact[1,1]=r(p) 

ritest Treat_Letter _b[2.Treat_Letter#c.zz] , strata(strata) cluster(ID) reps(`iter') seed($seed) fixlevels(1) force : reg `y'  i.Treat_Letter##c.zz `vSel' 
matrix pexact[1,2]=r(p) 

mat colnames pexact = 1.Treat_Letter#c.zz 2.Treat_Letter#c.zz 
est restore C`i'
estadd matrix pexact = pexact
sum `y' if T_FB==0
estadd  scalar c_mean = r(mean)


drop *zz*
}
}


#d ;
estout A* using "$out/elections_app_table_A.tex",  replace style(tex)
varlabels(1.T_FB "$\left[T\right]$ Any treatment" 1.T_FB#c.zz "$\left[T\times Z \right]$ Any treatment $\times Z$",elist(1.T_FB#c.zz \midrule))
cells(b(star fmt(%9.3f))  se(par fmt(%9.3f)) pexact(par([ ]) fmt(%9.3f)))  
keep(1.T_FB#c.zz) order(1.T_FB#c.zz)
collabels(none) stardrop(*) nolabel  mlabels(none);
#d cr

#d ;
estout B* using "$out/elections_app_table_B.tex",  replace style(tex)
varlabels(1.Treat_Ads "$\left[IA\right]$ Information Ad" 2.Treat_Ads "$\left[CA\right]$ Call-to-Action Ad" 3.Treat_Ads "$\left[I+CA\right]$ Info + Call-to-Action Ad"
1.Treat_Ads#c.zz "$\left[IA\times Z \right]$ Information Ad $\times Z$" 2.Treat_Ads#c.zz "$\left[CA \times Z \right]$ Call-to-Action Ad $\times Z$" 3.Treat_Ads#c.zz "$\left[I+CA \times Z \right]$ Info + Call-to-Action Ad $\times Z$"
,elist(3.Treat_Ads#c.zz \midrule))
cells(b(star fmt(%9.3f))  se(par fmt(%9.3f)) pexact(par([ ]) fmt(%9.3f)))  keep(1.Treat_Ads#c.zz 2.Treat_Ads#c.zz 3.Treat_Ads#c.zz) 
order(1.Treat_Ads#c.zz 2.Treat_Ads#c.zz 3.Treat_Ads#c.zz) stats( p_diff_az p_diff_bz p_diff_cz , fmt( %9.2f %9.2f %9.2f)
labels("Test $ IA \times Z=CA \times Z$, p-value" "Test $ IA \times Z=I+CA \times Z$, p-value" "Test $ CA \times Z=I+CA \times Z$, p-value"))
collabels(none) stardrop(*) nolabel  mlabels(none);
#d cr


#d ;
estout C* using "$out/elections_app_table_C.tex",  replace style(tex)
varlabels(1.Treat_Letter#c.zz "$\left[NL\times Z \right]$ No Letter - Any Ad $\times Z$" 2.Treat_Letter#c.zz "$\left[L\times Z \right]$ Letter - Any Ad $\times Z$" 
1.Treat_Letter "$\left[NL \right]$ No Letter - Any Ad" 2.Treat_Letter "$\left[L \right]$ Letter - Any Ad " ,elist(2.Treat_Letter#c.zz \midrule))
cells(b(star fmt(%9.3f))  se(par fmt(%9.3f)) pexact(par([ ]) fmt(%9.3f)))  keep( 1.Treat_Letter#c.zz  2.Treat_Letter#c.zz) 
order(1.Treat_Letter 2.Treat_Letter 1.Treat_Letter#c.zz 2.Treat_Letter#c.zz) stats( p_diff1 j c_mean N n_mun , fmt( %9.2f %9.2f %9.2f  %9.0f %9.0f)
labels("Test $ NL \times Z=L \times Z$, p-value" "\midrule" "Control Mean" "Sample Size" "N. Municipalities"))
collabels(none) stardrop(*) nolabel  mlabels(none);
#d cr

est clear


/*===============================================================================================
=================================================================================================
						Table A16: Impacts on Additional Electoral Outcomes
=================================================================================================
===============================================================================================*/

use "$final_data/main_data_municipal_level.dta", clear 

global outcomes "turnout  win_marg2019"

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


* Panel A: Any Treatment

matrix pexact=J(1,1,.) 

local i=`i'+1

eststo A`i': reg `y'  T_FB `vSel' , cluster(ID)


ritest T_FB _b[T_FB], strata(strata) reps(`iter') seed($seed) force: reg `y' T_FB `vSel'  

matrix pexact[1,1]=r(p) 

mat colnames pexact = T_FB
est restore A`i'
estadd matrix pexact = pexact
sum `y' if T_FB==0
estadd  scalar c_mean = r(mean)


* Panel B: Different Ads

matrix pexact=J(1,3,.) 

eststo B`i': reg `y' i.Treat_Ads  `vSel'  , cluster(ID)

test 1.Treat_Ads=2.Treat_Ads
estadd scalar p_diff_a=r(p) 

test 1.Treat_Ads=3.Treat_Ads
estadd scalar p_diff_b=r(p) 

test 3.Treat_Ads=2.Treat_Ads
estadd scalar p_diff_c=r(p) 

ritest Treat_Ads _b[1.Treat_Ads], strata(strata) reps(`iter') seed($seed) fixlevels(2 3) force: reg `y' i.Treat_Ads `vSel' 
matrix pexact[1,1]=r(p) 

ritest Treat_Ads _b[2.Treat_Ads], strata(strata) reps(`iter') seed($seed) fixlevels(1 3) force: reg `y' i.Treat_Ads `vSel' 
matrix pexact[1,2]=r(p) 

ritest Treat_Ads _b[3.Treat_Ads], strata(strata) reps(`iter') seed($seed) fixlevels(1 2) force: reg `y' i.Treat_Ads `vSel' 
matrix pexact[1,3]=r(p) 


mat colnames pexact = 1.Treat_Ads 2.Treat_Ads 3.Treat_Ads
est restore B`i'
estadd matrix pexact = pexact
sum `y' if T_FB==0
estadd  scalar c_mean = r(mean)


* Panel C: Letter - No Letter

matrix pexact=J(1,2,.) 


eststo C`i': reg `y'  i.Treat_Letter `vSel'  , cluster(ID)
test 1.Treat_Letter=2.Treat_Letter
estadd scalar p_diff=r(p) 


ritest Treat_Letter _b[1.Treat_Letter] , strata(strata) reps(`iter') seed($seed) fixlevels(2) force: reg `y'  i.Treat_Letter `vSel' 
matrix pexact[1,1]=r(p) 

ritest Treat_Letter _b[2.Treat_Letter] , strata(strata) reps(`iter') seed($seed) fixlevels(1 ) force : reg `y'  i.Treat_Letter `vSel' 
matrix pexact[1,2]=r(p) 

mat colnames pexact = 1.Treat_Letter 2.Treat_Letter
est restore C`i'
estadd matrix pexact = pexact
sum `y' if T_FB==0
estadd  scalar c_mean = r(mean)


}


#d ;
estout A* using "$out/elections_mun_table_A.tex",  replace style(tex)
varlabels(T_FB "$\left[T\right]$ Any treatment",elist(T_FB \midrule))
cells(b(star fmt(%9.3f))  se(par fmt(%9.3f)) pexact(par([ ]) fmt(%9.3f)))  
keep(T_FB) order(T_FB)
collabels(none) stardrop(*) nolabel  mlabels(none);
#d cr

#d ;
estout B* using "$out/elections_mun_table_B.tex",  replace style(tex)
varlabels(1.Treat_Ads "$\left[IA\right]$ Information Ad" 2.Treat_Ads "$\left[CA\right]$ Call-to-Action Ad" 3.Treat_Ads "$\left[I+CA\right]$ Info + Call-to-Action Ad",elist(3.Treat_Ads \midrule))
cells(b(star fmt(%9.3f))  se(par fmt(%9.3f)) pexact(par([ ]) fmt(%9.3f)))  keep(1.Treat_Ads 2.Treat_Ads 3.Treat_Ads) 
order(1.Treat_Ads 2.Treat_Ads 3.Treat_Ads) stats(p_diff_a p_diff_b p_diff_c , fmt(%9.2f %9.2f %9.2f)
labels("Test $ IA=CA$, p-value" "Test $ IA=I+CA$, p-value" "Test $ CA=I+CA$, p-value"))
collabels(none) stardrop(*) nolabel  mlabels(none);
#d cr


#d ;
estout C* using "$out/elections_mun_table_C.tex",  replace style(tex)
varlabels(1.Treat_Letter "$\left[NL\right]$ No Letter - Any Ad" 2.Treat_Letter "$\left[L\right]$ Letter - Any Ad" ,elist(2.Treat_Letter \midrule))
cells(b(star fmt(%9.3f))  se(par fmt(%9.3f)) pexact(par([ ]) fmt(%9.3f)))  keep(1.Treat_Letter 2.Treat_Letter) 
order(1.Treat_Letter 2.Treat_Letter) stats(p_diff , fmt(%9.2f)
labels("Test $ NL=L$, p-value"))
collabels(none) stardrop(*) nolabel  mlabels(none);
#d cr

est clear


/*===============================================================================================
=================================================================================================
				Table A17: Estimates of the Percentage of the Effect on Candidate 
					 Vote Share Due to Decreasing Electoral Irregularities
=================================================================================================
===============================================================================================*/

use "$final_data/main_data_candidate_level.dta", clear


global Zc "p_d_misdeed d_mean_p_d_misdeed above_mean_p_d_misdeed"


matrix dVdT=J(1,3,.) 
matrix dVdI=J(4,3,.) 

local i=0

foreach z of global Zc{

gen zz=`z'


quietly: rlasso pct_vote  $controls_can , cluster(ID)
local ySel `e(selected)'

quietly: rlasso T_FB  $controls_can , cluster(ID)
local xSel0 `e(selected)' 

quietly: rlasso TT_FB  $controls_can , cluster(ID)
local xSel1 `e(selected)' 

quietly: rlasso T_FB_call  $controls_can , cluster(ID)
local xSel2 `e(selected)' 

quietly: rlasso T_FB_info  $controls_can , cluster(ID)
local xSel3 `e(selected)' 

quietly: rlasso T_FB_both  $controls_can , cluster(ID)
local xSel4 `e(selected)' 

quietly: rlasso T_Letter  $controls_can , cluster(ID)
local xSel5 `e(selected)'

quietly: rlasso T_Letter_no_sj  $controls_can , cluster(ID)
local xSel6 `e(selected)' 

quietly: rlasso T_Letter_sj  $controls_can , cluster(ID)
local xSel7 `e(selected)' 


local vSel : list ySel | xSel0
local vSel : list vSel | xSel1
local vSel : list vSel | xSel2
local vSel : list vSel | xSel3
local vSel : list vSel | xSel4
local vSel : list vSel | xSel5
local vSel : list vSel | xSel6
local vSel : list vSel | xSel7


local i=`i'+1

* Storing dV/dT:

reg pct_vote  i.T_FB##c.zz `vSel' , cluster(ID)
matrix dVdT[1,`i']=_b[1.T_FB#c.zz]

* Storing dV/dI:


global outcomes "d_media_irreg media_irreg z_index_second sig95_max_second"

local j=0
foreach I of global outcomes{
local j=`j'+1
reg pct_vote  c.`I'##c.zz if T_FB==0, cluster(ID)
matrix dVdI[`j',`i']=_b[c.`I'#c.zz]
}
drop *zz*
}


use "$final_data/main_data_municipal_level.dta", clear 


global irregs "d_media_irreg media_irreg z_index_second sig95_max_second"
local j=0

matrix dIdT=J(4,1,.) 
matrix table_a17=J(4,3,.) 

foreach y of global irregs{


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



local j=`j'+1

* Storing dI/dT:
reg `y'  T_FB `vSel' , cluster(ID)
matrix dIdT[`j',1]=_b[T_FB]

* Computing final estimates matrix:

matrix table_a17[`j',1]=dVdI[`j',1]*dIdT[`j',1]*100/dVdT[1,1]
matrix table_a17[`j',2]=dVdI[`j',2]*dIdT[`j',1]*100/dVdT[1,2]
matrix table_a17[`j',3]=dVdI[`j',3]*dIdT[`j',1]*100/dVdT[1,3]
}


matrix rownames table_a17 = "Media Irregularities (=1)" "Number of Media Irregularities" "Index - All Forensic Stats" "Any P-val From Forensics $<0.05 (=1)$"
matrix colnames table_a17 = "Will do irregs (frac)" "Demean Will do irregs (frac)" "Above Av Will do irregs (=1)"

frmttable using "$out/table_a17.tex", statmat(table_a17) sdec(2)  tex  replace fragment

preserve
clear
set obs 1
local tex "$out/table_a17.tex"

generate strL s = fileread("`tex'") if fileexists("`tex'")
assert filereaderror(s)==0
replace s = subinstr(s,"\begin{center}","",1)   
replace s = subinstr(s,"\begin{tabular}{lccc}","",1)   
replace s = subinstr(s,"\hline \noalign{\smallskip}","",1)   
replace s = subinstr(s,"\noalign{\smallskip}\hline \noalign{\smallskip}","",1) 
replace s = subinstr(s,"\noalign{\smallskip}\hline\end{tabular}\\","",1)   
replace s = subinstr(s,"\end{center}","",1)   
gen byte fw = filewrite("`tex'",s,1)
restore



/*===============================================================================================
=================================================================================================
					Table A18: Robustness: Impacts on Reports - No Controls
=================================================================================================
===============================================================================================*/

use "$final_data/main_data_municipal_level.dta", clear 

global outcomes "d_reportMOE_any reportMOE_any d_reportMOE_any_quality reportMOE_any_quality"

local i=0

foreach y of global outcomes{



* Panel A: Any Treatment

matrix pexact=J(1,1,.) 

local i=`i'+1

eststo A`i': reg `y'  T_FB ss* , cluster(ID)


ritest T_FB _b[T_FB], strata(strata) reps(`iter') seed($seed) force: reg `y' T_FB ss* 

matrix pexact[1,1]=r(p) 

mat colnames pexact = T_FB
est restore A`i'
estadd matrix pexact = pexact
sum `y' if T_FB==0
estadd  scalar c_mean = r(mean)


* Panel B: Different Ads

matrix pexact=J(1,3,.) 

eststo B`i': reg `y' i.Treat_Ads  ss* , cluster(ID)

test 1.Treat_Ads=2.Treat_Ads
estadd scalar p_diff_a=r(p) 

test 1.Treat_Ads=3.Treat_Ads
estadd scalar p_diff_b=r(p) 

test 3.Treat_Ads=2.Treat_Ads
estadd scalar p_diff_c=r(p) 

ritest Treat_Ads _b[1.Treat_Ads], strata(strata) reps(`iter') seed($seed) fixlevels(2 3) force: reg `y' i.Treat_Ads ss*
matrix pexact[1,1]=r(p) 

ritest Treat_Ads _b[2.Treat_Ads], strata(strata) reps(`iter') seed($seed) fixlevels(1 3) force: reg `y' i.Treat_Ads ss*
matrix pexact[1,2]=r(p) 

ritest Treat_Ads _b[3.Treat_Ads], strata(strata) reps(`iter') seed($seed) fixlevels(1 2) force: reg `y' i.Treat_Ads ss* 
matrix pexact[1,3]=r(p) 


mat colnames pexact = 1.Treat_Ads 2.Treat_Ads 3.Treat_Ads
est restore B`i'
estadd matrix pexact = pexact
sum `y' if T_FB==0
estadd  scalar c_mean = r(mean)


* Panel C: Letter - No Letter

matrix pexact=J(1,2,.) 


eststo C`i': reg `y'  i.Treat_Letter ss* , cluster(ID)
test 1.Treat_Letter=2.Treat_Letter
estadd scalar p_diff=r(p) 


ritest Treat_Letter _b[1.Treat_Letter] , strata(strata) reps(`iter') seed($seed) fixlevels(2) force: reg `y'  i.Treat_Letter ss*
matrix pexact[1,1]=r(p) 

ritest Treat_Letter _b[2.Treat_Letter] , strata(strata) reps(`iter') seed($seed) fixlevels(1 ) force : reg `y'  i.Treat_Letter ss*
matrix pexact[1,2]=r(p) 

mat colnames pexact = 1.Treat_Letter 2.Treat_Letter
est restore C`i'
estadd matrix pexact = pexact
sum `y' if T_FB==0
estadd  scalar c_mean = r(mean)

}


#d ;
estout A* using "$out/MOEreport_table_nocontrols_A.tex",  replace style(tex)
varlabels(T_FB "$\left[T\right]$ Any treatment",elist(T_FB \midrule))
cells(b(star fmt(%9.3f))  se(par fmt(%9.3f)) pexact(par([ ]) fmt(%9.3f)))  
keep(T_FB) order(T_FB)
collabels(none) stardrop(*) nolabel  mlabels(none);
#d cr

#d ;
estout B* using "$out/MOEreport_table_nocontrols_B.tex",  replace style(tex)
varlabels(1.Treat_Ads "$\left[IA\right]$ Information Ad" 2.Treat_Ads "$\left[CA\right]$ Call-to-Action Ad" 3.Treat_Ads "$\left[I+CA\right]$ Info + Call-to-Action Ad",elist(3.Treat_Ads \midrule))
cells(b(star fmt(%9.3f))  se(par fmt(%9.3f)) pexact(par([ ]) fmt(%9.3f)))  keep(1.Treat_Ads 2.Treat_Ads 3.Treat_Ads) 
order(1.Treat_Ads 2.Treat_Ads 3.Treat_Ads) stats(p_diff_a p_diff_b p_diff_c , fmt(%9.2f %9.2f %9.2f)
labels("Test $ IA=CA$, p-value" "Test $ IA=I+CA$, p-value" "Test $ CA=I+CA$, p-value"))
collabels(none) stardrop(*) nolabel  mlabels(none);
#d cr


#d ;
estout C* using "$out/MOEreport_table_nocontrols_C.tex",  replace style(tex)
varlabels(1.Treat_Letter "$\left[NL\right]$ No Letter - Any Ad" 2.Treat_Letter "$\left[L\right]$ Letter - Any Ad" ,elist(2.Treat_Letter \midrule))
cells(b(star fmt(%9.3f))  se(par fmt(%9.3f)) pexact(par([ ]) fmt(%9.3f)))  keep(1.Treat_Letter 2.Treat_Letter) 
order(1.Treat_Letter 2.Treat_Letter) stats(p_diff j c_mean N, fmt(%9.2f %9.2f %9.2f %9.0f) 
labels("Test $ NL=L$, p-value" "\midrule" "Control Mean" "Sample Size"))
collabels(none) stardrop(*) nolabel  mlabels(none);
#d cr

est clear


/*===============================================================================================
=================================================================================================
						Table A19: Impacts on Irregularity Measures - No Controls
=================================================================================================
===============================================================================================*/

use "$final_data/main_data_municipal_level.dta", clear 

global outcomes "d_media_irreg media_irreg z_index_second sig95_max_second"
local i=0


foreach y of global outcomes{



* Panel A: Any Treatment

matrix pexact=J(1,1,.) 

local i=`i'+1

eststo A`i': reg `y'  T_FB ss* , cluster(ID)


ritest T_FB _b[T_FB], strata(strata) reps(`iter') seed($seed) force: reg `y' T_FB ss* 

matrix pexact[1,1]=r(p) 

mat colnames pexact = T_FB
est restore A`i'
estadd matrix pexact = pexact
sum `y' if T_FB==0
estadd  scalar c_mean = r(mean)


* Panel B: Different Ads

matrix pexact=J(1,3,.) 

eststo B`i': reg `y' i.Treat_Ads  ss* , cluster(ID)

test 1.Treat_Ads=2.Treat_Ads
estadd scalar p_diff_a=r(p) 

test 1.Treat_Ads=3.Treat_Ads
estadd scalar p_diff_b=r(p) 

test 3.Treat_Ads=2.Treat_Ads
estadd scalar p_diff_c=r(p) 

ritest Treat_Ads _b[1.Treat_Ads], strata(strata) reps(`iter') seed($seed) fixlevels(2 3) force: reg `y' i.Treat_Ads ss* 
matrix pexact[1,1]=r(p) 

ritest Treat_Ads _b[2.Treat_Ads], strata(strata) reps(`iter') seed($seed) fixlevels(1 3) force: reg `y' i.Treat_Ads ss* 
matrix pexact[1,2]=r(p) 

ritest Treat_Ads _b[3.Treat_Ads], strata(strata) reps(`iter') seed($seed) fixlevels(1 2) force: reg `y' i.Treat_Ads ss* 
matrix pexact[1,3]=r(p) 


mat colnames pexact = 1.Treat_Ads 2.Treat_Ads 3.Treat_Ads
est restore B`i'
estadd matrix pexact = pexact
sum `y' if T_FB==0
estadd  scalar c_mean = r(mean)


* Panel C: Letter - No Letter

matrix pexact=J(1,2,.) 


eststo C`i': reg `y'  i.Treat_Letter ss* , cluster(ID)
test 1.Treat_Letter=2.Treat_Letter
estadd scalar p_diff=r(p) 


ritest Treat_Letter _b[1.Treat_Letter] , strata(strata) reps(`iter') seed($seed) fixlevels(2) force: reg `y'  i.Treat_Letter ss* 
matrix pexact[1,1]=r(p) 

ritest Treat_Letter _b[2.Treat_Letter] , strata(strata) reps(`iter') seed($seed) fixlevels(1 ) force : reg `y'  i.Treat_Letter ss* 
matrix pexact[1,2]=r(p) 

mat colnames pexact = 1.Treat_Letter 2.Treat_Letter
est restore C`i'
estadd matrix pexact = pexact
sum `y' if T_FB==0
estadd  scalar c_mean = r(mean)

}


#d ;
estout A* using "$out/irregularities_table_nocontrols_A.tex",  replace style(tex)
varlabels(T_FB "$\left[T\right]$ Any treatment",elist(T_FB \midrule))
cells(b(star fmt(%9.3f))  se(par fmt(%9.3f)) pexact(par([ ]) fmt(%9.3f)))  
keep(T_FB) order(T_FB)
collabels(none) stardrop(*) nolabel  mlabels(none);
#d cr

#d ;
estout B* using "$out/irregularities_table_nocontrols_B.tex",  replace style(tex)
varlabels(1.Treat_Ads "$\left[IA\right]$ Information Ad" 2.Treat_Ads "$\left[CA\right]$ Call-to-Action Ad" 3.Treat_Ads "$\left[I+CA\right]$ Info + Call-to-Action Ad",elist(3.Treat_Ads \midrule))
cells(b(star fmt(%9.3f))  se(par fmt(%9.3f)) pexact(par([ ]) fmt(%9.3f)))  keep(1.Treat_Ads 2.Treat_Ads 3.Treat_Ads) 
order(1.Treat_Ads 2.Treat_Ads 3.Treat_Ads) stats(p_diff_a p_diff_b p_diff_c , fmt(%9.2f %9.2f %9.2f)
labels("Test $ IA=CA$, p-value" "Test $ IA=I+CA$, p-value" "Test $ CA=I+CA$, p-value"))
collabels(none) stardrop(*) nolabel  mlabels(none);
#d cr


#d ;
estout C* using "$out/irregularities_table_nocontrols_C.tex",  replace style(tex)
varlabels(1.Treat_Letter "$\left[NL\right]$ No Letter - Any Ad" 2.Treat_Letter "$\left[L\right]$ Letter - Any Ad" ,elist(2.Treat_Letter \midrule))
cells(b(star fmt(%9.3f))  se(par fmt(%9.3f)) pexact(par([ ]) fmt(%9.3f)))  keep(1.Treat_Letter 2.Treat_Letter) 
order(1.Treat_Letter 2.Treat_Letter) stats(p_diff j c_mean N, fmt(%9.2f %9.2f %9.2f %9.0f) 
labels("Test $ NL=L$, p-value" "\midrule" "Control Mean" "Sample Size"))
collabels(none) stardrop(*) nolabel  mlabels(none);
#d cr

est clear


/*===============================================================================================
=================================================================================================
					Table A20: Robustness: Impacts on Vote Share of Candidates 
						Likely to Engage in Irregularities - No Controls
=================================================================================================
===============================================================================================*/

use "$final_data/main_data_candidate_level.dta", clear

global outcomes "pct_vote"
global interactions "p_d_misdeed d_mean_p_d_misdeed above_mean_p_d_misdeed cuestionado"


foreach y of global outcomes{

foreach z of global interactions{

gen zz=`z'


* Panel A: Any Treatment

matrix pexact=J(1,1,.) 

local i=`i'+1

eststo A`i': reg `y'  i.T_FB##c.zz ss* , cluster(ID)
estadd  scalar n_mun = e(N_clust)


ritest T_FB _b[1.T_FB#c.zz], strata(strata) cluster(ID) reps(`iter') seed($seed) force: reg `y' i.T_FB##c.zz ss* 

matrix pexact[1,1]=r(p) 

mat colnames pexact = 1.T_FB#c.zz
est restore A`i'
estadd matrix pexact = pexact
sum `y' if T_FB==0
estadd  scalar c_mean = r(mean)


* Panel B: Different Ads

matrix pexact=J(1,3,.) 


eststo B`i': reg `y'  i.Treat_Ads##c.zz  ss*  , cluster(ID)
estadd  scalar n_mun = e(N_clust)

test 1.Treat_Ads#c.zz=2.Treat_Ads#c.zz
estadd scalar p_diff_az=r(p) 

test 1.Treat_Ads#c.zz=3.Treat_Ads#c.zz
estadd scalar p_diff_bz=r(p) 

test 3.Treat_Ads#c.zz=2.Treat_Ads#c.zz
estadd scalar p_diff_cz=r(p) 

ritest Treat_Ads _b[1.Treat_Ads#c.zz] , strata(strata) cluster(ID) reps(`iter') seed($seed) fixlevels(2 3) force: reg `y'  i.Treat_Ads##c.zz ss* 
matrix pexact[1,1]=r(p) 

ritest Treat_Ads _b[2.Treat_Ads#c.zz] , strata(strata) cluster(ID) reps(`iter') seed($seed) fixlevels(1 3) force : reg `y'  i.Treat_Ads##c.zz ss* 
matrix pexact[1,2]=r(p) 

ritest Treat_Ads _b[3.Treat_Ads#c.zz] , strata(strata) cluster(ID) reps(`iter') seed($seed) fixlevels(1 2) force : reg `y'  i.Treat_Ads##c.zz ss* 
matrix pexact[1,3]=r(p) 

mat colnames pexact = 1.Treat_Ads#c.zz 2.Treat_Ads#c.zz 3.Treat_Ads#c.zz 
est restore B`i'
estadd matrix pexact = pexact
sum `y' if T_FB==0
estadd  scalar c_mean = r(mean)


* Panel C: Letter - No Letter

matrix pexact=J(1,2,.) 


eststo C`i': reg `y'  i.Treat_Letter##c.zz  ss*  , cluster(ID)
estadd  scalar n_mun = e(N_clust)
test 1.Treat_Letter#c.zz =2.Treat_Letter#c.zz 
estadd scalar p_diff1=r(p) 


ritest Treat_Letter _b[1.Treat_Letter#c.zz] , strata(strata) cluster(ID) reps(`iter') seed($seed) fixlevels(2) force: reg `y'  i.Treat_Letter##c.zz ss* 
matrix pexact[1,1]=r(p) 

ritest Treat_Letter _b[2.Treat_Letter#c.zz] , strata(strata) cluster(ID) reps(`iter') seed($seed) fixlevels(1) force : reg `y'  i.Treat_Letter##c.zz ss* 
matrix pexact[1,2]=r(p) 

mat colnames pexact = 1.Treat_Letter#c.zz 2.Treat_Letter#c.zz 
est restore C`i'
estadd matrix pexact = pexact
sum `y' if T_FB==0
estadd  scalar c_mean = r(mean)

drop *zz*
}
}


#d ;
estout A* using "$out/elections_table_nocontrols_A.tex",  replace style(tex)
varlabels(1.T_FB "$\left[T\right]$ Any treatment" 1.T_FB#c.zz "$\left[T\times Z \right]$ Any treatment $\times Z$",elist(1.T_FB#c.zz \midrule))
cells(b(star fmt(%9.3f))  se(par fmt(%9.3f)) pexact(par([ ]) fmt(%9.3f)))  
keep(1.T_FB#c.zz) order(1.T_FB#c.zz)
collabels(none) stardrop(*) nolabel  mlabels(none);
#d cr

#d ;
estout B* using "$out/elections_table_nocontrols_B.tex",  replace style(tex)
varlabels(1.Treat_Ads "$\left[IA\right]$ Information Ad" 2.Treat_Ads "$\left[CA\right]$ Call-to-Action Ad" 3.Treat_Ads "$\left[I+CA\right]$ Info + Call-to-Action Ad"
1.Treat_Ads#c.zz "$\left[IA\times Z \right]$ Information Ad $\times Z$" 2.Treat_Ads#c.zz "$\left[CA \times Z \right]$ Call-to-Action Ad $\times Z$" 3.Treat_Ads#c.zz "$\left[I+CA \times Z \right]$ Info + Call-to-Action Ad $\times Z$"
,elist(3.Treat_Ads#c.zz \midrule))
cells(b(star fmt(%9.3f))  se(par fmt(%9.3f)) pexact(par([ ]) fmt(%9.3f)))  keep(1.Treat_Ads#c.zz 2.Treat_Ads#c.zz 3.Treat_Ads#c.zz) 
order(1.Treat_Ads#c.zz 2.Treat_Ads#c.zz 3.Treat_Ads#c.zz) stats( p_diff_az p_diff_bz p_diff_cz , fmt( %9.2f %9.2f %9.2f)
labels("Test $ IA \times Z=CA \times Z$, p-value" "Test $ IA \times Z=I+CA \times Z$, p-value" "Test $ CA \times Z=I+CA \times Z$, p-value"))
collabels(none) stardrop(*) nolabel  mlabels(none);
#d cr


#d ;
estout C* using "$out/elections_table_nocontrols_C.tex",  replace style(tex)
varlabels(1.Treat_Letter#c.zz "$\left[NL\times Z \right]$ No Letter - Any Ad $\times Z$" 2.Treat_Letter#c.zz "$\left[L\times Z \right]$ Letter - Any Ad $\times Z$" 
1.Treat_Letter "$\left[NL \right]$ No Letter - Any Ad" 2.Treat_Letter "$\left[L \right]$ Letter - Any Ad " ,elist(2.Treat_Letter#c.zz \midrule))
cells(b(star fmt(%9.3f))  se(par fmt(%9.3f)) pexact(par([ ]) fmt(%9.3f)))  keep( 1.Treat_Letter#c.zz  2.Treat_Letter#c.zz) 
order(1.Treat_Letter 2.Treat_Letter 1.Treat_Letter#c.zz 2.Treat_Letter#c.zz) stats( p_diff1 j c_mean N n_mun , fmt( %9.2f %9.2f %9.2f  %9.0f %9.0f)
labels("Test $ NL \times Z=L \times Z$, p-value" "\midrule" "Control Mean" "Sample Size" "N. Municipalities"))
collabels(none) stardrop(*) nolabel  mlabels(none);
#d cr

est clear


/*===============================================================================================
					Bootstrap P-Values for Table A20
===============================================================================================*/

if $confidential == 1 {
do "$analysis_code/aer_wildbootstrap_no_controls.do"
}


/*===============================================================================================
=================================================================================================
						Table A21: Long regression - Impacts on Reports
=================================================================================================
===============================================================================================*/

use "$final_data/main_data_municipal_level.dta", clear 


label define alabel 1 "Control" 2 "Info + No Letter" 3 "CtA + No Letter" 4 "Info + CtA + No Letter" 5 "Info + Partial K. Letter" 6 "CtA + Partial K. Letter"  7 "Info + CtA + Partial K. Letter" 8 "Info + Full K. Letter" 9 "CtA + Full K. Letter"  10 "Info + CtA + Full K. Letter"
label value _assign alabel
ren _assignment assignment

global outcomes "d_reportMOE_any reportMOE_any d_reportMOE_any_quality reportMOE_any_quality"


foreach y of global outcomes{

matrix pexact=J(1,9,.) 

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

local i=`i'+1

eststo A`i': reg `y' i.assignment `vSel', r


test 2.assignment=3.assignment=4.assignment=5.assignment=6.assignment=7.assignment=8.assignment=9.assignment=10.assignment
estadd scalar p_diff_T_FB=r(p) 
test 2.assignment=3.assignment=4.assignment
estadd scalar p_diff_No_Letter=r(p) 
test 5.assignment=6.assignment=7.assignment=8.assignment=9.assignment=10.assignment
estadd scalar p_diff_Letter=r(p) 
test 5.assignment=6.assignment=7.assignment
estadd scalar p_diff_PK_Letter=r(p) 
test 8.assignment=9.assignment=10.assignment
estadd scalar p_diff_FK_Letter=r(p) 
test 2.assignment=5.assignment=8.assignment
estadd scalar p_diff_Info=r(p) 
test 3.assignment=6.assignment=9.assignment
estadd scalar p_diff_CtA=r(p) 
test 4.assignment=7.assignment=10.assignment
estadd scalar p_diff_Info_CtA=r(p) 

ritest assignment _b[2.assignment] , strata(strata) reps(`iter') seed($seed) fixlevels(3 4 5 6 7 8 9 10) force: reg `y' i.assignment `vSel'
matrix pexact[1,1]=r(p) 

ritest assignment _b[3.assignment] , strata(strata) reps(`iter') seed($seed) fixlevels(2 4 5 6 7 8 9 10) force: reg `y' i.assignment `vSel'
matrix pexact[1,2]=r(p) 

ritest assignment _b[4.assignment] , strata(strata) reps(`iter') seed($seed) fixlevels(2 3 5 6 7 8 9 10) force: reg `y' i.assignment `vSel'
matrix pexact[1,3]=r(p) 

ritest assignment _b[5.assignment] , strata(strata) reps(`iter') seed($seed) fixlevels(2 3 4 6 7 8 9 10) force: reg `y' i.assignment `vSel'
matrix pexact[1,4]=r(p) 

ritest assignment _b[6.assignment] , strata(strata) reps(`iter') seed($seed) fixlevels(2 3 4 5 7 8 9 10) force: reg `y' i.assignment `vSel'
matrix pexact[1,5]=r(p) 

ritest assignment _b[7.assignment] , strata(strata) reps(`iter') seed($seed) fixlevels(2 3 4 5 6 8 9 10) force: reg `y' i.assignment `vSel'
matrix pexact[1,6]=r(p) 

ritest assignment _b[8.assignment] , strata(strata) reps(`iter') seed($seed) fixlevels(2 3 4 5 6 7 9 10) force: reg `y' i.assignment `vSel'
matrix pexact[1,7]=r(p) 

ritest assignment _b[9.assignment] , strata(strata) reps(`iter') seed($seed) fixlevels(2 3 4 5 6 7 8 10) force: reg `y' i.assignment `vSel'
matrix pexact[1,8]=r(p) 

ritest assignment _b[10.assignment] , strata(strata) reps(`iter') seed($seed) fixlevels(2 3 4 5 6 7 8 9) force: reg `y' i.assignment `vSel'
matrix pexact[1,9]=r(p) 

mat colnames pexact = 2.assignment 3.assignment 4.assignment 5.assignment 6.assignment 7.assignment 8.assignment 9.assignment 10.assignment
est restore A`i'
estadd matrix pexact = pexact


sum `y' if T_FB==0
estadd  scalar c_mean = r(mean)
}

#d ;
estout A* using "$out/long_reg_MOEreports.tex",  replace style(tex)
cells(b(star fmt(%9.3f))  se(par fmt(%9.3f)) pexact(par([ ]) fmt(%9.3f)) )  
keep(2.assignment 3.assignment 4.assignment 5.assignment 6.assignment 7.assignment 8.assignment 9.assignment 10.assignment) 
stats( j p_diff_T_FB p_diff_Info p_diff_CtA p_diff_Info_CtA p_diff_No_Letter p_diff_Letter p_diff_PK_Letter p_diff_FK_Letter j c_mean N,
 fmt(%9.2f %9.2f %9.2f %9.2f %9.2f %9.2f %9.2f %9.2f %9.2f %9.2f %9.2f %9.0f)
labels("\midrule" "Test: Any treatment is equal, p-value" "Test: Info treatments are equal, p-value" "Test: CtA treatments are equal, p-value" "Test: Info + CtA treatments are equal, p-value"
 "Test: No Letter treatments are equal, p-value" "Test: Letter treatments are equal, p-value" "Test: Partial K. Letter treatments are equal, p-value" 
 "Test: Full K. Letter treatments are equal, p-value" "\midrule" "Control Mean" "Sample Size"))
collabels(none) stardrop(*) label mlabels(none);
#d cr
est clear

/*===============================================================================================
=================================================================================================
						Table A22: Long regression - Impacts on Irregularities
=================================================================================================
===============================================================================================*/

use "$final_data/main_data_municipal_level.dta", clear


label define alabel 1 "Control" 2 "Info + No Letter" 3 "CtA + No Letter" 4 "Info + CtA + No Letter" 5 "Info + Partial K. Letter" 6 "CtA + Partial K. Letter"  7 "Info + CtA + Partial K. Letter" 8 "Info + Full K. Letter" 9 "CtA + Full K. Letter"  10 "Info + CtA + Full K. Letter"
label value _assign alabel
ren _assignment assignment

global outcomes "d_media_irreg media_irreg z_index_second sig95_max_second"


foreach y of global outcomes{

matrix pexact=J(1,9,.) 

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

local i=`i'+1

eststo A`i': reg `y' i.assignment `vSel', r



test 2.assignment=3.assignment=4.assignment=5.assignment=6.assignment=7.assignment=8.assignment=9.assignment=10.assignment
estadd scalar p_diff_T_FB=r(p) 
test 2.assignment=3.assignment=4.assignment
estadd scalar p_diff_No_Letter=r(p) 
test 5.assignment=6.assignment=7.assignment=8.assignment=9.assignment=10.assignment
estadd scalar p_diff_Letter=r(p) 
test 5.assignment=6.assignment=7.assignment
estadd scalar p_diff_PK_Letter=r(p) 
test 8.assignment=9.assignment=10.assignment
estadd scalar p_diff_FK_Letter=r(p) 
test 2.assignment=5.assignment=8.assignment
estadd scalar p_diff_Info=r(p) 
test 3.assignment=6.assignment=9.assignment
estadd scalar p_diff_CtA=r(p) 
test 4.assignment=7.assignment=10.assignment
estadd scalar p_diff_Info_CtA=r(p) 

ritest assignment _b[2.assignment] , strata(strata) reps(`iter') seed($seed) fixlevels(3 4 5 6 7 8 9 10) force: reg `y' i.assignment `vSel'
matrix pexact[1,1]=r(p) 

ritest assignment _b[3.assignment] , strata(strata) reps(`iter') seed($seed) fixlevels(2 4 5 6 7 8 9 10) force: reg `y' i.assignment `vSel'
matrix pexact[1,2]=r(p) 

ritest assignment _b[4.assignment] , strata(strata) reps(`iter') seed($seed) fixlevels(2 3 5 6 7 8 9 10) force: reg `y' i.assignment `vSel'
matrix pexact[1,3]=r(p) 

ritest assignment _b[5.assignment] , strata(strata) reps(`iter') seed($seed) fixlevels(2 3 4 6 7 8 9 10) force: reg `y' i.assignment `vSel'
matrix pexact[1,4]=r(p) 

ritest assignment _b[6.assignment] , strata(strata) reps(`iter') seed($seed) fixlevels(2 3 4 5 7 8 9 10) force: reg `y' i.assignment `vSel'
matrix pexact[1,5]=r(p) 

ritest assignment _b[7.assignment] , strata(strata) reps(`iter') seed($seed) fixlevels(2 3 4 5 6 8 9 10) force: reg `y' i.assignment `vSel'
matrix pexact[1,6]=r(p) 

ritest assignment _b[8.assignment] , strata(strata) reps(`iter') seed($seed) fixlevels(2 3 4 5 6 7 9 10) force: reg `y' i.assignment `vSel'
matrix pexact[1,7]=r(p) 

ritest assignment _b[9.assignment] , strata(strata) reps(`iter') seed($seed) fixlevels(2 3 4 5 6 7 8 10) force: reg `y' i.assignment `vSel'
matrix pexact[1,8]=r(p) 

ritest assignment _b[10.assignment] , strata(strata) reps(`iter') seed($seed) fixlevels(2 3 4 5 6 7 8 9) force: reg `y' i.assignment `vSel'
matrix pexact[1,9]=r(p) 

mat colnames pexact = 2.assignment 3.assignment 4.assignment 5.assignment 6.assignment 7.assignment 8.assignment 9.assignment 10.assignment
est restore A`i'
estadd matrix pexact = pexact


sum `y' if T_FB==0
estadd  scalar c_mean = r(mean)
}

#d ;
estout A* using "$out/long_reg_irregularities.tex",  replace style(tex)
cells(b(star fmt(%9.3f))  se(par fmt(%9.3f)) pexact(par([ ]) fmt(%9.3f)) )  
keep(2.assignment 3.assignment 4.assignment 5.assignment 6.assignment 7.assignment 8.assignment 9.assignment 10.assignment) 
stats( j p_diff_T_FB p_diff_Info p_diff_CtA p_diff_Info_CtA p_diff_No_Letter p_diff_Letter p_diff_PK_Letter p_diff_FK_Letter j c_mean N,
 fmt(%9.2f %9.2f %9.2f %9.2f %9.2f %9.2f %9.2f %9.2f %9.2f %9.2f %9.2f %9.0f)
labels("\midrule" "Test: Any treatment is equal, p-value" "Test: Info treatments are equal, p-value" "Test: CtA treatments are equal, p-value" "Test: Info + CtA treatments are equal, p-value"
 "Test: No Letter treatments are equal, p-value" "Test: Letter treatments are equal, p-value" "Test: Partial K. Letter treatments are equal, p-value" 
 "Test: Full K. Letter treatments are equal, p-value" "\midrule" "Control Mean" "Sample Size"))
collabels(none) stardrop(*) label mlabels(none);
#d cr
est clear

/*===============================================================================================
=================================================================================================
				Table A23: Long regression - Impacts on Vote Share of Candidates
								Likely to Engage in Irregularities
=================================================================================================
===============================================================================================*/

use "$final_data/main_data_candidate_level.dta", clear


label define alabel 1 "Control" 2 "Info + No Letter" 3 "CtA + No Letter" 4 "Info + CtA + No Letter" 5 "Info + Partial K. Letter" 6 "CtA + Partial K. Letter"  7 "Info + CtA + Partial K. Letter" 8 "Info + Full K. Letter" 9 "CtA + Full K. Letter"  10 "Info + CtA + Full K. Letter"
label value _assign alabel
ren _assignment assignment


global outcomes "pct_vote"
global interactions "p_d_misdeed d_mean_p_d_misdeed above_mean_p_d_misdeed"


foreach y of global outcomes{ 

foreach z of global interactions{

matrix pexact=J(1,9,.)


gen zz=`z'

quietly: rlasso `y'  $controls_can , cluster(ID)
local ySel `e(selected)'

quietly: rlasso T_FB  $controls_can , cluster(ID)
local xSel0 `e(selected)' 

quietly: rlasso TT_FB  $controls_can , cluster(ID)
local xSel1 `e(selected)' 

quietly: rlasso T_FB_call  $controls_can , cluster(ID)
local xSel2 `e(selected)' 

quietly: rlasso T_FB_info  $controls_can , cluster(ID)
local xSel3 `e(selected)' 

quietly: rlasso T_FB_both  $controls_can , cluster(ID)
local xSel4 `e(selected)' 

quietly: rlasso T_Letter  $controls_can , cluster(ID)
local xSel5 `e(selected)'

quietly: rlasso T_Letter_no_sj  $controls_can , cluster(ID)
local xSel6 `e(selected)' 

quietly: rlasso T_Letter_sj  $controls_can , cluster(ID)
local xSel7 `e(selected)' 


local vSel : list ySel | xSel0
local vSel : list vSel | xSel1
local vSel : list vSel | xSel2
local vSel : list vSel | xSel3
local vSel : list vSel | xSel4
local vSel : list vSel | xSel5
local vSel : list vSel | xSel6
local vSel : list vSel | xSel7



local i=`i'+1

eststo A`i': reg `y'  i.assign##c.zz `vSel' , cluster(ID)
estadd  scalar n_mun = e(N_clust)

test 2.assignment#c.zz=3.assignment#c.zz=4.assignment#c.zz=5.assignment#c.zz=6.assignment#c.zz=7.assignment#c.zz=8.assignment#c.zz=9.assignment#c.zz=10.assignment#c.zz
estadd scalar p_diff_T_FB=r(p) 
test 2.assignment#c.zz=3.assignment#c.zz=4.assignment#c.zz
estadd scalar p_diff_No_Letter=r(p) 
test 5.assignment#c.zz=6.assignment#c.zz=7.assignment#c.zz=8.assignment#c.zz=9.assignment#c.zz=10.assignment#c.zz
estadd scalar p_diff_Letter=r(p) 
test 5.assignment#c.zz=6.assignment#c.zz=7.assignment#c.zz
estadd scalar p_diff_PK_Letter=r(p) 
test 8.assignment#c.zz=9.assignment#c.zz=10.assignment#c.zz
estadd scalar p_diff_FK_Letter=r(p) 
test 2.assignment#c.zz=5.assignment#c.zz=8.assignment#c.zz
estadd scalar p_diff_Info=r(p) 
test 3.assignment#c.zz=6.assignment#c.zz=9.assignment#c.zz
estadd scalar p_diff_CtA=r(p) 
test 4.assignment#c.zz=7.assignment#c.zz=10.assignment#c.zz
estadd scalar p_diff_Info_CtA=r(p) 

ritest assignment _b[2.assignment#c.zz] , strata(strata) reps(`iter') seed($seed) fixlevels(3 4 5 6 7 8 9 10) force: reg `y' i.assignment##c.zz `vSel'
matrix pexact[1,1]=r(p) 

ritest assignment _b[3.assignment#c.zz] , strata(strata) reps(`iter') seed($seed) fixlevels(2 4 5 6 7 8 9 10) force: reg `y' i.assignment##c.zz `vSel'
matrix pexact[1,2]=r(p) 

ritest assignment _b[4.assignment#c.zz] , strata(strata) reps(`iter') seed($seed) fixlevels(2 3 5 6 7 8 9 10) force: reg `y' i.assignment##c.zz `vSel'
matrix pexact[1,3]=r(p) 

ritest assignment _b[5.assignment#c.zz] , strata(strata) reps(`iter') seed($seed) fixlevels(2 3 4 6 7 8 9 10) force: reg `y' i.assignment##c.zz `vSel'
matrix pexact[1,4]=r(p) 

ritest assignment _b[6.assignment#c.zz] , strata(strata) reps(`iter') seed($seed) fixlevels(2 3 4 5 7 8 9 10) force: reg `y' i.assignment##c.zz `vSel'
matrix pexact[1,5]=r(p) 

ritest assignment _b[7.assignment#c.zz] , strata(strata) reps(`iter') seed($seed) fixlevels(2 3 4 5 6 8 9 10) force: reg `y' i.assignment##c.zz `vSel'
matrix pexact[1,6]=r(p) 

ritest assignment _b[8.assignment#c.zz] , strata(strata) reps(`iter') seed($seed) fixlevels(2 3 4 5 6 7 9 10) force: reg `y' i.assignment##c.zz `vSel'
matrix pexact[1,7]=r(p) 

ritest assignment _b[9.assignment#c.zz] , strata(strata) reps(`iter') seed($seed) fixlevels(2 3 4 5 6 7 8 10) force: reg `y' i.assignment##c.zz `vSel'
matrix pexact[1,8]=r(p) 

ritest assignment _b[10.assignment#c.zz] , strata(strata) reps(`iter') seed($seed) fixlevels(2 3 4 5 6 7 8 9) force: reg `y' i.assignment##c.zz `vSel'
matrix pexact[1,9]=r(p) 

mat colnames pexact = 2.assignment#c.zz 3.assignment#c.zz 4.assignment#c.zz 5.assignment#c.zz 6.assignment#c.zz 7.assignment#c.zz 8.assignment#c.zz 9.assignment#c.zz 10.assignment#c.zz
est restore A`i'
estadd matrix pexact = pexact

sum `y' if T_FB==0
estadd  scalar c_mean = r(mean)
drop *zz*
}
}


#d ;
estout A* using "$out/long_reg_candidates.tex",  replace style(tex)
cells(b(star fmt(%9.3f))  se(par fmt(%9.3f)) pexact(par([ ]) fmt(%9.3f)) )  
keep(2.assignment#c.zz 3.assignment#c.zz 4.assignment#c.zz 5.assignment#c.zz 6.assignment#c.zz 7.assignment#c.zz 8.assignment#c.zz 9.assignment#c.zz 10.assignment#c.zz)
 
varlabels(2.assignment#c.zz "Info + No Letter $\times Z$" 3.assignment#c.zz "CtA + No Letter $\times Z$" 4.assignment#c.zz "Info + CtA + No Letter $\times Z$" 
5.assignment#c.zz "Info + Partial K. Letter $\times Z$" 6.assignment#c.zz "CtA + Partial K. Letter $\times Z$" 7.assignment#c.zz "Info + CtA + Partial K. Letter $\times Z$" 
8.assignment#c.zz "Info + Full K. Letter $\times Z$" 9.assignment#c.zz "CtA + Full K. Letter $\times Z$" 10.assignment#c.zz "Info + CtA + Full K. Letter $\times Z$" )
stats( j p_diff_T_FB p_diff_Info p_diff_CtA p_diff_Info_CtA p_diff_No_Letter p_diff_Letter p_diff_PK_Letter p_diff_FK_Letter j c_mean N n_mun,
 fmt(%9.2f %9.2f %9.2f %9.2f %9.2f %9.2f %9.2f %9.2f %9.2f %9.2f %9.2f %9.0f %9.0f)
labels("\midrule" "Test: Any treatment is equal, p-value" "Test: Info treatments are equal, p-value" "Test: CtA treatments are equal, p-value" "Test: Info + CtA treatments are equal, p-value"
 "Test: No Letter treatments are equal, p-value" "Test: Letter treatments are equal, p-value" "Test: Partial K. Letter treatments are equal, p-value" 
 "Test: Full K. Letter treatments are equal, p-value" "\midrule" "Control Mean" "Sample Size" "N. Municipalities"))
collabels(none) stardrop(*) label mlabels(none);
#d cr
est clear


/*===============================================================================================
=================================================================================================
			   Table A28: Balance on Post-Treat Survey Respondent Characteristics
=================================================================================================
===============================================================================================*/

if $confidential == 1 {

use "$intermediate_data/post_svy_respondent_vars_clean.dta", clear

* Merging with treatment indicators:

merge n:1 ID using "$intermediate_data/treatment_indicators.dta"
drop _m 


label var  female_post_svy "Female (=1)"
label var  birth_year_post_svy "Age"
label var  high_school_post_svy "High School or Less (=1)"
label var  high_school_more_post_svy "More than High School (=1)"
label var no_resp_likelihood_misdeed_1 "V. Intimidation - No Response (=1)"
label var no_resp_likelihood_misdeed_2 "Vote Buying - No Response (=1)"
label var no_resp_likelihood_misdeed_3 "Registr. Fraud - No Response (=1)"
label var no_resp_likelihood_misdeed_4 "Elect. Fraud - No Response (=1)"
label var no_resp_likelihood_misdeed_5 "Public Campaign. - No Response (=1)"
label var no_resp_likelihood_misdeed_6 "Illicit Advert. - No Response (=1)"

** Balance table:

global controlsdem_post "female_post_svy birth_year_post_svy  high_school_post_svy high_school_more_post_svy no_resp_likelihood_misdeed_1 no_resp_likelihood_misdeed_2 no_resp_likelihood_misdeed_3 no_resp_likelihood_misdeed_4 no_resp_likelihood_misdeed_5 no_resp_likelihood_misdeed_6 "


foreach y of global controlsdem_post{

matrix mean=J(3,1,.)
matrix A=J(1,24,.)

* 1. Control Mean
reg `y' if T_FB==0, r
matrix mean[1,1]=_b[_cons]
matrix rownames A= "`: var label `y''"

* 2. Any Treatment vs. Control
reg `y' T_FB  , r
matrix A[1,1]=_b[T_FB]
matrix A[1,2]=_se[T_FB]
ritest T_FB _b[T_FB], strata(strata) reps(`iter') seed($seed) force: reg `y' T_FB
matrix A[1,3]=r(p) 
			

* 3. Information vs. Control
reg `y' T_FB_info  if T_FB_call==0 & T_FB_both==0, r
matrix A[1,4]=_b[T_FB_info]
matrix A[1,5]=_se[T_FB_info]
ritest T_FB_info _b[T_FB_info], strata(strata) reps(`iter') seed($seed) force: reg `y' T_FB_info  if T_FB_call==0 & T_FB_both==0
matrix A[1,6]=r(p) 	

* 4. Call-to-action vs. Control
reg `y' T_FB_call  if T_FB_info==0 & T_FB_both==0, r
matrix A[1,7]=_b[T_FB_call]
matrix A[1,8]=_se[T_FB_call]
ritest T_FB_call _b[T_FB_call], strata(strata) reps(`iter') seed($seed) force: reg `y' T_FB_call  if T_FB_info==0 & T_FB_both==0
matrix A[1,9]=r(p) 	

* 5. Info + Call-to-action vs. Control
reg `y' T_FB_both  if T_FB_call==0 & T_FB_info==0, r
matrix A[1,10]=_b[T_FB_both]
matrix A[1,11]=_se[T_FB_both]
ritest T_FB_both _b[T_FB_both], strata(strata) reps(`iter') seed($seed) force: reg `y' T_FB_both  if T_FB_call==0 & T_FB_info==0
matrix A[1,12]=r(p) 	
	
* 6. Any Letter vs. No Letter
reg `y' T_Letter  if T_FB==1, r
matrix A[1,13]=_b[T_Letter]
matrix A[1,14]=_se[T_Letter]
ritest T_Letter _b[T_Letter], strata(strata) reps(`iter') seed($seed) force: reg `y'  T_Letter  if T_FB==1
matrix A[1,15]=r(p) 

* 7. Letter P. Knowledge vs. No Letter
reg `y' T_Letter_no_sj  if T_FB==1 & T_Letter_sj==0, r
matrix A[1,16]=_b[T_Letter_no_sj]
matrix A[1,17]=_se[T_Letter_no_sj]
ritest T_Letter_no_sj _b[T_Letter_no_sj], strata(strata) reps(`iter') seed($seed) force: reg `y' T_Letter_no_sj  if T_FB==1 & T_Letter_sj==0
matrix A[1,18]=r(p) 

* 8. Letter F. Knowledge vs. No Letter
reg `y' T_Letter_sj  if T_FB==1 & T_Letter_no_sj==0, r
matrix A[1,19]=_b[T_Letter_sj]
matrix A[1,20]=_se[T_Letter_sj]
ritest T_Letter_sj _b[T_Letter_sj], strata(strata) reps(`iter') seed($seed) force: reg `y' T_Letter_sj  if T_FB==1 & T_Letter_no_sj==0
matrix A[1,21]=r(p) 

* 9. Letter F. Knowledge vs. Letter P. Knowledge
reg `y' T_Letter_sj  if T_Letter==1 , r
matrix A[1,22]=_b[T_Letter_sj]
matrix A[1,23]=_se[T_Letter_sj]
ritest T_Letter_sj _b[T_Letter_sj], strata(strata) reps(`iter') seed($seed) force: reg `y' T_Letter_sj  if T_Letter==1 
matrix A[1,24]=r(p) 	

frmttable, statmat(mean) sdec(3) tex  replace store(mean)

frmttable using "$out_balance/balance_post_svy_`y'.tex", statmat(A) sdec(3) substat(2)  tex  replace merge(mean) fragment


preserve
clear
set obs 1
local tex "$out_balance/balance_post_svy_`y'.tex"


generate strL s = fileread("`tex'") if fileexists("`tex'")
assert filereaderror(s)==0
replace s = subinstr(s,"\begin{center}","",1)   
replace s = subinstr(s,"\begin{tabular}{lccccccccc}","",1)   
replace s = subinstr(s,"\hline \noalign{\smallskip}","",1)   
replace s = subinstr(s,"\noalign{\smallskip}\hline\end{tabular}\\","",1)   
replace s = subinstr(s,"\end{center}","",1)   
gen byte fw = filewrite("`tex'",s,1)
restore
}
 
}
 
/*===============================================================================================
=================================================================================================
					Table A29: Covariate Balance - Post-Treatment Survey
=================================================================================================
===============================================================================================*/

use "$final_data/main_data_municipal_level.dta", clear 

* Keeping only municipalities with post-treat survey responses:
keep if z_misdeeds_index!=.

label var  n_respuestas_post "N Responses Post-Treat Survey"

global controls_balance_post_survey "`political'  `violence' `development'  `other' rr1 rr2 rr3 rr4 rr5 rr6 n_respuestas_pre pct_Reach Reach no_Reach n_respuestas_post"


foreach y of global controls_balance_post_survey{

matrix mean=J(3,1,.)
matrix A=J(1,24,.)

* 1. Control Mean
reg `y' if T_FB==0, r
matrix mean[1,1]=_b[_cons]
matrix rownames A= "`: var label `y''"

* 2. Any Treatment vs. Control
reg `y' T_FB  , r
matrix A[1,1]=_b[T_FB]
matrix A[1,2]=_se[T_FB]
ritest T_FB _b[T_FB], strata(strata) reps(`iter') seed($seed) force: reg `y' T_FB
matrix A[1,3]=r(p) 
			

* 3. Information vs. Control
reg `y' T_FB_info  if T_FB_call==0 & T_FB_both==0, r
matrix A[1,4]=_b[T_FB_info]
matrix A[1,5]=_se[T_FB_info]
ritest T_FB_info _b[T_FB_info], strata(strata) reps(`iter') seed($seed) force: reg `y' T_FB_info  if T_FB_call==0 & T_FB_both==0
matrix A[1,6]=r(p) 	

* 4. Call-to-action vs. Control
reg `y' T_FB_call  if T_FB_info==0 & T_FB_both==0, r
matrix A[1,7]=_b[T_FB_call]
matrix A[1,8]=_se[T_FB_call]
ritest T_FB_call _b[T_FB_call], strata(strata) reps(`iter') seed($seed) force: reg `y' T_FB_call  if T_FB_info==0 & T_FB_both==0
matrix A[1,9]=r(p) 	

* 5. Info + Call-to-action vs. Control
reg `y' T_FB_both  if T_FB_call==0 & T_FB_info==0, r
matrix A[1,10]=_b[T_FB_both]
matrix A[1,11]=_se[T_FB_both]
ritest T_FB_both _b[T_FB_both], strata(strata) reps(`iter') seed($seed) force: reg `y' T_FB_both  if T_FB_call==0 & T_FB_info==0
matrix A[1,12]=r(p) 	
	
* 6. Any Letter vs. No Letter
reg `y' T_Letter  if T_FB==1, r
matrix A[1,13]=_b[T_Letter]
matrix A[1,14]=_se[T_Letter]
ritest T_Letter _b[T_Letter], strata(strata) reps(`iter') seed($seed) force: reg `y'  T_Letter  if T_FB==1
matrix A[1,15]=r(p) 

* 7. Letter P. Knowledge vs. No Letter
reg `y' T_Letter_no_sj  if T_FB==1 & T_Letter_sj==0, r
matrix A[1,16]=_b[T_Letter_no_sj]
matrix A[1,17]=_se[T_Letter_no_sj]
ritest T_Letter_no_sj _b[T_Letter_no_sj], strata(strata) reps(`iter') seed($seed) force: reg `y' T_Letter_no_sj  if T_FB==1 & T_Letter_sj==0
matrix A[1,18]=r(p) 

* 8. Letter F. Knowledge vs. No Letter
reg `y' T_Letter_sj  if T_FB==1 & T_Letter_no_sj==0, r
matrix A[1,19]=_b[T_Letter_sj]
matrix A[1,20]=_se[T_Letter_sj]
ritest T_Letter_sj _b[T_Letter_sj], strata(strata) reps(`iter') seed($seed) force: reg `y' T_Letter_sj  if T_FB==1 & T_Letter_no_sj==0
matrix A[1,21]=r(p) 

* 9. Letter F. Knowledge vs. Letter P. Knowledge
reg `y' T_Letter_sj  if T_Letter==1 , r
matrix A[1,22]=_b[T_Letter_sj]
matrix A[1,23]=_se[T_Letter_sj]
ritest T_Letter_sj _b[T_Letter_sj], strata(strata) reps(`iter') seed($seed) force: reg `y' T_Letter_sj  if T_Letter==1 
matrix A[1,24]=r(p) 	

frmttable, statmat(mean) sdec(3) tex  replace store(mean)

frmttable using "$out_balance/balance_post_svy_`y'.tex", statmat(A) sdec(3) substat(2)  tex  replace merge(mean) fragment


preserve
clear
set obs 1
local tex "$out_balance/balance_post_svy_`y'.tex"


generate strL s = fileread("`tex'") if fileexists("`tex'")
assert filereaderror(s)==0
replace s = subinstr(s,"\begin{center}","",1)   
replace s = subinstr(s,"\begin{tabular}{lccccccccc}","",1)   
replace s = subinstr(s,"\hline \noalign{\smallskip}","",1)   
replace s = subinstr(s,"\noalign{\smallskip}\hline\end{tabular}\\","",1)   
replace s = subinstr(s,"\end{center}","",1)   
gen byte fw = filewrite("`tex'",s,1)
restore
}

/*===============================================================================================
=================================================================================================
			   Table A30: Impacts on Survey-Based Irregularity Measures
=================================================================================================
===============================================================================================*/

use "$final_data/main_data_municipal_level.dta", clear 

global outcomes "z_misdeeds_index z_likelihood_misdeed_2 z_likelihood_misdeed_1 z_likelihood_misdeed_3 z_likelihood_misdeed_5 z_likelihood_misdeed_4 z_likelihood_misdeed_6"

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


* Panel A: Any Treatment

matrix pexact=J(1,1,.) 

local i=`i'+1

eststo A`i': reg `y'  T_FB `vSel' , cluster(ID)


ritest T_FB _b[T_FB], strata(strata) reps(`iter') seed($seed) force: reg `y' T_FB `vSel' 

matrix pexact[1,1]=r(p) 

mat colnames pexact = T_FB
est restore A`i'
estadd matrix pexact = pexact
sum `y' if T_FB==0
estadd  scalar c_mean = r(mean)


* Panel B: Different Ads

matrix pexact=J(1,3,.) 

eststo B`i': reg `y' i.Treat_Ads  `vSel' , cluster(ID)

test 1.Treat_Ads=2.Treat_Ads
estadd scalar p_diff_a=r(p) 

test 1.Treat_Ads=3.Treat_Ads
estadd scalar p_diff_b=r(p) 

test 3.Treat_Ads=2.Treat_Ads
estadd scalar p_diff_c=r(p) 

ritest Treat_Ads _b[1.Treat_Ads], strata(strata) reps(`iter') seed($seed) fixlevels(2 3) force: reg `y' i.Treat_Ads `vSel' 
matrix pexact[1,1]=r(p) 

ritest Treat_Ads _b[2.Treat_Ads], strata(strata) reps(`iter') seed($seed) fixlevels(1 3) force: reg `y' i.Treat_Ads `vSel' 
matrix pexact[1,2]=r(p) 

ritest Treat_Ads _b[3.Treat_Ads], strata(strata) reps(`iter') seed($seed) fixlevels(1 2) force: reg `y' i.Treat_Ads `vSel' 
matrix pexact[1,3]=r(p) 


mat colnames pexact = 1.Treat_Ads 2.Treat_Ads 3.Treat_Ads
est restore B`i'
estadd matrix pexact = pexact
sum `y' if T_FB==0
estadd  scalar c_mean = r(mean)


* Panel C: Letter - No Letter

matrix pexact=J(1,2,.) 


eststo C`i': reg `y'  i.Treat_Letter `vSel' , cluster(ID)
test 1.Treat_Letter=2.Treat_Letter
estadd scalar p_diff=r(p) 


ritest Treat_Letter _b[1.Treat_Letter] , strata(strata) reps(`iter') seed($seed) fixlevels(2) force: reg `y'  i.Treat_Letter `vSel' 
matrix pexact[1,1]=r(p) 

ritest Treat_Letter _b[2.Treat_Letter] , strata(strata) reps(`iter') seed($seed) fixlevels(1 ) force : reg `y'  i.Treat_Letter `vSel' 
matrix pexact[1,2]=r(p) 

mat colnames pexact = 1.Treat_Letter 2.Treat_Letter
est restore C`i'
estadd matrix pexact = pexact
sum `y' if T_FB==0
estadd  scalar c_mean = r(mean)

}


#d ;
estout A* using "$out/survey_irregs_table_A.tex",  replace style(tex)
varlabels(T_FB "$\left[T\right]$ Any treatment",elist(T_FB \midrule))
cells(b(star fmt(%9.3f))  se(par fmt(%9.3f)) pexact(par([ ]) fmt(%9.3f)))  
keep(T_FB) order(T_FB)
collabels(none) stardrop(*) nolabel  mlabels(none);
#d cr

#d ;
estout B* using "$out/survey_irregs_table_B.tex",  replace style(tex)
varlabels(1.Treat_Ads "$\left[IA\right]$ Information Ad" 2.Treat_Ads "$\left[CA\right]$ Call-to-Action Ad" 3.Treat_Ads "$\left[I+CA\right]$ Info + Call-to-Action Ad",elist(3.Treat_Ads \midrule))
cells(b(star fmt(%9.3f))  se(par fmt(%9.3f)) pexact(par([ ]) fmt(%9.3f)))  keep(1.Treat_Ads 2.Treat_Ads 3.Treat_Ads) 
order(1.Treat_Ads 2.Treat_Ads 3.Treat_Ads) stats(p_diff_a p_diff_b p_diff_c , fmt(%9.2f %9.2f %9.2f)
labels("Test $ IA=CA$, p-value" "Test $ IA=I+CA$, p-value" "Test $ CA=I+CA$, p-value"))
collabels(none) stardrop(*) nolabel  mlabels(none);
#d cr


#d ;
estout C* using "$out/survey_irregs_table_C.tex",  replace style(tex)
varlabels(1.Treat_Letter "$\left[NL\right]$ No Letter - Any Ad" 2.Treat_Letter "$\left[L\right]$ Letter - Any Ad" ,elist(2.Treat_Letter \midrule))
cells(b(star fmt(%9.3f))  se(par fmt(%9.3f)) pexact(par([ ]) fmt(%9.3f)))  keep(1.Treat_Letter 2.Treat_Letter) 
order(1.Treat_Letter 2.Treat_Letter) stats(p_diff j c_mean N, fmt(%9.2f %9.2f %9.2f %9.0f) 
labels("Test $ NL=L$, p-value" "\midrule" "Control Mean" "Sample Size"))
collabels(none) stardrop(*) nolabel  mlabels(none);
#d cr

est clear






