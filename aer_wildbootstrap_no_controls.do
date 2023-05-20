/*==================================================================================================
Project:       All Eyes on Them: A Field Experiment on Citizen Oversight and Electoral Integrity
Author:        Mateo Montenegro 
Purpose:       This code produces the bootstrap p-values described in Appendix D to account for the 
			   variance in estimating the candidate-level variables for Table A20 (without controls)
----------------------------------------------------------------------------------------------------
Index:		   A. Storing Estimated Coefficients from Unrestricted Model
			   B. Storing Estimated Coefficients from restricted Model
			   C. Bootstraping Procedure
===================================================================================================*/

est clear
set more off

local reps=2000 // Defines number of bootstrap draws 


/*===============================================================================================
                            A. Storing Estimated Coefficients from Unrestricted Model
===============================================================================================*/


use "$final_data/main_data_candidate_level.dta", clear
matrix w=J(9,3,.) 


global outcomes "pct_vote"
global interactions "p_d_misdeed d_mean_p_d_misdeed above_mean_p_d_misdeed"

local m=0
foreach y of global outcomes{

foreach z of global interactions{
local m=`m'+1
display `z'

gen zz=`z'



* Panel A: Any Treatment


reg `y'  i.T_FB##c.zz ss* , cluster(ID)
local G=e(N_clust)

matrix w[1,`m']=_b[1.T_FB#c.zz]/_se[1.T_FB#c.zz]

* Panel B: Different Ads

 reg `y'  i.Treat_Ads##c.zz  ss*  , cluster(ID)
 matrix w[2,`m']=_b[1.Treat_Ads#c.zz]/_se[1.Treat_Ads#c.zz]
 matrix w[3,`m']=_b[2.Treat_Ads#c.zz]/_se[2.Treat_Ads#c.zz]
 matrix w[4,`m']=_b[3.Treat_Ads#c.zz]/_se[3.Treat_Ads#c.zz]

* Panel C: Letter - No Letter


reg `y'  i.Treat_Letter##c.zz  ss*  , cluster(ID)
 matrix w[5,`m']=_b[1.Treat_Letter#c.zz]/_se[1.Treat_Letter#c.zz]
 matrix w[6,`m']=_b[2.Treat_Letter#c.zz]/_se[2.Treat_Letter#c.zz]


* Table A26: Different Letters


reg `y'  i.Treat_Letters##c.zz  ss*  , cluster(ID)
 matrix w[7,`m']=_b[1.Treat_Letters#c.zz]/_se[1.Treat_Letters#c.zz]
 matrix w[8,`m']=_b[2.Treat_Letters#c.zz]/_se[2.Treat_Letters#c.zz]
 matrix w[9,`m']=_b[3.Treat_Letters#c.zz]/_se[3.Treat_Letters#c.zz]

drop *zz*
}
}


/*===============================================================================================
                            B. Storing Estimated Coefficients from Restricted Model
===============================================================================================*/

matrix est=J(23,3,.) 

global outcomes "pct_vote"
global interactions "p_d_misdeed d_mean_p_d_misdeed above_mean_p_d_misdeed"

local m=0
foreach y of global outcomes{

foreach z of global interactions{
local m=`m'+1
display `z'

gen zz=`z'


* Panel A: Any Treatment


reg `y'  c.zz ss* , cluster(ID)

predict u1`m', resid
predict xb1`m', xb
replace xb1`m'=xb1`m'-_b[c.zz]*zz
matrix est[1,`m']=_b[c.zz]

* Panel B: Different Ads

 reg `y'  2.Treat_Ads##c.zz 3.Treat_Ads##c.zz  ss*  , cluster(ID)
 
predict u2`m', resid
predict xb2`m', xb
replace xb2`m'=xb2`m'-_b[c.zz]*zz -_b[2.Treat_Ads#c.zz]*zz*2.Treat_Ads-_b[3.Treat_Ads#c.zz]*zz*3.Treat_Ads
matrix est[2,`m']=_b[c.zz]
matrix est[3,`m']=_b[2.Treat_Ads#c.zz]
matrix est[4,`m']=_b[3.Treat_Ads#c.zz]

 reg `y'  1.Treat_Ads##c.zz 3.Treat_Ads##c.zz  ss*  , cluster(ID)
 
 predict u3`m', resid
predict xb3`m', xb
replace xb3`m'=xb3`m'-_b[c.zz]*zz -_b[1.Treat_Ads#c.zz]*zz*1.Treat_Ads-_b[3.Treat_Ads#c.zz]*zz*3.Treat_Ads
matrix est[5,`m']=_b[c.zz]
matrix est[6,`m']=_b[1.Treat_Ads#c.zz]
matrix est[7,`m']=_b[3.Treat_Ads#c.zz]


 reg `y'  1.Treat_Ads##c.zz 2.Treat_Ads##c.zz  ss*  , cluster(ID)
 
 predict u4`m', resid
predict xb4`m', xb
replace xb4`m'=xb4`m'-_b[c.zz]*zz -_b[1.Treat_Ads#c.zz]*zz*1.Treat_Ads-_b[2.Treat_Ads#c.zz]*zz*2.Treat_Ads
matrix est[8,`m']=_b[c.zz]
matrix est[9,`m']=_b[1.Treat_Ads#c.zz]
matrix est[10,`m']=_b[2.Treat_Ads#c.zz]

 
* Panel C: Letter - No Letter


reg `y'  2.Treat_Letter##c.zz   ss*  , cluster(ID)

predict u5`m', resid
predict xb5`m', xb
replace xb5`m'=xb5`m'-_b[c.zz]*zz -_b[2.Treat_Letter#c.zz ]*zz*2.Treat_Letter
matrix est[11,`m']=_b[c.zz]
matrix est[12,`m']=_b[2.Treat_Letter#c.zz]


reg `y'  1.Treat_Letter##c.zz  ss*  , cluster(ID)

predict u6`m', resid
predict xb6`m', xb
replace xb6`m'=xb6`m'-_b[c.zz]*zz -_b[1.Treat_Letter#c.zz ]*zz*1.Treat_Letter
matrix est[13,`m']=_b[c.zz]
matrix est[14,`m']=_b[1.Treat_Letter#c.zz]



* Table A26: Different Letters


reg `y'  2.Treat_Letters##c.zz 3.Treat_Letters##c.zz  ss*  , cluster(ID)

predict u7`m', resid
predict xb7`m', xb
replace xb7`m'=xb7`m'-_b[c.zz]*zz -_b[2.Treat_Letters#c.zz]*zz*2.Treat_Letters -_b[3.Treat_Letters#c.zz]*zz*3.Treat_Letters
matrix est[15,`m']=_b[c.zz]
matrix est[16,`m']=_b[2.Treat_Letters#c.zz]
matrix est[17,`m']=_b[3.Treat_Letters#c.zz]

reg `y'  1.Treat_Letters##c.zz 3.Treat_Letters##c.zz  ss*  , cluster(ID)

predict u8`m', resid
predict xb8`m', xb
replace xb8`m'=xb8`m'-_b[c.zz]*zz -_b[1.Treat_Letters#c.zz]*zz*1.Treat_Letters -_b[3.Treat_Letters#c.zz]*zz*3.Treat_Letters
matrix est[18,`m']=_b[c.zz]
matrix est[19,`m']=_b[1.Treat_Letters#c.zz]
matrix est[20,`m']=_b[3.Treat_Letters#c.zz]


reg `y'  1.Treat_Letters##c.zz 2.Treat_Letters##c.zz  ss*  , cluster(ID)

predict u9`m', resid
predict xb9`m', xb
replace xb9`m'=xb9`m'-_b[c.zz]*zz -_b[1.Treat_Letters#c.zz]*zz*1.Treat_Letters -_b[2.Treat_Letters#c.zz]*zz*2.Treat_Letters
matrix est[21,`m']=_b[c.zz]
matrix est[22,`m']=_b[1.Treat_Letters#c.zz]
matrix est[23,`m']=_b[2.Treat_Letters#c.zz]

drop *zz*
}
}


tempfile restrict
save `restrict', replace

/*===============================================================================================
                            C. Bootstraping Procedure
===============================================================================================*/

matrix pvalue1=J(`reps',9,.) 
matrix pvalue2=J(`reps',9,.) 
matrix pvalue3=J(`reps',9,.) 

forvalues j=1(1)`reps'{

use "$intermediate_data/pre_svy_bootstrap_data_clean.dta", clear

bsample, strata(candidate_id_)  

gen nn_resp=1

collapse (sum)  d_misdeed nn_resp (max) candidate_id , by(ID cannombre)


quietly:bys ID: gen pp_d_misdeed=d_misdeed/nn_resp

quietly:bys ID: egen mu_pp_d_misdeed=mean(pp_d_misdeed)
quietly:gen d_mean_pp_d_misdeed=pp_d_misdeed-mu_pp_d_misdeed
quietly:gen above_mean_pp_d_misdeed=(d_mean_pp_d>0) if d_mean_pp_d!=.


tempfile main
save `main'


quietly:merge n:1 ID cannombre using `restrict'
keep if _m==3

* Synthetic data:

local mammenp=((5^0.5)+1)/(2*(5^0.5))

quietly:bys ID: gen rand=(runiform()<`mammenp') if _n==1
quietly:bys ID: egen erand=max(rand)
local mammen=(1-(5^0.5))/2

global outcomes "pct_vote"
global interactions "pp_d_misdeed d_mean_pp_d_misdeed above_mean_pp_d_misdeed"


foreach y of global outcomes{
local m=0

foreach z of global interactions{
local m=`m'+1
gen zz=`z'


* Panel A: Any Treatment

quietly: gen bu1`m'= ((erand*`mammen'*u1`m')+((1-erand)*(1-`mammen')*u1`m'))*`G'/(`G'-1)
quietly:gen yhat1`m'= bu1`m'+xb1`m'+est[1,`m']*zz
quietly: reg yhat1`m'  i.T_FB##c.zz ss* , cluster(ID)

 if _b[1.T_FB#c.zz]!=.{
matrix pvalue`m'[`j',1]=(abs(((_b[1.T_FB#c.zz])/_se[1.T_FB#c.zz]))>abs(w[1,`m']))
}

* Panel B: Different Ads

quietly: gen bu2`m'= ((erand*`mammen'*u2`m')+((1-erand)*(1-`mammen')*u2`m'))*`G'/(`G'-1)
quietly:gen yhat2`m'= bu2`m'+xb2`m'+est[2,`m']*zz+est[3,`m']*zz*2.Treat_Ads+est[4,`m']*zz*3.Treat_Ads

quietly:reg yhat2`m' i.Treat_Ads##c.zz  ss*  , cluster(ID)
 
 if _b[1.Treat_Ads#c.zz]!=.{
matrix pvalue`m'[`j',2]=(abs(((_b[1.Treat_Ads#c.zz])/_se[1.Treat_Ads#c.zz]))>abs(w[2,`m']))
}

quietly: gen bu3`m'=((erand*`mammen'*u3`m')+((1-erand)*(1-`mammen')*u3`m'))*`G'/(`G'-1)
quietly:gen yhat3`m'= bu3`m'+xb3`m'+est[5,`m']*zz+est[6,`m']*zz*1.Treat_Ads+est[7,`m']*zz*3.Treat_Ads

quietly:reg yhat3`m' i.Treat_Ads##c.zz  ss*  , cluster(ID)
 
 if _b[2.Treat_Ads#c.zz]!=.{
matrix pvalue`m'[`j',3]=(abs(((_b[2.Treat_Ads#c.zz])/_se[2.Treat_Ads#c.zz]))>abs(w[3,`m']))
}


quietly: gen bu4`m'= ((erand*`mammen'*u4`m')+((1-erand)*(1-`mammen')*u4`m'))*`G'/(`G'-1)
quietly:gen yhat4`m'= bu4`m'+xb4`m'+est[8,`m']*zz+est[9,`m']*zz*1.Treat_Ads+est[10,`m']*zz*2.Treat_Ads

quietly:reg yhat4`m' i.Treat_Ads##c.zz  ss*  , cluster(ID)
 
 if _b[3.Treat_Ads#c.zz]!=.{
matrix pvalue`m'[`j',4]=(abs(((_b[3.Treat_Ads#c.zz])/_se[3.Treat_Ads#c.zz]))>abs(w[4,`m']))
}


* Panel C: Letter - No Letter

quietly: gen bu5`m'= ((erand*`mammen'*u5`m')+((1-erand)*(1-`mammen')*u5`m'))*`G'/(`G'-1)
quietly:gen yhat5`m'= bu5`m'+xb5`m'+est[11,`m']*zz+est[12,`m']*zz*2.Treat_Letter

quietly:reg yhat5`m'  i.Treat_Letter##c.zz  ss*  , cluster(ID)

 if _b[1.Treat_Letter#c.zz]!=.{
matrix pvalue`m'[`j',5]=(abs(((_b[1.Treat_Letter#c.zz])/_se[1.Treat_Letter#c.zz]))>abs(w[5,`m']))
}

quietly: gen bu6`m'= ((erand*`mammen'*u6`m')+((1-erand)*(1-`mammen')*u6`m'))*`G'/(`G'-1)
quietly:gen yhat6`m'= bu6`m'+xb6`m'+est[13,`m']*zz+est[14,`m']*zz*1.Treat_Letter

quietly:reg yhat6`m'  i.Treat_Letter##c.zz  ss*  , cluster(ID)

 if _b[2.Treat_Letter#c.zz]!=.{
matrix pvalue`m'[`j',6]=(abs(((_b[2.Treat_Letter#c.zz])/_se[2.Treat_Letter#c.zz]))>abs(w[6,`m']))
}

* Panel D: Different Letters

quietly: gen bu7`m'= ((erand*`mammen'*u7`m')+((1-erand)*(1-`mammen')*u7`m'))*`G'/(`G'-1)
quietly:gen yhat7`m'= bu7`m'+xb7`m'+est[15,`m']*zz+est[16,`m']*zz*2.Treat_Letters+est[17,`m']*zz*3.Treat_Letters

quietly:reg yhat7`m'  i.Treat_Letters##c.zz  ss*  , cluster(ID)

 if _b[1.Treat_Letters#c.zz]!=.{
matrix pvalue`m'[`j',7]=(abs(((_b[1.Treat_Letters#c.zz])/_se[1.Treat_Letters#c.zz]))>abs(w[7,`m']))
}


quietly: gen bu8`m'= ((erand*`mammen'*u8`m')+((1-erand)*(1-`mammen')*u8`m'))*`G'/(`G'-1)
quietly:gen yhat8`m'= bu8`m'+xb8`m'+est[18,`m']*zz+est[19,`m']*zz*1.Treat_Letters+est[20,`m']*zz*3.Treat_Letters

quietly:reg yhat8`m'  i.Treat_Letters##c.zz  ss*  , cluster(ID)

 if _b[2.Treat_Letters#c.zz]!=.{
matrix pvalue`m'[`j',8]=(abs(((_b[2.Treat_Letters#c.zz])/_se[2.Treat_Letters#c.zz]))>abs(w[8,`m']))
}


quietly: gen bu9`m'= ((erand*`mammen'*u9`m')+((1-erand)*(1-`mammen')*u9`m'))*`G'/(`G'-1)
quietly:gen yhat9`m'= bu9`m'+xb9`m'+est[21,`m']*zz+est[22,`m']*zz*1.Treat_Letters+est[23,`m']*zz*2.Treat_Letters

quietly:reg yhat9`m'  i.Treat_Letters##c.zz  ss*  , cluster(ID)

 if _b[3.Treat_Letters#c.zz]!=.{
matrix pvalue`m'[`j',9]=(abs(((_b[3.Treat_Letters#c.zz])/_se[3.Treat_Letters#c.zz]))>abs(w[9,`m']))
}


drop *zz*
}
}
}



mata
pvalue1 = st_matrix("pvalue1")
pvalue2 = st_matrix("pvalue2")
pvalue3 = st_matrix("pvalue3")
est = st_matrix("est")

pvalues = J(9, 3, .)

/* P-values on "Any Treatment": */
pvalues[1,1]=mean(pvalue1[,1])
pvalues[1,2]=mean(pvalue2[,1])
pvalues[1,3]=mean(pvalue3[,1])

/* P-values on "Information Ad": */
pvalues[2,1]=mean(pvalue1[,2])
pvalues[2,2]=mean(pvalue2[,2])
pvalues[2,3]=mean(pvalue3[,2])

/* P-values on "Call-to-Action Ad": */
pvalues[3,1]=mean(pvalue1[,3])
pvalues[3,2]=mean(pvalue2[,3])
pvalues[3,3]=mean(pvalue3[,3])

/* P-values on "Info+Call-to-Action Ad": */
pvalues[4,1]=mean(pvalue1[,4])
pvalues[4,2]=mean(pvalue2[,4])
pvalues[4,3]=mean(pvalue3[,4])

/* P-values on "No Letter - Any Ad": */
pvalues[5,1]=mean(pvalue1[,5])
pvalues[5,2]=mean(pvalue2[,5])
pvalues[5,3]=mean(pvalue3[,5])

/* P-values on "Letter - Any Ad": */
pvalues[6,1]=mean(pvalue1[,6])
pvalues[6,2]=mean(pvalue2[,6])
pvalues[6,3]=mean(pvalue3[,6])

/* P-values on "No Letter - Any Ad": */

pvalues[7,1]=mean(pvalue1[,7])
pvalues[7,2]=mean(pvalue2[,7])
pvalues[7,3]=mean(pvalue3[,7])

/* P-values on "Partial Knowledge Letter - Any Ad": */
pvalues[8,1]=mean(pvalue1[,8])
pvalues[8,2]=mean(pvalue2[,8])
pvalues[8,3]=mean(pvalue3[,8])

/* P-values on "Full Knowledge Letter - Any Ad": */
pvalues[9,1]=mean(pvalue1[,9])
pvalues[9,2]=mean(pvalue2[,9])
pvalues[9,3]=mean(pvalue3[,9])

st_matrix("pvalues", pvalues)
end

label var  p_d_misdeed "Will do irregs (frac)"
label var  d_mean_p_d_misdeed "Demean Will do irregs (frac)"
label var  above_mean_p_d_misdeed "Above Av Will do irregs (=1)"
#d ;
matrix rownames pvalues = "Any Treatment" "Information Ad" "Call-to-Action Ad" "Info+Call-to-Action Ad"
                          "No Letter - Any Ad" "Letter - Any Ad" "No Letter - Any Ad" 
						  "P Knowledge Letter - Any Ad" "Full Knowledge Letter - Any Ad";
matrix colnames pvalues = "Will do irregs (frac)" "Demean Will do irregs (frac)" "Above Av Will do irregs (=1)";
#d cr

mat2txt, matrix(pvalues) saving("$out/wildbootrap_pvals_nocontrols") format(%9.3f) replace

