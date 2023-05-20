/*==================================================================================================
Project:       All Eyes on Them: A Field Experiment on Citizen Oversight and Electoral Integrity
Author:        Mateo Montenegro 
Purpose:       This code executes all the programs preparing and cleaning the data used in the
			   paper, labels the variables, and builds the main datasets.
----------------------------------------------------------------------------------------------------
Index:		   A. Running Cleaning and Preparation Routines
			   B. Constructing Main Data at Municipal-Level
			   C. Constructing Main Data at Candidate-Level
===================================================================================================*/

est clear
set more off

/*===============================================================================================
                                  A. Running Cleaning and Preparation Routines
===============================================================================================*/

* Note: The following lines of code must be run in order

do "$prep_code/aer_prep_treatment_indicators.do"
do "$prep_code/aer_prep_population.do"
do "$prep_code/aer_prep_socioeconomic_covs.do"
do "$prep_code/aer_prep_region_indicator_covs.do"
do "$prep_code/aer_prep_pre_svy_variables.do"
do "$prep_code/aer_prep_post_svy_variables.do"
do "$prep_code/aer_prep_moe_reports.do"
do "$prep_code/aer_prep_media_irregularity_outcomes.do"
do "$prep_code/aer_prep_incumbent_party.do"
do "$prep_code/aer_prep_gdp2016.do"
do "$prep_code/aer_prep_forensics_2015.do"
do "$prep_code/aer_prep_forensic_outcomes_2019.do"
do "$prep_code/aer_prep_facebook_ad_metrics.do"
do "$prep_code/aer_prep_election_outcomes_2019.do"
do "$prep_code/aer_prep_election_controls.do"
do "$prep_code/aer_prep_candidate_past_malfeasance.do"
do "$prep_code/aer_prep_candidate_demographic_covs_2019.do"


/*===============================================================================================
                                  B. Constructing Main Data at Municipal-Level
===============================================================================================*/

* Note: The municipalities not merged in the following code are not included in the experimental sample.

use "$intermediate_data/treatment_indicators.dta", clear

merge 1:1 ID using "$intermediate_data/population.dta"
drop _m

merge 1:1 ID using "$intermediate_data/socioeconomic_controls.dta"
drop _m

merge 1:1 ID using "$intermediate_data/region_indicators.dta"
drop _m

merge 1:1 ID using "$intermediate_data/pre_svy_number_respondents_clean.dta"
drop _m

merge 1:1 ID using "$intermediate_data/post_svy_irregularity_outcomes_clean.dta"
drop _m

merge 1:1 ID using "$intermediate_data/post_svy_number_respondents_clean.dta"
drop _m

merge 1:1 ID using "$intermediate_data/moe_reports.dta"
drop _m

merge 1:1 ID using "$intermediate_data/media_irregularity_outcomes.dta"
drop _m

merge 1:1 ID using "$intermediate_data/gdp2016.dta"
drop _m

merge 1:1 ID using "$intermediate_data/forensics_2015_covs.dta"
drop _m

merge 1:1 ID using "$intermediate_data/forensic_outcomes_2019.dta"
drop _m

merge 1:1 ID using "$intermediate_data/fb_users_clean.dta"
drop _m

merge 1:1 ID using "$intermediate_data/facebook_ad_metrics.dta"
drop _m

merge 1:1 ID using "$intermediate_data/additional_election_outcomes_2019.dta"
drop _m

merge 1:1 ID using "$intermediate_data/election_controls_c2018.dta"
drop _m

merge 1:1 ID using "$intermediate_data/election_controls_a2015.dta"
drop _m

merge 1:1 ID using "$intermediate_data/election_controls_2p2014.dta"
drop _m

* Constructing per capita variables:

gen homicidios_pc=homicidios*100000/pop2018
gen pib_num_pc2016 =pib_num_2016*100/pop2018
gen pct_Reach=Reach*100/pop2018_18older
gen penetration_fb_pre=fb_users_pre/pop2018_18older

* Labeling variables:

label var  ID "Municipality identifier (DANE)"
label var  strata "Strata used for randomization (categorical)"
label var  departamento "Name of department"
label var  municipio "Name of municipality"
label var  T_FB "Any Treatment (=1)"
label var  T_FB_info "Information Ad (=1)"
label var  T_FB_call "Call-to-action Ad (=1)"
label var  T_FB_both "Info+CtA Ad (=1)"
label var  TT_FB "No Letter - Any Add (=1)"
label var  T_Letter "Letter - Any Ad (=1)"
label var  T_Letter_no_sj "Partial Knowledge Letter - Any Ad (=1)"
label var  T_Letter_sj "Full Knowledge Letter - Any Ad (=1)"
label var  Treat_Letter "Control-No Letter-Letter (categorical)"
label var  Treat_Letters "Control-No Letter-PK Letter-FK Letter (categorical)"
label var  Treat_Ads "Control-Info-CtA-Info+CtA Ad (categorical)" 

label var  reportMOE_any "Number of reports on dates of the intervention"
label var  reportMOE_any_quality "Number of high quality reports on dates of the intervention"
label var  d_reportMOE_any "Reports on dates of the intervention (=1)"
label var  d_reportMOE_any_quality "High quality reports on dates of the intervention (=1)"
label var  reportMOE_late_any "Number of reports on dates after the intervention"
label var  reportMOE_late_any_quality "Number of high quality reports on dates after the intervention"
label var  d_reportMOE_late_any "Reports on dates after the intervention (=1)"
label var  d_reportMOE_late_any_quality "High quality reports on dates after the intervention (=1)"


label var  media_irreg "Number of media irregularities"
label var  d_media_irreg "Media irregularities (=1)"
label var  media_irreg_inclMOE "Number of media irregularities including news coming from MOE reports"
label var  d_media_irreg_inclMOE "Media irregularities including news coming from MOE reports (=1)"
label var  media_irreg_disturbio "Number of media irregularities about riots"
label var  d_media_irreg_disturbio "Media irregularities about riots (=1)"
label var  media_irreg_sin_disturbio "Number of media irregularities excluding riots"
label var  d_media_irreg_sin_disturbio "Media irregularities excluding riots (=1)"
label var  media_irreg_compra "Number of media irregularities about vote-buying"
label var  d_media_irreg_compra "Media irregularities about vote-buying (=1)"
label var  media_irreg_sin_compra "Number of media irregularities excluding vote-buying"
label var  d_media_irreg_sin_compra "Media irregularities excluding vote-buying (=1)"
label var  media_irreg_cand_int "Number of media irregularities about candidate intimidation"
label var  d_media_irreg_cand_int "Media irregularities about candidate intimidation (=1)"
label var  media_irreg_sin_cand_int "Number of media irregularities excluding candidate intimidation"
label var  d_media_irreg_sin_cand_int "Media irregularities excluding candidate intimidation (=1)"
label var  media_irreg_intimidacion "Number of media irregularities about voter intimidation"
label var  d_media_irreg_intimidacion "Media irregularities about voter intimidation (=1)"
label var  media_irreg_sin_intimidacion "Number of media irregularities excluding voter intimidation"
label var  d_media_irreg_sin_intimidacion "Media irregularities excluding voter intimidation (=1)"
label var  media_irreg_trashumancia "Number of media irregularities about registration fraud"
label var  d_media_irreg_trashumancia "Media irregularities about registration fraud (=1)"
label var  media_irreg_sin_trashumancia "Number of media irregularities excluding registration fraud"
label var  d_media_irreg_sin_trashumancia "Media irregularities excluding registration fraud (=1)"
label var  media_irreg_intervencion "Number of media irregularities about public servant campaigning"
label var  d_media_irreg_intervencion "Media irregularities about public servant campaigning (=1)"
label var  media_irreg_sin_intervencion "Number of media irregularities excluding public servant campaigning"
label var  d_media_irreg_sin_intervencion "Media irregularities excluding public servant campaigning (=1)"
label var  media_irreg_publicidad "Number of media irregularities about illicit advertising"
label var  d_media_irreg_publicidad "Media irregularities about public illicit advertising (=1)"
label var  media_irreg_sin_publicidad "Number of media irregularities excluding illicit advertising"
label var  d_media_irreg_sin_publicidad "Media irregularities excluding illicit advertising (=1)"
label var  media_irreg_fraud "Number of media irregularities about electoral fraud"
label var  d_media_irreg_fraud "Media irregularities about public electoral fraud (=1)"
label var  media_irreg_sin_fraud "Number of media irregularities excluding electoral fraud"
label var  d_media_irreg_sin_fraud "Media irregularities excluding electoral fraud (=1)"
label var  media_irreg_otro "Number of media irregularities about other types of irregularities"
label var  d_media_irreg_otro "Media irregularities about other types of irregularities (=1)"
label var  media_irreg_sin_otro "Number of media irregularities excluding "
label var  d_media_irreg_sin_otro "Media irregularities excluding other types of irregularities (=1)"


label var  z_index_second "2nd Digit: Index - All Forensic Stats (z-score)"
label var  sig95_max_second "2nd Digit: Any P-val From Forensics $<0.05 (=1)$"
label var  z_index_last "Last Digit: Index - All Forensic Stats (z-score)"
label var  sig95_max_last "Last Digit: Any P-val From Forensics $<0.05 (=1)$"
label var  sig95_kolmo_second "2nd Digit Kolmogorov-Smirnov Test: P-val From Forensics $<0.05 (=1)$"
label var  sig95_chi2_second "2nd Digit Chi2 Test: P-val From Forensics $<0.05 (=1)$"
label var  sig95_kuiper_second "2nd Digit Kuiper Test: P-val From Forensics $<0.05 (=1)$"
label var  sig95_kolmo_last "Last Digit Kolmogorov-Smirnov Test: P-val From Forensics $<0.05 (=1)$"
label var  sig95_chi2_last "Last Digit Chi2 Test: P-val From Forensics $<0.05 (=1)$"
label var  sig95_kuiper_last "Last Digit Kuiper Test: P-val From Forensics $<0.05 (=1)$"
label var  z_kolmo_second "2nd Digit Kolmogorov-Smirnov Test: Statistic (z-score)"
label var  z_chi2_second "2nd Digit Chi2 Test: Statistic (z-score)"
label var  z_kuiper_second "2nd Digit Kuiper Test: Statistic (z-score)"
label var  z_kolmo_last "Last Digit Kolmogorov-Smirnov Test: Statistic (z-score)"
label var  z_chi2_last "Last Digit Chi2 Test: Statistic (z-score)"
label var  z_kuiper_last "Last Digit Kuiper Test: Statistic (z-score)"
label var  d_repeat_digits "Last Digits Repeated More Than Expected (=1)"
label var  d_adjecent_digits "Adjacent Pairs of Last Digits More Than Expected (=1)"

label var  win_marg2019 "Margin of Victory"
label var  turnout2019 "Turnout"

label var  pct_santos_2p2014 "Santos Vote Share 2014"
label var  pct_zuluaga_2p2014 "Zuluaga Vote Share 2014"
label var  win_margin_a2015 "Mayor Margin of Victory 2015"
label var  part_c2018 "Turnout 2018 (\%)"
label var  pct_blanco_c2018 "Share Blank Votes 2018 (\%)"
label var  pct_cambioradical_c2018 "Cambio R Vote Share 2018"
label var  pct_centrodem_c2018 "Centro Dem Vote Share 2018"
label var  pct_conservador_c2018 "Conservatives Vote Share 2018"
label var  pct_decentes_c2018 "Decentes Vote Share 2018"
label var  pct_liberal_c2018 "Liberals Vote Share 2018"
label var  pct_partU_c2018 "P de la U Vote Share 2018"
label var  pct_polo_c2018 "Polo Vote Share 2018"
label var  pct_verde_c2018 "Green Party Vote Share 2018"


label var  homicidios_pc "Homicide Rate 2017"
label var  homicidios "Homicides 2017"
label var  indruralildad2017 "Rural Population 2017"
label var  nbi2005 "Poor 2005 (\%)"
label var  pib_num_pc2016 "GDP p.c. 2016 (Ms of Pesos)"
label var  pib_num_2016 "GDP 2016 (Ms of Pesos)"
label var  pop2018 "Population"
label var  pop2018_18older "Adult Population"
label var  l_pop2018 "Log(Population)"
label var  penetration_fb_pre "Facebook Penetration 2019"
label var  fb_users_pre "Facebook Users 2019" 
label var  reports_any_MOE_c2018 "Reports to MOE 2018"
label var  reports_any_MOE_a2015 "Reports to MOE 2015"
label var  n_cand_real "Number of Candidates 2019"

label var  z_index_second_a2015 "Index - All Forensic Stats 2015"
label var  sig95_max_second_a2015 "P-val From Forensics $<0.05$ 2015"
label var  n_respuestas_pre "Number Responses Survey"
label var  n_respuestas_post "Number Responses Post-Treat Survey"
label var  region "Geographical Region (categorical)"
label var  rr1 "Caribean Region (=1)"
label var  rr2 "Center-East Region (=1)"
label var  rr3 "Center-South Region (=1)"
label var  rr4 "Coffee-growing Region (=1)"
label var  rr5 "Llanos Region (=1)"
label var  rr6 "Pacific Region (=1)"

label var  pct_Reach "Population Reached by Ads (\%)"
label var  Reach "Users Reached by Ads (Thousands)"
label var  no_Reach "No Users Reached by Ads (=1)"

label var  z_misdeeds_index "Irregularity Index from Post-Treat Survey (z-score)"
label var  z_likelihood_misdeed_2 "Vote-buying from Post-Treat Survey (z-score)"
label var  z_likelihood_misdeed_1 "Voter Intimidation from Post-Treat Survey (z-score)"
label var  z_likelihood_misdeed_3 "Registration Fraud from Post-Treat Survey (z-score)"
label var  z_likelihood_misdeed_5 "Public Servant Campaigning from Post-Treat Survey (z-score)"
label var  z_likelihood_misdeed_4 "Electoral Fraud from Post-Treat Survey (z-score)"
label var  z_likelihood_misdeed_6 "Illicit Advertising from Post-Treat Survey (z-score)"

save "$final_data/main_data_municipal_level.dta", replace


/*===============================================================================================
                                  C. Constructing Main Data at Candidate-Level
===============================================================================================*/

use "$intermediate_data/election_outcomes_2019.dta", clear

merge n:1 ID using "$final_data/main_data_municipal_level.dta"
drop _m

merge 1:1 ID can using "$intermediate_data/candidate_demographic_covariates_2019.dta"
drop _m

merge n:1 ID can using "$intermediate_data/candidate_past_malfeasance.dta"
drop _m

merge n:1 ID can using "$intermediate_data/incumbent_party_vars.dta"
drop _m

merge n:1 ID can using "$intermediate_data/pre_svy_candidate_variables_clean.dta"
drop _m

merge n:1 ID can using "$intermediate_data/pre_svy_candidate_variables_clean.dta"
drop _m

* Labeling variables:

label var  ID "Municipality identifier (DANE)"
label var  can "Candidate Code"
label var  cannombre "Candidate Name"
label var  pct_vote "Vote Share of Candidate (\%)"
label var  p_d_misdeed "Candidate will engage in irregularities (fraction of respondents)"
label var  d_mean_p_d_misdeed "Demeaned Candidate will engage in irregularities (fraction of respondents)"
label var  above_mean_p_d_misdeed "Above Average Candidate will engage in irregularities (fraction of respondents) "
label var  cuestionado "Past  Malfeasance (=1)"
label var  l_age "Log(Age)"
label var  female "Female (=1)"
label var  incumbent_loose "Incumbent Party - Lax (=1)"
label var  incumbent "Incumbent Party - Strict (=1)"
label var  citizen_group "Independent Candidate (=1)"
label var  coalition "Party Coalition (=1)"
label var  winner_c "Candidate Will Win (frac respondents)"
 

save "$final_data/main_data_candidate_level.dta", replace
