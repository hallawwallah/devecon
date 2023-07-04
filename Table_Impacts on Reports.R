# Table - Impacts on Reporsts:

rm(list = ls())

library(usethis)


library(haven) # Load data

library(tidyverse) # Basic commands

library(ggplot2) # Plots
library(ggpubr)

library(kableExtra) # Tables
library(modelsummary)
library(gt)

library(estimatr) # Robust standard errors
#################
municipal <- read_dta("C:/main_data_municipal_level.dta") 

# any treatment


reg_1_T <- lm_robust(d_reportMOE_any ~ factor(T_FB)+indruralildad2017+ l_pop2018+ reports_any_MOE_c2018 + reports_any_MOE_a2015 +ss9,
                     data=municipal,
                     se_type="stata")

reg_2_T <- lm_robust(reportMOE_any ~ factor(T_FB)+indruralildad2017+ l_pop2018+ reports_any_MOE_c2018 + reports_any_MOE_a2015 +ss9,
                     data=municipal,
                     se_type="stata")



reg_3_T <- lm_robust(d_reportMOE_any_quality ~ factor(T_FB)+indruralildad2017 + nbi2005 + l_pop2018+ reports_any_MOE_c2018 +ss9,
                     data=municipal,
                     se_type="stata")



reg_4_T <- lm_robust(reportMOE_any_quality ~ factor(T_FB)+ nbi2005 + l_pop2018,
                     data=municipal,
                     se_type="stata")

# Panel B Treat_Ads:

reg_1_B<- lm_robust(d_reportMOE_any ~ factor(T_FB_info)+ factor(T_FB_call) + factor(T_FB_both) +indruralildad2017 + nbi2005 + l_pop2018+ reports_any_MOE_c2018 +ss9,
                    data=municipal,
                    se_type="stata")

reg_3_B<- lm_robust(d_reportMOE_any_quality ~ factor(T_FB_info)+ factor(T_FB_call) + factor(T_FB_both) +indruralildad2017 + nbi2005 + l_pop2018+ reports_any_MOE_c2018 +ss9,
                    data=municipal,
                    se_type="stata")

reg_2_B <- lm_robust(reportMOE_any ~ factor(T_FB_info) + factor(T_FB_call) + factor(T_FB_both) + indruralildad2017 + nbi2005 + l_pop2018,
                     data=municipal,
                     se_type="stata")

reg_4_B <- lm_robust(reportMOE_any_quality ~ factor(T_FB_info) + factor(T_FB_call) + factor(T_FB_both) + indruralildad2017 + nbi2005 + l_pop2018,
                     data=municipal,
                     se_type="stata")

# Creating Table second trial:

reg_list <- list(
  "reports (= 1)" =reg_1_T,
  "n. reports" = reg_2_T,
  "high quality reports (= 1)" = reg_3_T,
  "high quality reports" = reg_4_T
)

including_controls <- c("factor(T_FB)")

modelsummary(
  reg_list,
  include = including_controls,
  stars = TRUE,
  title = "Table - Impact on Reports",
  coef_rename = c(
    "factor(T_FB)" = "any treatment"
  )
)

# Creating Table 

reg_list <- list(
  reg_1_T, reg_2_T, reg_3_T, reg_4_T,
  reg_1_B, reg_2_B, reg_3_B, reg_4_B
)

relevant_controls <- c("T_FB", "T_FB_info", "T_FB_call", "T_FB_both")

modelsummary(
  reg_list,
  include = relevant_controls,
  stars = TRUE,
  title = "Table - Impact on Reports",
  coef_rename = c(
    "T_FB" = "any treatment",
    "T_FB_info" = "information ad",
    "T_FB_call" = "call-to-action ad",
    "T_FB_both" = "information + call-to-action ad",
    "d_reportMOE_any" = "reports (= 1)",
    "reportMOE_any" = "n. reports",
    "d_reportMOE_any_quality" = "high quality reports (= 1)",
    "reportMOE_any_quality" = "high quality n. reports"
  )
)
