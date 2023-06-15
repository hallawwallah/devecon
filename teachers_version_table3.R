# TABLES AS SHOWN IN THE EXERCISE SESSION 

install.packages("lmtest")
install.packages("mice")
install.packages("Rtools")
install.packages("clubSandwich")
install.packages("knitr")
install.packages("stargazer")


library(tidyverse)
library(haven)
library(lmtest)  # for clustered standard errors
library(glmnet)  # for lasso
library(mice)    # for value imputation
library(sandwich)# for robust standard errors
library(clubSandwich)
library(estimatr)
library(knitr)
library(ggplot2)
library(kableExtra)
library(webshot)
library(stargazer)
library(modelsummary)


# Load the data
data <- drop_na(read_dta("main_data_municipal_level.dta")) 


# controls
# political <- c("n_cand_real", "pct_santos_2p2014", "pct_zuluaga_2p2014", "win_margin_a2015", "part_c2018", "pct_blanco_c2018", "pct_cambioradical_c2018", "pct_centrodem_c2018", "pct_conservador_c2018", "pct_decentes_c2018", "pct_liberal_c2018", "pct_partU_c2018", "pct_polo_c2018", "pct_verde_c2018")
# violence <- "homicidios_pc"
# development <- c("indruralildad2017", "nbi2005", "pib_num_pc2016")
# other <- c("l_pop2018", "penetration_fb_pre", "reports_any_MOE_c2018", "reports_any_MOE_a2015", "z_index_second_a2015", "sig95_max_second_a2015")
# candidates <- c("l_age", "female", "incumbent_loose", "incumbent", "citizen_group", "coalition", "winner_c")
# 
# controls <- c(political, violence, development, other, "ss*", "rr*")
# controls_can <- c(political, violence, development, other, candidates, "n_respuestas_pre", "ss*", "rr*")

# all the outcomes we have to control for
outcomes <- c("d_reportMOE_any", "reportMOE_any", "d_reportMOE_any_quality", "reportMOE_any_quality")

reg1 = lm_robust(d_reportMOE_any ~ factor(T_FB),
           data = data,
           clusters = ID,
           se_type ="stata")

reg2 = lm_robust(d_reportMOE_any ~ factor (T_FB) + indruralildad2017 + l_pop2018 + reports_any_MOE_c2018 + reports_any_MOE_a2015 +ss9,
          data = data,
          clusters = ID,
          se_type = "stata")

#Being in the treatment Increases the prob. to file a report by 10.6 pp (increase of 37 percent in comparison to control me

reg3 = lm_robust(d_reportMOE_any_quality ~ factor(T_FB) + nbi2005 + l_pop2018,
          data = data,
          clusters = ID,
          se_type ="stata")

reg4 = lm_robust(reportMOE_any_quality ~ factor(T_FB) + nbi2005 + l_pop2018,
                 data = data,
                 clusters = ID,
                 se_type ="stata")

#Increase of 0.19 in br of high quality reports (94 percent increase in comparison control)


combined_table <- list(reg1, reg2, reg3, reg4)

modelsummary(combined_table)

