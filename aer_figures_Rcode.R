# setwd("C:/Users/billo/OneDrive/Desktop/FAU/DEVELOPEMENT ECONOMICS/GROUP WORK/160921-V1/devecon")
install.packages("lmtest")
install.packages("mice")
install.packages("Rtools")
install.packages("clubSandwich")


library(tidyverse)
library(haven)
library(lmtest)  # for clustered standard errors
library(glmnet)  # for lasso
library(mice)    # for value imputation
library(sandwich)# for robust standard errors
library(clubSandwich)


# Load the data
data <- drop_na(read_dta("main_data_municipal_level.dta")) 

#DOUBT: IS IT OKAY TO EXCLUDE MISSING VALUES? 
# THE CODE WAS GIVING AN ERROR OTHERWISE WHEN PERFORMING LASSO REGRESSION

# Define control sets ----
political <- c("n_cand_real", "pct_santos_2p2014", "pct_zuluaga_2p2014", "win_margin_a2015",
               "part_c2018", "pct_blanco_c2018", "pct_cambioradical_c2018", "pct_centrodem_c2018",
               "pct_conservador_c2018", "pct_decentes_c2018", "pct_liberal_c2018", "pct_partU_c2018",
               "pct_polo_c2018", "pct_verde_c2018")

violence <- c("homicidios_pc")

development <- c("indruralildad2017", "nbi2005", "pib_num_pc2016")

other <- c("l_pop2018", "penetration_fb_pre", "reports_any_MOE_c2018", "reports_any_MOE_a2015",
           "z_index_second_a2015", "sig95_max_second_a2015")

candidates <- c("l_age", "female", "incumbent_loose", "incumbent", "citizen_group",
                "coalition", "winner_c")

ss <- grep("^ss", names(data), value = TRUE)

rr <- grep("^rr", names(data), value = TRUE)

# Define larger control variables from control sets ----

controls <- c(political, violence, development, other, ss, rr)

controls_can <- c(political, violence, development, other, candidates, data$n_respuestas_pre, ss, rr)

# coef_obj <- coef(fit_y, s = "lambda.min")[-1, 1]
# variable_names <- names(coef_obj)

# Figure 4 ----

outcomes <- c("reportMOE_any", "d_reportMOE_any")

i = 0 # I HAVEN'T FIGURED OUT WHAT THIS IS FOR BUT IT WAS IN THE STATA CODE

# # corresponds to $controls , cluster(ID) in the stata code; instead of rewriting every time, I create a new variable and use it in every regression
controls_clusterID = data[, c("ID", controls)]
controls_clusterID = makeX(controls_clusterID) # required to avoid an error message:
# Converts a data frame to a data matrix suitable for input to glmnet. Factors are converted to dummy matrices via "one-hot" encoding. Options deal with missing values and sparsity.

for (y in outcomes) {
    
    # Perform Lasso regression for y with control variables
    fit_y <- cv.glmnet(controls_clusterID, data[[y]], alpha = 1, family = "gaussian", type.measure = "mse")
    ySel <- names(coef(fit_y, s = "lambda.min")[-1, 1])
    
    # Perform Lasso regression for T_FB with control variables
    fit_T_FB <- cv.glmnet(controls_clusterID, data$T_FB, alpha = 1, family = "gaussian", type.measure = "mse")
    xSel0 <- names(coef(fit_T_FB, s = "lambda.min")[-1, 1])
    
    # Perform Lasso regression for TT_FB with control variables
    fit_TT_FB <- cv.glmnet(controls_clusterID, data$TT_FB, alpha = 1, family = "gaussian", type.measure = "mse")
    xSel1 <- names(coef(fit_TT_FB, s = "lambda.min")[-1, 1])
    
    # Perform Lasso regression for T_FB_call with control variables
    fit_T_FB_call <- cv.glmnet(controls_clusterID, data$T_FB_call, alpha = 1, family = "gaussian", type.measure = "mse")
    xSel2 <- names(coef(fit_T_FB_call, s = "lambda.min")[-1, 1])
    
    # Perform Lasso regression for T_FB_info with control variables
    fit_T_FB_info <- cv.glmnet(controls_clusterID, data$T_FB_info, alpha = 1, family = "gaussian", type.measure = "mse")
    xSel3 <- names(coef(fit_T_FB_info, s = "lambda.min")[-1, 1])
    
    # Perform Lasso regression for T_FB_both with control variables
    fit_T_FB_both <- cv.glmnet(controls_clusterID, data$T_FB_both, alpha = 1, family = "gaussian", type.measure = "mse")
    xSel4 <- names(coef(fit_T_FB_both, s = "lambda.min")[-1, 1])
    
    # Perform Lasso regression for T_Letter with control variables
    fit_T_Letter <- cv.glmnet(controls_clusterID, data$T_Letter, alpha = 1, family = "gaussian", type.measure = "mse")
    xSel5 <- names(coef(fit_T_Letter, s = "lambda.min")[-1, 1])
    
    # Perform Lasso regression for T_Letter_no_sj with control variables
    fit_T_Letter_no_sj <- cv.glmnet(controls_clusterID, data$T_Letter_no_sj, alpha = 1, family = "gaussian", type.measure = "mse")
    xSel6 <- names(coef(fit_T_Letter_no_sj, s = "lambda.min")[-1, 1])
    
    # Perform Lasso regression for T_Letter_sj with control variables
    fit_T_Letter_sj <- cv.glmnet(controls_clusterID, data$T_Letter_sj, alpha = 1, family = "gaussian", type.measure = "mse")
    xSel7 <- names(coef(fit_T_Letter_sj, s = "lambda.min")[-1, 1])
    
    # create the variables vector of the same name in the Stata code 
    vSel <- c(ySel, xSel0, xSel1, xSel2, xSel3, xSel4, xSel5, xSel6, xSel7)
    
    # Any Treat
    pexact <- matrix(NA, nrow = 1, ncol = 3)
    
    # DATA BINS?
    data$group <- rep(1, nrow(data))
    data$group[1] <- 2
    
    # extract the mean for the treatment(s)
    # FOR SOME REASON IT DOESN'T WORK WITHIN THE FOR LOOP
    summary_y <- data %>% filter(T_FB == 0) 
    summary_y = summary_y[y]
    control <- mean(summary_y$reportMOE_any)
    
    
    data$est <- rep(control, nrow(data))
    data$est = ifelse(data$group == 1, control, 0)
    
    # fit the regression reg `y' T_FB  `vSel' , cluster(ID)
    browser() # debug command to step through the code: https://berkeley-scf.github.io/tutorial-R-debugging/#31-interactive-debugging-via-the-browser
    
    # main regression of the outcome on the vector created before
    # i know it's weird but it wouldn't work any other way: 
    # I would get a length mismatch between the data and the vector
    # even though the former contains every variable thatis inside it 
    vars = paste(vSel, collapse = "+")
    fit <- lm(paste(y, " ~ ", vars), data = data)
    
    # Update the value of 'est' by adding the coefficient of 'T_FB' multiplied by 'control' for 'group' equals 2
    data$est[data$group == 2] <- data$est[data$group == 2] + coef(fit)["T_FB"] * control
    
    # Cluster-robust standard errors using the sandwich package
    vcov <- vcovHC(fit, cluster = "ID")
    
    coeftest(fit, vcov = vcov)
    
    # data$est[group == 2] <- control + coefs["T_FB"]
    # in the stata code: replace est= `control'+_b[T_FB] if group==2
    # the _b represents coefficient estimates: however there is no estimate 
    # for the T_FB variable in the clustered covariance matrix
    ci_low <- rep(NA, nrow(data))
    ci_high <- rep(NA, nrow(data))
    filtered_data = data %>% filter(group == 2)
    
    ci_low <- filtered_data$est- 1.96 * coef(fit)["T_FB"] # supposedly 
    # you should be able to access the coefficient by directly indexing
    # into the matrix. However I just got NAs because the coefficient is not there
    ci_high <- filtered_data$est + 1.96 * coef(fit)["T_FB"]
  
    # rest of the code I still haven't had a chance to try
    
  # # Letters:
  # data$group1 <- ifelse(seq_len(nrow(data)) == 1, 2,
  #                       ifelse(seq_len(nrow(data)) == 2, 3, 1))
  # 
  # fit_letter <- lm(paste(y, " ~ as.factor(Treat_Letter) + ", paste(vSel, collapse = " + ")),
  #                  data = data, cluster = data$ID)
  # 
  # est1 <- control + coef(fit_letter)[2]
  # ci_low1 <- est1 - 1.96 * sqrt(vcovHC(fit_letter, cluster = data$ID)[2, 2])
  # ci_high1 <- est1 + 1.96 * sqrt(vcovHC(fit_letter, cluster = data$ID)[2, 2])
  # 
  # ci_high2 <- max(ci_high, ci_high1) + 0.07
}

