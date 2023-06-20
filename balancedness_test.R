# BALANCEDNESS TEST

library(stargazer)
# Load the data
data <- read_dta("main_data_municipal_level.dta") 

# T_FB
reg = lm(T_FB ~ reports_any_MOE_c2018, data = data)
summary(reg)

reg1 = lm(T_FB ~ l_pop2018, data = data)
summary(reg)

# T_FB_info
reg2 = lm(T_FB_info ~ reports_any_MOE_c2018, data = data)
summary(reg)

reg3 = lm(T_FB_info ~ l_pop2018, data = data)
summary(reg)


# T_FB_call
reg4 = lm(T_FB_call ~ reports_any_MOE_c2018, data = data)

reg5 = lm(T_FB_call ~ l_pop2018, data = data)
summary(reg)

regressions = list(reg, reg1, reg2, reg3, reg4, reg5)

modelsummary(regressions)



