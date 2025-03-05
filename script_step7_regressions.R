

# install.packages("gt", dependencies = TRUE, reinstall = TRUE)
# install.packages("rlang", dependencies = TRUE, type = "source")
# install.packages("kableExtra")

library(stargazer)
library(car)

library(broom)
library(dplyr)

library(gt)  # For nice tables (or use kableExtra)


library(kableExtra)

# load data  --------------------------------------------------------------


rm(list=ls())
load(file="data_step3-out-variables-all-setup.Rdata")

# Modellieren Part 1 -------------------------------------------------------------


# --- POV.CLUST.Mainly_economically_vulnerable ---
m1_vuln <- glm(POV.CLUST.Mainly_economically_vulnerable ~ eduyears, family = "binomial", data = data)
m2_vuln <- glm(POV.CLUST.Mainly_economically_vulnerable ~ highest_lifetime_ISCO_88_recoded, family = "binomial", data = data)
m3_vuln <- glm(POV.CLUST.Mainly_economically_vulnerable ~ eduyears + cohort + gender.rcd +
                 valid.information.wjoint.income.wealth.poverty.bn.w2 +
                 valid.information.wjoint.income.wealth.poverty.bn.w4 +
                 valid.information.wjoint.income.wealth.poverty.bn.w5 +
                 valid.information.wjoint.income.wealth.poverty.bn.w6 +
                 valid.information.wjoint.income.wealth.poverty.bn.w7 +
                 valid.information.wjoint.income.wealth.poverty.bn.w8 +
                 valid.information.wjoint.income.wealth.poverty.bn.w9, family = "binomial", data = data)
m4_vuln <- glm(POV.CLUST.Mainly_economically_vulnerable ~ highest_lifetime_ISCO_88_recoded + cohort + gender.rcd +
                 valid.information.wjoint.income.wealth.poverty.bn.w2 +
                 valid.information.wjoint.income.wealth.poverty.bn.w4 +
                 valid.information.wjoint.income.wealth.poverty.bn.w5 +
                 valid.information.wjoint.income.wealth.poverty.bn.w6 +
                 valid.information.wjoint.income.wealth.poverty.bn.w7 +
                 valid.information.wjoint.income.wealth.poverty.bn.w8 +
                 valid.information.wjoint.income.wealth.poverty.bn.w9, family = "binomial", data = data)

# Compute Odds Ratios
coef_m1_vuln <- exp(coef(m1_vuln))
coef_m2_vuln <- exp(coef(m2_vuln))
coef_m3_vuln <- exp(coef(m3_vuln))
coef_m4_vuln <- exp(coef(m4_vuln))

# Stargazer Table
stargazer(
  m1_vuln, m2_vuln, m3_vuln, m4_vuln,
  coef = list(coef_m1_vuln, coef_m2_vuln, coef_m3_vuln, coef_m4_vuln),
  type = "text", 
  report = "vc*",
  omit = c("valid*", "cohort*", "Constant", "gender*"),
  single.row = TRUE,
  p.auto = FALSE,
  digits = 2,
  dep.var.labels = "Mainly economically vulnerable", 
  dep.var.caption = "Poverty trajectory type",
  covariate.labels = c("Years of education", "Highly skilled occupation (ref. medium)", "Low skilled occupation")
)

# --- POV.CLUST.Mainly_missing___not_observed ---
m1_miss <- glm(POV.CLUST.Mainly_missing___not_observed ~ eduyears, family = "binomial", data = data)
m2_miss <- glm(POV.CLUST.Mainly_missing___not_observed ~ highest_lifetime_ISCO_88_recoded, family = "binomial", data = data)
m3_miss <- glm(POV.CLUST.Mainly_missing___not_observed ~ eduyears + cohort + gender.rcd +
                 valid.information.wjoint.income.wealth.poverty.bn.w2 +
                 valid.information.wjoint.income.wealth.poverty.bn.w4 +
                 valid.information.wjoint.income.wealth.poverty.bn.w5 +
                 valid.information.wjoint.income.wealth.poverty.bn.w6 +
                 valid.information.wjoint.income.wealth.poverty.bn.w7 +
                 valid.information.wjoint.income.wealth.poverty.bn.w8 +
                 valid.information.wjoint.income.wealth.poverty.bn.w9, family = "binomial", data = data)
m4_miss <- glm(POV.CLUST.Mainly_missing___not_observed ~ highest_lifetime_ISCO_88_recoded + cohort + gender.rcd +
                 valid.information.wjoint.income.wealth.poverty.bn.w2 +
                 valid.information.wjoint.income.wealth.poverty.bn.w4 +
                 valid.information.wjoint.income.wealth.poverty.bn.w5 +
                 valid.information.wjoint.income.wealth.poverty.bn.w6 +
                 valid.information.wjoint.income.wealth.poverty.bn.w7 +
                 valid.information.wjoint.income.wealth.poverty.bn.w8 +
                 valid.information.wjoint.income.wealth.poverty.bn.w9, family = "binomial", data = data)

# Compute Odds Ratios
coef_m1_miss <- exp(coef(m1_miss))
coef_m2_miss <- exp(coef(m2_miss))
coef_m3_miss <- exp(coef(m3_miss))
coef_m4_miss <- exp(coef(m4_miss))

# Stargazer Table
stargazer(
  m1_miss, m2_miss, m3_miss, m4_miss,
  coef = list(coef_m1_miss, coef_m2_miss, coef_m3_miss, coef_m4_miss),
  type = "text", 
  report = "vc*",
  omit = c("valid*", "cohort*", "Constant", "gender*"),
  single.row = TRUE,
  p.auto = FALSE,
  digits = 2,
  dep.var.labels = "Mainly missing / not observed", 
  dep.var.caption = "Poverty trajectory type",
  covariate.labels = c("Years of education", "Highly skilled occupation (ref. medium)", "Low skilled occupation")
)

# --- POV.CLUST.Mainly_missing_to_non_poor ---
m1_mtp <- glm(POV.CLUST.Mainly_missing_to_non_poor ~ eduyears, family = "binomial", data = data)
m2_mtp <- glm(POV.CLUST.Mainly_missing_to_non_poor ~ highest_lifetime_ISCO_88_recoded, family = "binomial", data = data)
m3_mtp <- glm(POV.CLUST.Mainly_missing_to_non_poor ~ eduyears + cohort + gender.rcd +
                valid.information.wjoint.income.wealth.poverty.bn.w2 +
                valid.information.wjoint.income.wealth.poverty.bn.w4 +
                valid.information.wjoint.income.wealth.poverty.bn.w5 +
                valid.information.wjoint.income.wealth.poverty.bn.w6 +
                valid.information.wjoint.income.wealth.poverty.bn.w7 +
                valid.information.wjoint.income.wealth.poverty.bn.w8 +
                valid.information.wjoint.income.wealth.poverty.bn.w9, family = "binomial", data = data)
m4_mtp <- glm(POV.CLUST.Mainly_missing_to_non_poor ~ highest_lifetime_ISCO_88_recoded + cohort + gender.rcd +
                valid.information.wjoint.income.wealth.poverty.bn.w2 +
                valid.information.wjoint.income.wealth.poverty.bn.w4 +
                valid.information.wjoint.income.wealth.poverty.bn.w5 +
                valid.information.wjoint.income.wealth.poverty.bn.w6 +
                valid.information.wjoint.income.wealth.poverty.bn.w7 +
                valid.information.wjoint.income.wealth.poverty.bn.w8 +
                valid.information.wjoint.income.wealth.poverty.bn.w9, family = "binomial", data = data)

coef_m1_mtp <- exp(coef(m1_mtp))
coef_m2_mtp <- exp(coef(m2_mtp))
coef_m3_mtp <- exp(coef(m3_mtp))
coef_m4_mtp <- exp(coef(m4_mtp))

stargazer(
  m1_mtp, m2_mtp, m3_mtp, m4_mtp,
  coef = list(coef_m1_mtp, coef_m2_mtp, coef_m3_mtp, coef_m4_mtp),
  type = "text", 
  report = "vc*",
  omit = c("valid*", "cohort*", "Constant", "gender*"),
  single.row = TRUE,
  p.auto = FALSE,
  digits = 2,
  dep.var.labels = "Mainly missing to non-poor", 
  dep.var.caption = "Poverty trajectory type",
  covariate.labels = c("Years of education", "Highly skilled occupation (ref. medium)", "Low skilled occupation")
)


library(stargazer)

# --- POV.CLUST.Mainly_economically_vulnerable ---
m1_vuln <- glm(POV.CLUST.Mainly_economically_vulnerable ~ eduyears, family = "binomial", data = data)
m2_vuln <- glm(POV.CLUST.Mainly_economically_vulnerable ~ highest_lifetime_ISCO_88_recoded, family = "binomial", data = data)
m3_vuln <- glm(POV.CLUST.Mainly_economically_vulnerable ~ eduyears + cohort + gender.rcd +
                 valid.information.wjoint.income.wealth.poverty.bn.w2 +
                 valid.information.wjoint.income.wealth.poverty.bn.w4 +
                 valid.information.wjoint.income.wealth.poverty.bn.w5 +
                 valid.information.wjoint.income.wealth.poverty.bn.w6 +
                 valid.information.wjoint.income.wealth.poverty.bn.w7 +
                 valid.information.wjoint.income.wealth.poverty.bn.w8 +
                 valid.information.wjoint.income.wealth.poverty.bn.w9, family = "binomial", data = data)
m4_vuln <- glm(POV.CLUST.Mainly_economically_vulnerable ~ highest_lifetime_ISCO_88_recoded + cohort + gender.rcd +
                 valid.information.wjoint.income.wealth.poverty.bn.w2 +
                 valid.information.wjoint.income.wealth.poverty.bn.w4 +
                 valid.information.wjoint.income.wealth.poverty.bn.w5 +
                 valid.information.wjoint.income.wealth.poverty.bn.w6 +
                 valid.information.wjoint.income.wealth.poverty.bn.w7 +
                 valid.information.wjoint.income.wealth.poverty.bn.w8 +
                 valid.information.wjoint.income.wealth.poverty.bn.w9, family = "binomial", data = data)

coef_m1_vuln <- exp(coef(m1_vuln))
coef_m2_vuln <- exp(coef(m2_vuln))
coef_m3_vuln <- exp(coef(m3_vuln))
coef_m4_vuln <- exp(coef(m4_vuln))

stargazer(
  m1_vuln, m2_vuln, m3_vuln, m4_vuln,
  coef = list(coef_m1_vuln, coef_m2_vuln, coef_m3_vuln, coef_m4_vuln),
  type = "text", 
  report = "vc*",
  omit = c("valid*", "cohort*", "Constant", "gender*"),
  single.row = TRUE,
  p.auto = FALSE,
  digits = 2,
  dep.var.labels = "Mainly economically vulnerable", 
  dep.var.caption = "Poverty trajectory type",
  covariate.labels = c("Years of education", "Highly skilled occupation (ref. medium)", "Low skilled occupation")
)

# --- POV.CLUST.Mainly_non_poor_then_not_observed ---
m1_npno <- glm(POV.CLUST.Mainly_non_poor_then_not_observed ~ eduyears, family = "binomial", data = data)
m2_npno <- glm(POV.CLUST.Mainly_non_poor_then_not_observed ~ highest_lifetime_ISCO_88_recoded, family = "binomial", data = data)
m3_npno <- glm(POV.CLUST.Mainly_non_poor_then_not_observed ~ eduyears + cohort + gender.rcd +
                 valid.information.wjoint.income.wealth.poverty.bn.w2 +
                 valid.information.wjoint.income.wealth.poverty.bn.w4 +
                 valid.information.wjoint.income.wealth.poverty.bn.w5 +
                 valid.information.wjoint.income.wealth.poverty.bn.w6 +
                 valid.information.wjoint.income.wealth.poverty.bn.w7 +
                 valid.information.wjoint.income.wealth.poverty.bn.w8 +
                 valid.information.wjoint.income.wealth.poverty.bn.w9, family = "binomial", data = data)
m4_npno <- glm(POV.CLUST.Mainly_non_poor_then_not_observed ~ highest_lifetime_ISCO_88_recoded + cohort + gender.rcd +
                 valid.information.wjoint.income.wealth.poverty.bn.w2 +
                 valid.information.wjoint.income.wealth.poverty.bn.w4 +
                 valid.information.wjoint.income.wealth.poverty.bn.w5 +
                 valid.information.wjoint.income.wealth.poverty.bn.w6 +
                 valid.information.wjoint.income.wealth.poverty.bn.w7 +
                 valid.information.wjoint.income.wealth.poverty.bn.w8 +
                 valid.information.wjoint.income.wealth.poverty.bn.w9, family = "binomial", data = data)

coef_m1_npno <- exp(coef(m1_npno))
coef_m2_npno <- exp(coef(m2_npno))
coef_m3_npno <- exp(coef(m3_npno))
coef_m4_npno <- exp(coef(m4_npno))

stargazer(
  m1_npno, m2_npno, m3_npno, m4_npno,
  coef = list(coef_m1_npno, coef_m2_npno, coef_m3_npno, coef_m4_npno),
  type = "text", 
  report = "vc*",
  omit = c("valid*", "cohort*", "Constant", "gender*"),
  single.row = TRUE,
  p.auto = FALSE,
  digits = 2,
  dep.var.labels = "Mainly non-poor then not observed", 
  dep.var.caption = "Poverty trajectory type",
  covariate.labels = c("Years of education", "Highly skilled occupation (ref. medium)", "Low skilled occupation")
)


library(stargazer)

# --- POV.CLUST.Mainly_economically_vulnerable ---
# Models and stargazer table already included

# --- POV.CLUST.Mainly_non_poor_then_not_observed ---
# Models and stargazer table already included

# --- POV.CLUST.Mainly_protected_poor ---
m1_pp <- glm(POV.CLUST.Mainly_protected_poor ~ eduyears, family = "binomial", data = data)
m2_pp <- glm(POV.CLUST.Mainly_protected_poor ~ highest_lifetime_ISCO_88_recoded, family = "binomial", data = data)
m3_pp <- glm(POV.CLUST.Mainly_protected_poor ~ eduyears + cohort + gender.rcd +
               valid.information.wjoint.income.wealth.poverty.bn.w2 +
               valid.information.wjoint.income.wealth.poverty.bn.w4 +
               valid.information.wjoint.income.wealth.poverty.bn.w5 +
               valid.information.wjoint.income.wealth.poverty.bn.w6 +
               valid.information.wjoint.income.wealth.poverty.bn.w7 +
               valid.information.wjoint.income.wealth.poverty.bn.w8 +
               valid.information.wjoint.income.wealth.poverty.bn.w9, family = "binomial", data = data)
m4_pp <- glm(POV.CLUST.Mainly_protected_poor ~ highest_lifetime_ISCO_88_recoded + cohort + gender.rcd +
               valid.information.wjoint.income.wealth.poverty.bn.w2 +
               valid.information.wjoint.income.wealth.poverty.bn.w4 +
               valid.information.wjoint.income.wealth.poverty.bn.w5 +
               valid.information.wjoint.income.wealth.poverty.bn.w6 +
               valid.information.wjoint.income.wealth.poverty.bn.w7 +
               valid.information.wjoint.income.wealth.poverty.bn.w8 +
               valid.information.wjoint.income.wealth.poverty.bn.w9, family = "binomial", data = data)

coef_m1_pp <- exp(coef(m1_pp))
coef_m2_pp <- exp(coef(m2_pp))
coef_m3_pp <- exp(coef(m3_pp))
coef_m4_pp <- exp(coef(m4_pp))

stargazer(
  m1_pp, m2_pp, m3_pp, m4_pp,
  coef = list(coef_m1_pp, coef_m2_pp, coef_m3_pp, coef_m4_pp),
  type = "text", 
  report = "vc*",
  omit = c("valid*", "cohort*", "Constant", "gender*"),
  single.row = TRUE,
  p.auto = FALSE,
  digits = 2,
  dep.var.labels = "Mainly protected poor", 
  dep.var.caption = "Poverty trajectory type",
  covariate.labels = c("Years of education", "Highly skilled occupation (ref. medium)", "Low skilled occupation")
)

# --- POV.CLUST.Missing___not_observed_to_non_poor ---
m1_mnp <- glm(POV.CLUST.Missing___not_observed_to_non_poor ~ eduyears, family = "binomial", data = data)
m2_mnp <- glm(POV.CLUST.Missing___not_observed_to_non_poor ~ highest_lifetime_ISCO_88_recoded, family = "binomial", data = data)
m3_mnp <- glm(POV.CLUST.Missing___not_observed_to_non_poor ~ eduyears + cohort + gender.rcd +
                valid.information.wjoint.income.wealth.poverty.bn.w2 +
                valid.information.wjoint.income.wealth.poverty.bn.w4 +
                valid.information.wjoint.income.wealth.poverty.bn.w5 +
                valid.information.wjoint.income.wealth.poverty.bn.w6 +
                valid.information.wjoint.income.wealth.poverty.bn.w7 +
                valid.information.wjoint.income.wealth.poverty.bn.w8 +
                valid.information.wjoint.income.wealth.poverty.bn.w9, family = "binomial", data = data)
m4_mnp <- glm(POV.CLUST.Missing___not_observed_to_non_poor ~ highest_lifetime_ISCO_88_recoded + cohort + gender.rcd +
                valid.information.wjoint.income.wealth.poverty.bn.w2 +
                valid.information.wjoint.income.wealth.poverty.bn.w4 +
                valid.information.wjoint.income.wealth.poverty.bn.w5 +
                valid.information.wjoint.income.wealth.poverty.bn.w6 +
                valid.information.wjoint.income.wealth.poverty.bn.w7 +
                valid.information.wjoint.income.wealth.poverty.bn.w8 +
                valid.information.wjoint.income.wealth.poverty.bn.w9, family = "binomial", data = data)

coef_m1_mnp <- exp(coef(m1_mnp))
coef_m2_mnp <- exp(coef(m2_mnp))
coef_m3_mnp <- exp(coef(m3_mnp))
coef_m4_mnp <- exp(coef(m4_mnp))

stargazer(
  m1_mnp, m2_mnp, m3_mnp, m4_mnp,
  coef = list(coef_m1_mnp, coef_m2_mnp, coef_m3_mnp, coef_m4_mnp),
  type = "text", 
  report = "vc*",
  omit = c("valid*", "cohort*", "Constant", "gender*"),
  single.row = TRUE,
  p.auto = FALSE,
  digits = 2,
  dep.var.labels = "Missing / not observed to non-poor", 
  dep.var.caption = "Poverty trajectory type",
  covariate.labels = c("Years of education", "Highly skilled occupation (ref. medium)", "Low skilled occupation")
)

# Remaining variables:
# POV.CLUST.Non_poor_or_protected_poor_then_not_observed
# Repeat this block structure for each remaining variable with appropriate model names.

library(stargazer)

# --- POV.CLUST.Mainly_economically_vulnerable ---
# Models and stargazer table already included

# --- POV.CLUST.Mainly_non_poor_then_not_observed ---
# Models and stargazer table already included

# --- POV.CLUST.Mainly_protected_poor ---
# Models and stargazer table already included

# --- POV.CLUST.Missing___not_observed_to_non_poor ---
# Models and stargazer table already included

# --- POV.CLUST.Non_poor_or_protected_poor_then_not_observed ---
m1_npp <- glm(POV.CLUST.Non_poor_or_protected_poor_then_not_observed ~ eduyears, family = "binomial", data = data)
m2_npp <- glm(POV.CLUST.Non_poor_or_protected_poor_then_not_observed ~ highest_lifetime_ISCO_88_recoded, family = "binomial", data = data)
m3_npp <- glm(POV.CLUST.Non_poor_or_protected_poor_then_not_observed ~ eduyears + cohort + gender.rcd +
                valid.information.wjoint.income.wealth.poverty.bn.w2 +
                valid.information.wjoint.income.wealth.poverty.bn.w4 +
                valid.information.wjoint.income.wealth.poverty.bn.w5 +
                valid.information.wjoint.income.wealth.poverty.bn.w6 +
                valid.information.wjoint.income.wealth.poverty.bn.w7 +
                valid.information.wjoint.income.wealth.poverty.bn.w8 +
                valid.information.wjoint.income.wealth.poverty.bn.w9, family = "binomial", data = data)
m4_npp <- glm(POV.CLUST.Non_poor_or_protected_poor_then_not_observed ~ highest_lifetime_ISCO_88_recoded + cohort + gender.rcd +
                valid.information.wjoint.income.wealth.poverty.bn.w2 +
                valid.information.wjoint.income.wealth.poverty.bn.w4 +
                valid.information.wjoint.income.wealth.poverty.bn.w5 +
                valid.information.wjoint.income.wealth.poverty.bn.w6 +
                valid.information.wjoint.income.wealth.poverty.bn.w7 +
                valid.information.wjoint.income.wealth.poverty.bn.w8 +
                valid.information.wjoint.income.wealth.poverty.bn.w9, family = "binomial", data = data)

coef_m1_npp <- exp(coef(m1_npp))
coef_m2_npp <- exp(coef(m2_npp))
coef_m3_npp <- exp(coef(m3_npp))
coef_m4_npp <- exp(coef(m4_npp))

stargazer(
  m1_npp, m2_npp, m3_npp, m4_npp,
  coef = list(coef_m1_npp, coef_m2_npp, coef_m3_npp, coef_m4_npp),
  type = "text", 
  report = "vc*",
  omit = c("valid*", "cohort*", "Constant", "gender*"),
  single.row = TRUE,
  p.auto = FALSE,
  digits = 2,
  dep.var.labels = "Non-poor or protected poor then not observed", 
  dep.var.caption = "Poverty trajectory type",
  covariate.labels = c("Years of education", "Highly skilled occupation (ref. medium)", "Low skilled occupation")
)

# All variables have now been included in the analysis.



# consolidated table   -------------------------------------------------------------

library(stargazer)

# Export consolidated tables as HTML


library(stargazer)

# Function to exponentiate model coefficients
exp_model_coeffs <- function(model) {
  model$coefficients <- exp(model$coefficients)
  return(model)
}

# Shorten model names and exponentiate coefficients
m1_1 <- exp_model_coeffs(m1_vuln); m2_1 <- exp_model_coeffs(m2_vuln); m3_1 <- exp_model_coeffs(m3_vuln); m4_1 <- exp_model_coeffs(m4_vuln);
m1_2 <- exp_model_coeffs(m1_miss); m2_2 <- exp_model_coeffs(m2_miss); m3_2 <- exp_model_coeffs(m3_miss); m4_2 <- exp_model_coeffs(m4_miss);
m1_3 <- exp_model_coeffs(m1_mtp); m2_3 <- exp_model_coeffs(m2_mtp); m3_3 <- exp_model_coeffs(m3_mtp); m4_3 <- exp_model_coeffs(m4_mtp);
m1_4 <- exp_model_coeffs(m1_npno); m2_4 <- exp_model_coeffs(m2_npno); m3_4 <- exp_model_coeffs(m3_npno); m4_4 <- exp_model_coeffs(m4_npno);
m1_5 <- exp_model_coeffs(m1_pp); m2_5 <- exp_model_coeffs(m2_pp); m3_5 <- exp_model_coeffs(m3_pp); m4_5 <- exp_model_coeffs(m4_pp);
m1_6 <- exp_model_coeffs(m1_mnp); m2_6 <- exp_model_coeffs(m2_mnp); m3_6 <- exp_model_coeffs(m3_mnp); m4_6 <- exp_model_coeffs(m4_mnp);
m1_7 <- exp_model_coeffs(m1_npp); m2_7 <- exp_model_coeffs(m2_npp); m3_7 <- exp_model_coeffs(m3_npp); m4_7 <- exp_model_coeffs(m4_npp);

# --- Consolidated Table 1: Indicators 1-2 ---
stargazer(
  m1_1, m2_1, m3_1, m4_1,
  m1_2, m2_2, m3_2, m4_2,
  type = "html", 
  out = "stargazer_table_1.html",
  report = "vc*",
  omit = c("valid*", "cohort*", "Constant", "gender*"),
  single.row = TRUE,
  p.auto = FALSE,
  digits = 2,
  dep.var.caption = "Poverty Trajectory Type - Indicators 1-2",
  covariate.labels = c("Years of education", "Highly skilled occupation (ref. medium)", "Low skilled occupation")
)

# --- Consolidated Table 2: Indicators 3-4 ---
stargazer(
  m1_3, m2_3, m3_3, m4_3,
  m1_4, m2_4, m3_4, m4_4,
  type = "html", 
  out = "stargazer_table_2.html",
  report = "vc*",
  omit = c("valid*", "cohort*", "Constant", "gender*"),
  single.row = TRUE,
  p.auto = FALSE,
  digits = 2,
  dep.var.caption = "Poverty Trajectory Type - Indicators 3-4",
  covariate.labels = c("Years of education", "Highly skilled occupation (ref. medium)", "Low skilled occupation")
)

# --- Consolidated Table 3: Indicators 5-6 ---
stargazer(
  m1_5, m2_5, m3_5, m4_5,
  m1_6, m2_6, m3_6, m4_6,
  type = "html", 
  out = "stargazer_table_3.html",
  report = "vc*",
  omit = c("valid*", "cohort*", "Constant", "gender*"),
  single.row = TRUE,
  p.auto = FALSE,
  digits = 2,
  dep.var.caption = "Poverty Trajectory Type - Indicators 5-6",
  covariate.labels = c("Years of education", "Highly skilled occupation (ref. medium)", "Low skilled occupation")
)

# --- Consolidated Table 4: Indicators 7-8 ---
stargazer(
  m1_7, m2_7, m3_7, m4_7,
  type = "html", 
  out = "stargazer_table_4.html",
  report = "vc*",
  omit = c("valid*", "cohort*", "Constant", "gender*"),
  single.row = TRUE,
  p.auto = FALSE,
  digits = 2,
  dep.var.caption = "Poverty Trajectory Type - Indicators 7-8",
  covariate.labels = c("Years of education", "Highly skilled occupation (ref. medium)", "Low skilled occupation")
)

