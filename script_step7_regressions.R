
library(stargazer)
library(car)

# load data  --------------------------------------------------------------


rm(list=ls())
load(file="data_step3-out-variables-all-setup.Rdata")

# Modellieren Part 1 -------------------------------------------------------------

# listing the variables as an aide mémoire 

# dependent variables 
levels(as.factor(data$poverty.trajectories.clusters))
data$POV.CLUST.Mainly_economically_vulnerable
data$POV.CLUST.Mainly_Non_poor
#independent variables
data$eduyears
data$highest_lifetime_ISCO_88_recoded

# confounder variables
data$cohort
data$rabyear
data$gender.rcd
table(data$valid.information.wjoint.income.wealth.poverty.bn.w2)


# mocdelling --------------------------------------------------------------

m1.POV.CLUST.Mainly_economically_vulnerable <- glm(POV.CLUST.Mainly_economically_vulnerable~eduyears,family="binomial",data=data)
summary(m1.POV.CLUST.Mainly_economically_vulnerable)

m2.POV.CLUST.Mainly_economically_vulnerable <- glm(POV.CLUST.Mainly_economically_vulnerable~highest_lifetime_ISCO_88_recoded,family="binomial",data=data)
summary(m2.POV.CLUST.Mainly_economically_vulnerable)

m3.POV.CLUST.Mainly_economically_vulnerable <- glm(POV.CLUST.Mainly_economically_vulnerable~eduyears+cohort+
                                                        gender.rcd+
                                                        valid.information.wjoint.income.wealth.poverty.bn.w2+
                                                        valid.information.wjoint.income.wealth.poverty.bn.w4+
                                                        valid.information.wjoint.income.wealth.poverty.bn.w5+
                                                        valid.information.wjoint.income.wealth.poverty.bn.w6+
                                                        valid.information.wjoint.income.wealth.poverty.bn.w7+
                                                        valid.information.wjoint.income.wealth.poverty.bn.w8+
                                                        valid.information.wjoint.income.wealth.poverty.bn.w9,family="binomial",data=data)
summary(m3.POV.CLUST.Mainly_economically_vulnerable)

m4.POV.CLUST.Mainly_economically_vulnerable <- glm(POV.CLUST.Mainly_economically_vulnerable~highest_lifetime_ISCO_88_recoded+cohort+
                                                        gender.rcd+
                                                        valid.information.wjoint.income.wealth.poverty.bn.w2+
                                                        valid.information.wjoint.income.wealth.poverty.bn.w4+
                                                        valid.information.wjoint.income.wealth.poverty.bn.w5+
                                                        valid.information.wjoint.income.wealth.poverty.bn.w6+
                                                        valid.information.wjoint.income.wealth.poverty.bn.w7+
                                                        valid.information.wjoint.income.wealth.poverty.bn.w8+
                                                        valid.information.wjoint.income.wealth.poverty.bn.w9,family="binomial",data=data)
summary(m4.POV.CLUST.Mainly_economically_vulnerable)

coef.m1.POV.CLUST.Mainly_economically_vulnerable <- exp(coef(m1.POV.CLUST.Mainly_economically_vulnerable))
coef.m2.POV.CLUST.Mainly_economically_vulnerable <- exp(coef(m2.POV.CLUST.Mainly_economically_vulnerable))
coef.m3.POV.CLUST.Mainly_economically_vulnerable <- exp(coef(m3.POV.CLUST.Mainly_economically_vulnerable))
coef.m4.POV.CLUST.Mainly_economically_vulnerable <- exp(coef(m4.POV.CLUST.Mainly_economically_vulnerable))

stargazer(m1.POV.CLUST.Mainly_economically_vulnerable,
          m2.POV.CLUST.Mainly_economically_vulnerable,
          m3.POV.CLUST.Mainly_economically_vulnerable,
          m4.POV.CLUST.Mainly_economically_vulnerable,
          coef=list(
            coef.m1.POV.CLUST.Mainly_economically_vulnerable,
            coef.m2.POV.CLUST.Mainly_economically_vulnerable,
            coef.m3.POV.CLUST.Mainly_economically_vulnerable,
            coef.m4.POV.CLUST.Mainly_economically_vulnerable),
          type="text", 
          report="vc*",
          omit = "valid*",
          omit = c("valid*","cohort*", "Constant", "gender*"),
          single.row=TRUE,
          p.auto = FALSE,
          digits=2
)

stargazer(m3.POV.CLUST.Mainly_economically_vulnerable,
          m2.POV.CLUST.Mainly_economically_vulnerable,
          m4.POV.CLUST.Mainly_economically_vulnerable
          ,
          coef=list(
            coef.m3.POV.CLUST.Mainly_economically_vulnerable,
            coef.m2.POV.CLUST.Mainly_economically_vulnerable
            ,
            coef.m4.POV.CLUST.Mainly_economically_vulnerable
            ),
          type="text", 
          report="vc*",
  
          single.row=TRUE,
          p.auto = FALSE,
          digits=2
)


library(broom)
library(dplyr)
install.packages("gt", dependencies = TRUE, reinstall = TRUE)
install.packages("rlang", dependencies = TRUE, type = "source")

library(gt)  # For nice tables (or use kableExtra)
install.packages("kableExtra")

library(kableExtra)

# Define function to extract results and compute odds ratios
extract_logit_results <- function(model, model_name) {
  tidy(model, conf.int = TRUE, exponentiate = TRUE) %>%  # Convert logit coefs to odds ratios
    mutate(Model = model_name) %>%  # Add model name
    select(Model, term, estimate, conf.low, conf.high, p.value)  # Keep key columns
}

# Apply function to each model
results <- bind_rows(
  extract_logit_results(m1.POV.CLUST.Mainly_economically_vulnerable, "Model 1"),
  extract_logit_results(m2.POV.CLUST.Mainly_economically_vulnerable, "Model 2"),
  extract_logit_results(m3.POV.CLUST.Mainly_economically_vulnerable, "Model 3"),
  extract_logit_results(m4.POV.CLUST.Mainly_economically_vulnerable, "Model 4")
)

# Omit unwanted variables
omit_vars <- c("valid", "cohort", "Constant", "gender")
results_clean <- results %>%
  filter(!grepl(paste(omit_vars, collapse = "|"), term)) %>%  # Remove unwanted variables
  mutate(across(c(estimate, conf.low, conf.high), ~ round(.x, 2)))  # Round numbers

# Format nicely with gt
results_clean %>%
  gt() %>%
  tab_header(
    title = "Odds Ratios from Logistic Regression Models"
  ) %>%
  cols_label(
    term = "Variable",
    estimate = "Odds Ratio",
    conf.low = "Lower 95% CI",
    conf.high = "Upper 95% CI",
    p.value = "p-value"
  ) %>%
  fmt_number(columns = c(estimate, conf.low, conf.high, p.value), decimals = 2) %>%
  opt_table_font(font = "Arial")






# consolidated table   -------------------------------------------------------------


coef.m1.POV.CLUST.Mainly_economically_vulnerable <- exp(coef(m1.POV.CLUST.Mainly_economically_vulnerable))
coef.m2.POV.CLUST.Mainly_economically_vulnerable <- exp(coef(m2.POV.CLUST.Mainly_economically_vulnerable))
coef.m3.POV.CLUST.Mainly_economically_vulnerable <- exp(coef(m3.POV.CLUST.Mainly_economically_vulnerable))
coef.m4.POV.CLUST.Mainly_economically_vulnerable <- exp(coef(m4.POV.CLUST.Mainly_economically_vulnerable))
coef.m5.POV.CLUST.Mainly_economically_vulnerable <- exp(coef(m5.POV.CLUST.Mainly_economically_vulnerable))

stargazer(m1.POV.CLUST.Mainly_economically_vulnerable,
          m2.POV.CLUST.Mainly_economically_vulnerable,
          m3.POV.CLUST.Mainly_economically_vulnerable,
          m4.POV.CLUST.Mainly_economically_vulnerable,
          m5.POV.CLUST.Mainly_economically_vulnerable
          coef=list(
            coef.m1.POV.CLUST.Mainly_economically_vulnerable,
            coef.m2.POV.CLUST.Mainly_economically_vulnerable,
            coef.m3.POV.CLUST.Mainly_economically_vulnerable,
            coef.m4.POV.CLUST.Mainly_economically_vulnerable, 
            coef.m5.POV.CLUST.Mainly_economically_vulnerable),
          type="html", 
          report="vc*",
          single.row=TRUE,
          p.auto = FALSE,
          digits=2, 
          out="regtable_step7-out_clusters.html"
)
