install.packages("pwr")
library(pwr)

pwr.f2.test(u = 6, # Number of predictors
            v = NULL, # Sample size will be calculated
            f2 = 0.01, # Small effect size (0.02 = small, 0.15 = medium, 0.35 = large)
            sig.level = 0.05,
            power = 0.8)
