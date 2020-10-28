### Last ned pakker #####
library(tidyverse)
library(broom)

### Last inn data ####
hypertrophy <- read.csv("./data/hypertrophy.csv", na = NA)

### Valg av data ####
var_interest <- c("CLUSTER", "AVG_CSA_T1")

dat1 <- hypertrophy %>%
  select(all_of(var_interest)) %>%
  mutate(CLUSTER = factor(CLUSTER, levels = c("LOW", "HIGH"))) %>%
  drop_na(CLUSTER, AVG_CSA_T1) %>%
  print()


### T-test ####
ttest <- t.test(AVG_CSA_T1 ~ CLUSTER, data = dat1, var.equal = TRUE)

tidy(ttest) %>%
  select(-estimate, -alternative, -method) %>%
  kable(col.names = c("Gj.snitt LOW", 
                      "Gj.snitt HIGH", 
                      "T-verdi", 
                      "P-verdi", 
                      "DF", 
                      "CI LOW", 
                      "CI High"))
  
  

### Regression model ######
regmod <- lm(AVG_CSA_T1 ~ CLUSTER, data = dat1)

tidy(regmod) %>%
  kable(col.names = c("", "Estimate", "SE", "t-statistic", "p-value"), 
        digits = c(NA, 1, 1, 2, 3)) 
  

