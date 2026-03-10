#--------------------------------------------------------------------------------------------------------------
# 1. all loaded packages come here (resolve warnings in the file)
#--------------------------------------------------------------------------------------------------------------
suppressMessages({
  library(tidyverse)
  library(haven)
})

#----------------------------------------------------------------------------------------------
# 2. get all raw data
#----------------------------------------------------------------------------------------------
phl_randomization <- read.csv("data/in/philippines/06_Raw Administrative Data/TreatmentStatus_corrected.csv", stringsAsFactors = F)
phl_assignment_dta <- read_dta("data/in/philippines/05_Raw De-Indentified Data/PHI_MK4_Treatment_Assignments.dta")

load("data/in/philippines/06_Raw Administrative Data/lgu_sorsogon.Rda")
phl_unit <- lgu

#----------------------------------------------------------------------------------------------
# 3. clean all data
#----------------------------------------------------------------------------------------------
phl_randomization <- 
  phl_randomization %>% 
  mutate(psgc = psgc_code) %>% 
  left_join(phl_assignment_dta, by = "psgc") %>% 
  mutate(
    Z_common = if_else(CEP_group == "Control" & POP_group == "Control", 0L, 1L),
    Z_tipline = if_else(CEP_group == "CEP_with_SMStipline", 1L, 0L),
    Z_officer = if_else(PR_group == "With_recognition", 1L, 0L),
    Z_lgupnp = if_else(POP_group == "PNP_LGU", 1L, 0L),
    Z_mayor = if_else(pa_group == "Mayor", 1, 0),
    Z_dilg = if_else(pa_group == "DILG", 1, 0),
    
    Z_control = if_else(Z_common == 0L, 1L, 0L),
    Z = case_when(
      Z_common == 0 & Z_tipline == 0 & Z_officer == 0 & Z_lgupnp == 0 & Z_control == 1 ~ 1L,
      Z_common == 1 & Z_tipline == 0 & Z_officer == 0 & Z_lgupnp == 0 & Z_control == 0 ~ 2L,
      Z_common == 1 & Z_tipline == 0 & Z_officer == 0 & Z_lgupnp == 1 & Z_control == 0 ~ 3L,
      Z_common == 1 & Z_tipline == 0 & Z_officer == 1 & Z_lgupnp == 0 & Z_control == 0 ~ 4L,
      Z_common == 1 & Z_tipline == 0 & Z_officer == 1 & Z_lgupnp == 1 & Z_control == 0 ~ 5L,
      Z_common == 1 & Z_tipline == 1 & Z_officer == 0 & Z_lgupnp == 0 & Z_control == 0 ~ 6L,
      Z_common == 1 & Z_tipline == 1 & Z_officer == 0 & Z_lgupnp == 1 & Z_control == 0 ~ 7L,
      Z_common == 1 & Z_tipline == 1 & Z_officer == 1 & Z_lgupnp == 0 & Z_control == 0 ~ 8L,
      Z_common == 1 & Z_tipline == 1 & Z_officer == 1 & Z_lgupnp == 1 & Z_control == 0 ~ 9L,
      TRUE ~ NA_integer_)) %>% 
  # fix psgc code as highlighted by the team on june 17th titled "followup on missing data issues"
  mutate(psgc = if_else(lgu_lov == "sorsogon_salvacionwest", 56216029L, psgc_code))

phl_unit <- 
  phl_unit %>% 
  select(psgc = PSGC) %>% 
  left_join(phl_randomization, by = "psgc") %>% 
  mutate(safe_for_research = if_else(is.na(POP_group), 0, 1)) %>% 
  rename(population = population_2010)

#--------------------------------------------------------------------------------------------------------------
# Save randomization data
#--------------------------------------------------------------------------------------------------------------
saveRDS(phl_unit,  file = "data/out/phl-unit-clean.RDS")
