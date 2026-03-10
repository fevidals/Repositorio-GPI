#--------------------------------------------------------------------------------------------------------------
# all loaded packages come here (resolve warnings in the file)
#--------------------------------------------------------------------------------------------------------------
suppressMessages({
  library(tidyverse)
  library(haven)
})

#--------------------------------------------------------------------------------------------------------------
# get raw data
#--------------------------------------------------------------------------------------------------------------
uga_treatment <- read.csv("data/in/uganda/02_Randomization/UGA_MK4_treatment_status.csv", stringsAsFactors = F)
uga_treatment_blocks1 <- read.csv("data/in/uganda/02_Randomization/blocks1.csv",stringsAsFactors = F)
uga_treatment_blocks2 <- read.csv("data/in/uganda/02_Randomization/blocks2.csv",stringsAsFactors = F)
uga_treatment_blocks3 <- read.csv("data/in/uganda/02_Randomization/blocks3.csv",stringsAsFactors = F)
uga_treatment_blocks4 <- read.csv("data/in/uganda/02_Randomization/blocks4.csv",stringsAsFactors = F)
uga_unit <- read_csv("data/in/uganda/02_Randomization/UGA_MK4_baseline_used_for_assignment.csv")

#--------------------------------------------------------------------------------------------------------------
# treatment: clean
#--------------------------------------------------------------------------------------------------------------
blocks <- 
  bind_rows(
    uga_treatment_blocks1,
    uga_treatment_blocks2,
    uga_treatment_blocks3,
    uga_treatment_blocks4) %>% 
  mutate(block_ID = 1:n()) %>% 
  select(- X, - Max.Distance) %>% 
  pivot_longer(- block_ID, values_to = "station_id") %>% 
  select(block_ID, station_id) %>% 
  # join to get population data
  inner_join(uga_unit %>% transmute(station_id, population = ipopulation, by = "station_id")) %>% 
  # manually correct one incorrect station ID (please refer to team code i.e. UGA_MK4_data_cleaning_code_01_treatment_assignment)
  mutate(
    station_id = as.character(station_id),
    block_id_station = if_else(station_id == "201", "203", station_id)
  )


uga_treatment <- 
  uga_treatment %>% 
  mutate(
    block_id_station = substr(unit_id, 4, 6)) %>% 
  left_join(blocks, by = "block_id_station") %>% 
  transmute(
    station_id = unit_id,
    block_ID,
    village_id,
    population,
    Z_common = treatment,
    Z_alt = subtreatment,
    Z = case_when(
      Z_common == 0 ~ 0,
      Z_common == 1 & Z_alt == 0 ~ 1,
      Z_common == 1 & Z_alt == 1 ~ 2
    )
  ) %>% 
  mutate(Z_control = if_else(Z_common == 0 & Z_alt == 0, 1, 0))

saveRDS(uga_treatment,  file = "data/out/uga-unit-clean.RDS")
