#--------------------------------------------------------------------------------------------------------------
# load packages
#--------------------------------------------------------------------------------------------------------------
suppressMessages({
  library(tidyverse)
})

#--------------------------------------------------------------------------------------------------------------
# read all data required
#--------------------------------------------------------------------------------------------------------------
# officer data
col_officer <- read.csv("data/in/colombia/05_Raw De-Indentified Data/COL_MK4_raw_data_endline_police.csv")
col_units <- readRDS("data/out/col-unit-clean.RDS")
col_cuads <- read.csv("data/in/colombia/02_Randomization/cuadrantes031218.csv")

#--------------------------------------------------------------------------------------------------------------
# read and officer crime files
#--------------------------------------------------------------------------------------------------------------
col_officer <- 
  col_officer %>% 
  as_tibble() %>% 
  rename_at(vars(everything()), ~paste0("P_", str_remove(names(col_officer), pattern = "X"))) %>% 
  mutate(
    ESTACION = case_when(
      str_detect(P_, "ARANJUEZ") ~ "ARANJUEZ",
      str_detect(P_, "BELEN") ~ "BELEN",
      str_detect(P_, "BUENOS AIRES") ~ "BUENOS AIRES",
      str_detect(P_, "CANDELARIA") ~ "CANDELARIA",
      str_detect(P_, "DOCE") ~ "DOCE DE OCTUBRE",
      str_detect(P_, "LAURELES") ~ "LAURELES",
      str_detect(P_, "MANRIQUE") ~ "MANRIQUE",
      str_detect(P_, "POBLADO") ~ "POBLADO",
      str_detect(P_, "POPULAR") ~ "POPULAR",
      str_detect(P_, "SAPRADO") ~ "S.A PRADO	",
      str_detect(P_, "JAVIER") ~ "SAN JAVIER",
      str_detect(P_, "SANTA CRUZ") ~ "SANTA CRUZ",
      str_detect(P_, "VILLA HERMOSA") ~ "VILLA HERMOSA",
      TRUE ~ NA_character_)) %>%
  rename(cuadrante = P_Cuad1) %>%
  left_join(col_cuads %>% rename(cuadrante = CUADRANTE), by = c("ESTACION", "cuadrante")) %>% 
  select(-cuadrante) %>% 
  right_join(col_units %>% rename(NRO_CUADRA = cuadrante_chr), by = c("NRO_CUADRA")) %>%
  mutate(
    officer_rotation = P_19,
    empathy_complaints = P_3D - 1,
    # the following is a comment by the team: "there is a typo in the raw data -- checked with original (paper) instrument"
    empathy_reports = ifelse(P_3E == 5, 3, P_3E - 1),
    account_pol_matter = P_5D - 1,
    account_pol_matter = ifelse(P_5D == 4, 3, P_5D),
    
    hypothetical2_punishment = P_7C.1_class,
    hypothetical2_reportself = P_7D - 1,
    hypothetical2_reportothers = P_7E - 1,
    
    hypothetical3_punishment = P_8C.1_class,
    hypothetical3_reportself = P_8D - 1,
    hypothetical3_reportothers = P_8E - 1,
    hypothetical5_punishment = P_9C.1_class,
    
    hypothetical5_reportself = P_9D - 1,
    hypothetical5_reportothers = P_9E - 1,
    hypothetical5_abuseself = 4 - P_9A,
    hypothetical5_abuseother = 4 - P_9B,
    
    hypothetical2_corruptself = 4 - P_7A,
    hypothetical2_corruptother = 4 - P_7B,
    hypothetical3_corruptself = 4 - P_8A,
    hypothetical3_corruptother = 4 - P_8B)

saveRDS(col_officer, file = "data/out/col-officer-clean.RDS")
