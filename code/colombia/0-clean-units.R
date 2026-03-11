#--------------------------------------------------------------------------------------------------------------
                            # 1. all loaded packages come here (resolve warnings in the file)
#--------------------------------------------------------------------------------------------------------------
suppressMessages({
  library(tidyverse)
  library(haven)
})

#--------------------------------------------------------------------------------------------------------------
                            # 2. read raw data
#--------------------------------------------------------------------------------------------------------------
col_ids <- read_dta("data/in/colombia/05_Raw De-Indentified Data/COL_MK4_cuadrante_id_link.dta")
col_units <- read.csv("data/in/colombia/02_Randomization/COL_MK4_assignment_data.csv", stringsAsFactors = FALSE)
col_meetings <- read.csv("data/in/colombia/06_Raw Administrative Data/COL_MK4_raw_data_meeting_log.csv", stringsAsFactors = FALSE)
col_cuadrantes <- read.csv("data/in/colombia/02_Randomization/cuadrantes031218.csv", stringsAsFactors = FALSE)

#--------------------------------------------------------------------------------------------------------------
                            # 3. clean column ID's, treatment data and merge
#--------------------------------------------------------------------------------------------------------------
col_ids <- 
  col_ids %>% 
  select(cuadrante = nro_cuadra, invamer_id, tratamiento, t_palabras)

col_meetings <- 
  col_meetings %>% 
  filter(MeetingNumber == 1) %>%
  mutate(Date = as.character(`DateOfMeeting..D.M.Y.`),
         year = 2000 + as.numeric(as.character(substr(x = Date, start = nchar(Date)-1, stop = nchar(Date)))),
         year = ifelse(year == 2008, 2018, year),
         dm = gsub("(.*)/.*", "\\1",Date),
         day = as.numeric(as.character(gsub("(.*)/.*", "\\1",dm))),
         month = substr(dm, nchar(dm)-1, nchar(dm)),
         month = as.numeric(as.character(ifelse(grepl(month, pattern = "/"), substr(month, start = 2, stop = 2), month))),
         first_mtg = as.Date(paste0(month, "/", day, "/", year), format = "%m/%d/%Y")) %>%
  select(-dm) %>%
  group_by(blockids) %>%
  summarize(first_mtg_block = min(first_mtg))

col_units <- 
  col_units %>% 
  mutate(cuadrante = as.character(cuadrante)) %>% 
  left_join(col_meetings, by = "blockids") %>% 
  select(cuadrante, block_ID = blockids, population, first_mtg_block) %>% 
  inner_join(col_ids, by = "cuadrante") %>% 
  mutate(invamer_id = as.integer(invamer_id)) %>% 
  select(cuadrante_chr = cuadrante, cuadrante = invamer_id, treatment = tratamiento, treatment_chr = t_palabras, block_ID, population, first_mtg_block) %>% 
  inner_join(col_cuadrantes %>% select(cuadrante_chr = NRO_CUADRA, ESTACION), by = "cuadrante_chr") %>% 
  mutate(
    Z_common = if_else((treatment == 1 | treatment == 3), 1, 0),
    Z_alt = if_else(treatment == 2 | treatment == 3, 1, 0),
    Z_control = if_else(treatment == 0, 1, 0),
    Z = case_when(
      Z_common == 0 & Z_alt == 0 ~ 0L,
      Z_common == 1 & Z_alt == 0 ~ 1L,
      Z_common == 0 & Z_alt == 1 ~ 2L, 
      Z_common == 1 & Z_alt == 1 ~ 3L))

#--------------------------------------------------------------------------------------------------------------
                              # 4. save clean data
#--------------------------------------------------------------------------------------------------------------
saveRDS(col_units,  file = "data/out/col-unit-clean.RDS")
