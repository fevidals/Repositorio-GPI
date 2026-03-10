#--------------------------------------------------------------------------------------------------------------
# load packages
#--------------------------------------------------------------------------------------------------------------
suppressMessages({
  library(tidyverse)
  library(sf)
})

#--------------------------------------------------------------------------------------------------------------
# read all data required
#--------------------------------------------------------------------------------------------------------------
# read in unit-level data about cuadrantes
col_units <- readRDS("data/out/col-unit-clean.RDS")
# read in study unit geo information
cuadrantes <- st_read("data/in/colombia/06_Raw Administrative Data/COL_MK4_raw_data_cuadrante_shapefiles/cuaranteswgs84 (2).shp", layer = "cuaranteswgs84 (2)")

#--------------------------------------------------------------------------------------------------------------
# read and clean crime files
#--------------------------------------------------------------------------------------------------------------
crime_files <- list.files("data/in/colombia/06_Raw Administrative Data/crimedata_dec2019")
# read in crime files
crime_df <- 
  str_c("data/in/colombia/06_Raw Administrative Data/crimedata_dec2019/", crime_files) %>% 
  map(read_delim, delim = ';') %>% 
  setNames(crime_files) %>% 
  map(mutate, across(c(seguridad.codigo_comuna, seguridad.grupo_actor, seguridad.sede_receptora, seguridad.modelo, seguridad.testigo), as.character)) %>% 
  bind_rows(.id = "file") %>% 
  # filter to those with a non-missing lat and lon (to merge in cuadrante)
  filter(!is.na(seguridad.longitud) & !is.na(seguridad.latitud)) %>% 
  mutate(crime_ID = 1:n())

# join to cuadrantes to add cuadrante indicator
crime_cuadrante_join <- 
  st_as_sf(crime_df, coords = c("seguridad.longitud", "seguridad.latitud"), crs = 4326) %>% 
  st_join(cuadrantes %>% select(NRO_CUADRA), join = st_within) %>% 
  # there are duplicates because of an imprecise shapefile
  group_by(crime_ID) %>% 
  mutate(n = 1:n()) %>% 
  filter(n == 1)

crime_df <- 
  crime_df %>% 
  left_join(crime_cuadrante_join %>% 
              as_tibble %>% 
              transmute(crime_ID, cuadrante_chr = NRO_CUADRA)) %>% 
  transmute(
    year = as.numeric(substr(seguridad.fecha_hecho, start = 1, stop = 4)),
    month = as.numeric(substr(seguridad.fecha_hecho, start = 6, stop = 7)),
    day = as.numeric(substr(seguridad.fecha_hecho, start = 9, stop = 10)),
    cuadrante_chr, 
    longitude = seguridad.longitud, 
    latitude = seguridad.latitud,
    file) %>% 
  mutate(
    crime = case_when(
      str_detect(file, "hurto") ~ "burglary",
      str_detect(file, "conviv") ~ "other_nonviolent",
      str_detect(file, "secues") | str_detect(file, "homici") ~ "other_violent",
      str_detect(file, "1098") | str_detect(file, "reincidencia") ~ "domestic_violence",
      str_detect(file, "123") | str_detect(file, "solicitud") ~ "tips",
      str_detect(file, "hurto") ~ "burglary",
      TRUE ~ NA_character_))

crime_summary <- 
  crime_df %>% 
  group_by(year, month, cuadrante_chr, crime) %>%
  count(name = "n_events") %>% 
  ungroup %>% 
  filter(year >= 2017) %>%
  filter(!is.na(crime)) %>% 
  pivot_wider(id_cols = c(cuadrante_chr, year, month), names_from = crime, values_from = n_events) %>% 
  rename(
    aother_num_nonviolent = other_nonviolent, 
    aburglary_num = burglary,
    aother_num_violent = other_violent,
    adomestic_phys_num = domestic_violence,
    atips_hline = tips) %>% 
  mutate(across(aburglary_num:aother_num_nonviolent, ~replace_na(., 0))) %>% 
  mutate(
    aother_num = aother_num_nonviolent + aother_num_violent,
    aviolentcrime_num = adomestic_phys_num + aother_num_violent,
    anonviolentcrime_num = aburglary_num + aother_num_nonviolent) %>% 
  readr::type_convert()

#--------------------------------------------------------------------------------------------------------------
# make panel of cuadrante-months
#--------------------------------------------------------------------------------------------------------------
panel_df <- 
  crossing(
    col_units %>% distinct(cuadrante_chr),
    tibble(year = rep(2017:2019, each = 12),
           month = rep(1:12, 3)))

col_admin <- 
  panel_df %>% 
  filter(year != 2019 | month <= 10) %>% 
  left_join(crime_summary) %>% 
  mutate(across(aburglary_num:anonviolentcrime_num, ~replace_na(., 0))) %>% 
  left_join(col_units) %>% 
  mutate(
    pt_common = 1 * (year < 2018 | (year == 2018 & month < 7)),
    cum_month = (year - 2017) * 12 + month,
    trt_start = (as.numeric(as.character(substr(first_mtg_block, start = 1, stop = 4))) - 2017) * 12 +
      as.numeric(as.character(substr(first_mtg_block, start = 6, stop = 7))),
    time_period = case_when(
      cum_month >= trt_start + 1 & cum_month < trt_start + 7 ~ "endline",
      cum_month <= trt_start -1 & cum_month > trt_start - 7 ~ "baseline")) %>% 
  filter(!is.na(time_period)) %>% 
  group_by(cuadrante_chr, time_period) %>% 
  summarize(across(aburglary_num:anonviolentcrime_num, sum, na.rm = TRUE)) %>% 
  ungroup %>% 
  pivot_wider(id_cols = cuadrante_chr, names_from = time_period, values_from = aburglary_num:anonviolentcrime_num) %>% 
  rename_with(~gsub("_endline", "", .), everything()) %>% 
  left_join(col_units)

saveRDS(col_admin, file = 'data/out/col-admin-clean.RDS')
