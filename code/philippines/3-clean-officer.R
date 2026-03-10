#--------------------------------------------------------------------------------------------------------------
# all loaded packages come here
#--------------------------------------------------------------------------------------------------------------
suppressMessages({
  library(tidyverse)
  library(haven)
})

#--------------------------------------------------------------------------------------------------------------
# get raw data
#--------------------------------------------------------------------------------------------------------------
phl_officer_endline <- read_dta("data/in/philippines/07_Processed Data/PHI_MK4_OfficerEndline2.dta")
phl_randomization <- read_dta("data/in/philippines/02_Randomization/PHI_MK4_Officer Randomization/POPParticipants.dta")
phl_endline_trackers <- read_dta("data/in/philippines/05_Raw De-Indentified Data/PHI_MK4_OfficerEndlineTrackers.dta")

#--------------------------------------------------------------------------------------------------------------
# clean data
#--------------------------------------------------------------------------------------------------------------
phl_officer_endline <- 
  phl_officer_endline %>% 
  select(-tracker) %>% 
  inner_join(phl_endline_trackers, by = "sid") %>% 
  mutate(endline = 1,
         tracker = if_else(is.na(tracker), 2000 + n(), tracker),
    tracker = case_when(
      tracker == 2144 ~ 273,
      tracker == 2537 ~ 749,
      tracker == 2208 ~ 682,
      tracker == 2737 ~ 637,
      tracker == 2743 ~ 420,
      tracker == 2147 ~ 246,
      tracker == 2462 ~ 564,
      tracker == 2593 ~ 577,
      tracker == 2642 ~ 527,
      tracker == 2655 ~ 594,
      tracker == 2742 ~ 416,
      TRUE ~ tracker)) %>% 
  left_join(phl_randomization, by = "tracker")


# save the data
saveRDS(phl_officer_endline,  file = "data/out/phl-officer-clean.RDS")

