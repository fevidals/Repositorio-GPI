# load all packages here
suppressMessages({
  library(tidyverse)
  library(DeclareDesign)
  library(lmtest)
})

# read data
bra_citizen <- readRDS("data/out/bra-citizen-construct.RDS") %>% 
  filter(municipality != "Florianopolis (x2)") 

# define variations of group formed variables
outcome_vars <- 
  c("groupformed_presence_201806",
    "groupformed_presence_201810",
    "groupformed_area_201806",
    "groupformed_area_201810",
    "know_rdv")

#--------------------------------------------------------------------------------------------------------------
# 1. with FE
#--------------------------------------------------------------------------------------------------------------

# define lists to store outcomes
estimates_list <- list()
# outcome specifications
for (var in outcome_vars) {
  fit_1 <- lm_robust(as.formula(glue::glue("{ var } ~ as.factor(muncode)")), clusters = meeting_id, data = bra_citizen)
  fit_2 <- lm_robust(as.formula(glue::glue("{ var } ~ Z + as.factor(muncode)")), clusters = meeting_id, data = bra_citizen)

  fit_2_df <- tidy(fit_2) %>% filter(term == "Z")
  
  f_test <- waldtest(fit_1, fit_2, test = "F") %>% tidy %>% slice(2) %>% rename_with(~paste0("F_test_", .))
  
  estimates_list[[var]] <- bind_cols(fit_2_df, f_test)
}

# data.frames to be exported
estimates_list_df <- estimates_list %>% bind_rows

# save dataframes
saveRDS(estimates_list_df,  file = "data/out/bra-estimates-firststage.RDS")

