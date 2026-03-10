
#  All loaded packages should come here
suppressMessages({
  library(tidyverse)
  library(DeclareDesign)
  library(ri2)
})

set.seed(42)

lbr_citizen <- readRDS("data/out/lbr-citizen-construct.RDS")

source("code/meta-analysis/0-variable-labels.R")

main_outcomes <- main_hypotheses_original %>% slice(1:8) %>% filter(outcome != "officer_attitude_idx") %>% pull(outcome)

lbr_citizen <- readRDS("data/out/lbr-citizen-construct.RDS")

lbr_variance_df <- lbr_citizen %>% 
  select(communities, police_zones, Z, any_of(main_outcomes)) %>% 
  na.omit

lbr_declaration <- 
  with(lbr_variance_df, {
    declare_ra(
      blocks = police_zones,
      clusters = communities,
      conditions = 0:1
    )
  })

tidy_f_var_test <- function(data, var = "value", treat = "Z") {
  data <- as.data.frame(data)
  val <- try({var.test(data[data[, treat] == 1, var], data[data[, treat] == 0, var], alternative = "two.sided")})
  if(inherits(val, "try-error")){
    NA_real_
  } else {
    val$estimate
  }
}

ri_f_var_test <- function(data) {
  conduct_ri(
    test_function = tidy_f_var_test,
    declaration = lbr_declaration,
    assignment = "Z",
    sharp_hypothesis = 0,
    sims = 1000,
    data = data)
}

lbr_var_test_results <- lbr_variance_df %>% 
  select(communities, police_zones, Z, any_of(main_outcomes)) %>% 
  pivot_longer(cols = any_of(main_outcomes)) %>% 
  group_by(name) %>% 
  do(summary(ri_f_var_test(data = .))) %>% 
  ungroup

saveRDS(lbr_var_test_results, file = "data/out/lbr-var-test.RDS")
