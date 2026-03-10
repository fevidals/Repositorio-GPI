
#  All loaded packages should come here
suppressMessages({
  library(tidyverse)
  library(DeclareDesign)
  library(ri2)
})

set.seed(42)

uga_citizen <- readRDS("data/out/uga-citizen-construct.RDS")

source("code/meta-analysis/0-variable-labels.R")

main_outcomes <- main_hypotheses_original %>% slice(1:8) %>% filter(outcome != "officer_attitude_idx") %>% pull(outcome)

uga_citizen <- readRDS("data/out/uga-citizen-construct.RDS")

uga_variance_df <- uga_citizen %>% 
  select(station_id, block_ID, Z, any_of(main_outcomes)) %>% 
  na.omit

uga_declaration <- 
  with(uga_variance_df, {
    declare_ra(
      blocks = block_ID,
      clusters = station_id,
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
    declaration = uga_declaration,
    assignment = "Z",
    sharp_hypothesis = 0,
    sims = 1000,
    data = data)
}

uga_var_test_results <- uga_variance_df %>% 
  select(station_id, block_ID, Z, any_of(main_outcomes)) %>% 
  pivot_longer(cols = any_of(main_outcomes)) %>% 
  group_by(name) %>% 
  do(summary(ri_f_var_test(data = .))) %>% 
  ungroup

saveRDS(uga_var_test_results, file = "data/out/uga-var-test.RDS")
