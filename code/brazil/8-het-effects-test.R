
#  All loaded packages should come here
suppressMessages({
  library(tidyverse)
  library(DeclareDesign)
  library(ri2)
})

set.seed(42)

bra_citizen <- readRDS("data/out/bra-citizen-construct.RDS")

source("code/meta-analysis/0-variable-labels.R")

main_outcomes <- main_hypotheses_original %>% slice(1:8) %>% filter(outcome != "officer_attitude_idx") %>% pull(outcome)

bra_citizen <- readRDS("data/out/bra-citizen-construct.RDS")

bra_variance_df <- bra_citizen %>% 
  select(meeting_id, muncode, Z, any_of(main_outcomes)) %>% 
  na.omit

bra_declaration <- 
  with(bra_variance_df, {
    declare_ra(
      blocks = muncode,
      clusters = meeting_id,
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
    declaration = bra_declaration,
    assignment = "Z",
    sharp_hypothesis = 0,
    sims = 1000,
    data = data)
}

bra_var_test_results <- bra_variance_df %>% 
  select(meeting_id, Z, any_of(main_outcomes)) %>% 
  pivot_longer(cols = any_of(main_outcomes)) %>% 
  group_by(name) %>% 
  do(summary(ri_f_var_test(data = .))) %>% 
  ungroup

saveRDS(bra_var_test_results, file = "data/out/bra-var-test.RDS")
