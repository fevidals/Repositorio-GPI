#--------------------------------------------------------------------------------------------------------------
# R packages and code
#--------------------------------------------------------------------------------------------------------------
suppressMessages({
  library(tidyverse)
  library(DeclareDesign)
  # library(stdidx)
})
source("code/shared/index-construction.R")

#--------------------------------------------------------------------------------------------------------------
# Read Cleaned Data
#--------------------------------------------------------------------------------------------------------------
lbr_citizen <-readRDS("data/out/lbr-citizen-clean.RDS")
lbr_admin <- readRDS("data/out/lbr-admin-clean.RDS")
lbr_citizen_baseline <-readRDS("data/out/lbr-citizen-baseline-clean.RDS")


#--------------------------------------------------------------------------------------------------------------
# 1. Hypotheses 1(a): crimevictim_idx
#--------------------------------------------------------------------------------------------------------------
# 1. IDEAL ANALYSIS 
lbr_citizen <-
  lbr_citizen %>%
  rowwise() %>%
  mutate(
    # sum all crimes into categories
    violentcrime_num = sum(armedrob_num, simpleassault_num, other_any_violent, na.rm = TRUE),
    nonviolentcrime_num = sum(burglary_num, other_any_nonviolent, na.rm = TRUE),
    cviolentcrime_num = sum(carmedrob_num, caggassault_num, csimpleassault_num, csexual_num, cdomestic_phys_num, cmurder_num, cother_any_violent, na.rm = TRUE),
    cnonviolentcrime_num = sum(cburglary_num, cother_any_nonviolent, na.rm = TRUE),
    violentcrime_num_baseline = sum( armedrob_num_baseline, simpleassault_num_baseline, other_any_violent_baseline, na.rm = TRUE),
    nonviolentcrime_num_baseline = sum(burglary_num_baseline, other_any_nonviolent_baseline, na.rm = TRUE),
    cviolentcrime_num_baseline = sum(carmedrob_num_baseline, caggassault_num_baseline, csimpleassault_num_baseline, sexual_num_baseline, cdomestic_phys_num_baseline, cmurder_num_baseline, cother_any_violent_baseline, na.rm = TRUE),
    cnonviolentcrime_num_baseline = sum(cburglary_num_baseline, cother_any_nonviolent_baseline, na.rm = TRUE),
    cnonviolentcrime_num_baseline = as.numeric(cnonviolentcrime_num_baseline, na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(
    # standardize categories
    violentcrime_num_std = stdize(violentcrime_num, to = violentcrime_num_baseline),
    nonviolentcrime_num_std = stdize(nonviolentcrime_num, to = nonviolentcrime_num_baseline),
    cviolentcrime_num_std = stdize(cviolentcrime_num, to = cviolentcrime_num_baseline),
    cnonviolentcrime_num_std = stdize(cnonviolentcrime_num, to = cnonviolentcrime_num_baseline),
    violentcrime_num_std_baseline = stdize(violentcrime_num_baseline, to = violentcrime_num_baseline),
    nonviolentcrime_num_std_baseline = stdize(nonviolentcrime_num_baseline, to = nonviolentcrime_num_baseline),
    cviolentcrime_num_std_baseline = stdize(cviolentcrime_num_baseline, to = cviolentcrime_num_baseline),
    cnonviolentcrime_num_std_baseline = stdize(cnonviolentcrime_num_baseline, to = cnonviolentcrime_num_baseline),
    # standardize individual crimes
    armedrob_num_std = stdize(armedrob_num, to = armedrob_num_baseline),
    simpleassault_num_std = stdize(simpleassault_num, to = simpleassault_num_baseline),
    other_any_violent_std = stdize(other_any_violent, to = other_any_violent_baseline),
    burglary_num_std = stdize(burglary_num, to = burglary_num_baseline),
    other_any_nonviolent_std = stdize(other_any_nonviolent, to = other_any_nonviolent_baseline),
    carmedrob_num_std = stdize(carmedrob_num, to = carmedrob_num_baseline),
    caggassault_num_std = stdize(caggassault_num, to = caggassault_num_baseline),
    csimpleassault_num_std = stdize(csimpleassault_num, to = csimpleassault_num_baseline),
    csexual_num_std = stdize(csexual_num, to = csexual_num_baseline),
    cdomestic_phys_num_std = stdize(cdomestic_phys_num, to = cdomestic_phys_num_baseline),
    cmurder_num_std = stdize(cmurder_num, to = cmurder_num_baseline),
    cother_any_violent_std = stdize(cother_any_violent, to = cother_any_violent_baseline),
    cburglary_num_std = stdize(cburglary_num, to = cburglary_num_baseline),
    cother_any_nonviolent_std = stdize(cother_any_nonviolent, to = cother_any_nonviolent_baseline),
    
    armedrob_num_std_baseline = stdize(armedrob_num_baseline, to = armedrob_num_baseline),
    simpleassault_num_std_baseline = stdize(simpleassault_num_baseline, to = simpleassault_num_baseline),
    other_any_violent_std_baseline = stdize(other_any_violent_baseline, to = other_any_violent_baseline),
    burglary_num_std_baseline = stdize(burglary_num_baseline, to = burglary_num_baseline),
    other_any_nonviolent_std_baseline = stdize(other_any_nonviolent_baseline, to = other_any_nonviolent_baseline),
    carmedrob_num_std_baseline = stdize(carmedrob_num_baseline, to = carmedrob_num_baseline),
    caggassault_num_std_baseline = stdize(caggassault_num_baseline, to = caggassault_num_baseline),
    csimpleassault_num_std_baseline = stdize(csimpleassault_num_baseline, to = csimpleassault_num_baseline),
    csexual_num_std_baseline = stdize(csexual_num_baseline, to = csexual_num_baseline),
    cdomestic_phys_num_std_baseline = stdize(cdomestic_phys_num_baseline, to = cdomestic_phys_num_baseline),
    cmurder_num_std_baseline = stdize(cmurder_num_baseline, to = cmurder_num_baseline),
    cother_any_violent_std_baseline = stdize(cother_any_violent_baseline, to = cother_any_violent_baseline),
    cburglary_num_std_baseline = stdize(cburglary_num_baseline, to = cburglary_num_baseline),
    cother_any_nonviolent_std_baseline = stdize(cother_any_nonviolent_baseline, to = cother_any_nonviolent_baseline),
    # generate crime index
    crime_victim_idx = idx_mean(violentcrime_num_std, nonviolentcrime_num_std, cviolentcrime_num_std, cnonviolentcrime_num_std, tx = Z, fe = police_zones),
    crime_victim_idx_baseline = idx_mean(violentcrime_num_std_baseline, nonviolentcrime_num_std_baseline, cviolentcrime_num_std_baseline, cnonviolentcrime_num_std_baseline, tx = Z, fe = police_zones),
    # re-standardize
    crime_victim_idx = stdize(crime_victim_idx, to = crime_victim_idx_baseline),
    crime_victim_idx_baseline = stdize(crime_victim_idx_baseline, to = crime_victim_idx_baseline)
  )

# clean for baseline (non collapsed data) Added by Fatiq Nadeem on 05-04-2021 to make country tables
lbr_citizen_baseline <- 
  lbr_citizen_baseline %>% 
  rowwise() %>%
  mutate(
    # sum all crimes into categories
    violentcrime_num_baseline = sum( armedrob_num_baseline, simpleassault_num_baseline, other_any_violent_baseline, na.rm = TRUE),
    nonviolentcrime_num_baseline = sum(burglary_num_baseline, other_any_nonviolent_baseline, na.rm = TRUE),
    cviolentcrime_num_baseline = sum(carmedrob_num_baseline, caggassault_num_baseline, csimpleassault_num_baseline, sexual_num_baseline, cdomestic_phys_num_baseline, cmurder_num_baseline, cother_any_violent_baseline, na.rm = TRUE),
    cnonviolentcrime_num_baseline = sum(cburglary_num_baseline, cother_any_nonviolent_baseline, na.rm = TRUE),
    cnonviolentcrime_num_baseline = as.numeric(cnonviolentcrime_num_baseline, na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(
    # standardize categories
    violentcrime_num_std_baseline = stdize(violentcrime_num_baseline, to = violentcrime_num_baseline),
    nonviolentcrime_num_std_baseline = stdize(nonviolentcrime_num_baseline, to = nonviolentcrime_num_baseline),
    cviolentcrime_num_std_baseline = stdize(cviolentcrime_num_baseline, to = cviolentcrime_num_baseline),
    cnonviolentcrime_num_std_baseline = stdize(cnonviolentcrime_num_baseline, to = cnonviolentcrime_num_baseline),
    # standardize individual crimes
    armedrob_num_std_baseline = stdize(armedrob_num_baseline, to = armedrob_num_baseline),
    simpleassault_num_std_baseline = stdize(simpleassault_num_baseline, to = simpleassault_num_baseline),
    other_any_violent_std_baseline = stdize(other_any_violent_baseline, to = other_any_violent_baseline),
    burglary_num_std_baseline = stdize(burglary_num_baseline, to = burglary_num_baseline),
    other_any_nonviolent_std_baseline = stdize(other_any_nonviolent_baseline, to = other_any_nonviolent_baseline),
    carmedrob_num_std_baseline = stdize(carmedrob_num_baseline, to = carmedrob_num_baseline),
    caggassault_num_std_baseline = stdize(caggassault_num_baseline, to = caggassault_num_baseline),
    csimpleassault_num_std_baseline = stdize(csimpleassault_num_baseline, to = csimpleassault_num_baseline),
    csexual_num_std_baseline = stdize(csexual_num_baseline, to = csexual_num_baseline),
    cdomestic_phys_num_std_baseline = stdize(cdomestic_phys_num_baseline, to = cdomestic_phys_num_baseline),
    cmurder_num_std_baseline = stdize(cmurder_num_baseline, to = cmurder_num_baseline),
    cother_any_violent_std_baseline = stdize(cother_any_violent_baseline, to = cother_any_violent_baseline),
    cburglary_num_std_baseline = stdize(cburglary_num_baseline, to = cburglary_num_baseline),
    cother_any_nonviolent_std_baseline = stdize(cother_any_nonviolent_baseline, to = cother_any_nonviolent_baseline)
    # generate crime index
    # crime_victim_idx_baseline = idx_mean(violentcrime_num_std_baseline, nonviolentcrime_num_std_baseline, cviolentcrime_num_std_baseline, cnonviolentcrime_num_std_baseline, tx = Z, fe = police_zones),
    # # re-standardize
    # crime_victim_idx_baseline = stdize(crime_victim_idx_baseline, to = crime_victim_idx_baseline)
  )


# 2. Common analysis
lbr_citizen <-
  lbr_citizen %>%
  rowwise() %>%
  mutate(
    # sum all crimes into categories
    cviolentcrime_num_common = sum(carmedrob_num, csimpleassault_num, csexual_num, cdomestic_phys_num, cother_any_violent, na.rm = TRUE),
    cviolentcrime_num_common_baseline = sum(carmedrob_num_baseline, csimpleassault_num_baseline, csexual_num_baseline, cdomestic_phys_num_baseline, cother_any_violent_baseline, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    # standardize categories
    cviolentcrime_num_common_std = stdize(cviolentcrime_num_common, to = cviolentcrime_num_common_baseline),
    cviolentcrime_num_common_std_baseline = stdize(cviolentcrime_num_common_baseline, to = cviolentcrime_num_common_baseline),
    # generate crime index
    crime_victim_idx_common = idx_mean(violentcrime_num_std, nonviolentcrime_num_std, cviolentcrime_num_common_std, cnonviolentcrime_num_std, tx = Z, fe = police_zones),
    crime_victim_idx_common_baseline = idx_mean(violentcrime_num_std_baseline, nonviolentcrime_num_std_baseline, cviolentcrime_num_common_std_baseline, cnonviolentcrime_num_std_baseline, tx = Z, fe = police_zones),
    # re-standardize
    crime_victim_idx_common = stdize(crime_victim_idx_common, to = crime_victim_idx_common_baseline),
    crime_victim_idx_common_baseline = stdize(crime_victim_idx_common_baseline, to = crime_victim_idx_common_baseline)
  )

# 3. List-wise deletion
lbr_citizen <-
  lbr_citizen %>%
  mutate(
    crime_victim_idx_listwise = idx_mean_listwise(violentcrime_num_std, nonviolentcrime_num_std, cviolentcrime_num_common_std, cnonviolentcrime_num_std),
    crime_victim_idx_listwise_baseline = idx_mean_listwise(violentcrime_num_std_baseline, nonviolentcrime_num_std_baseline, cviolentcrime_num_common_std_baseline, cnonviolentcrime_num_std_baseline),
    # re-standardize
    crime_victim_idx_listwise = stdize(crime_victim_idx_listwise, to = crime_victim_idx_listwise_baseline),
    crime_victim_idx_listwise_baseline = stdize(crime_victim_idx_listwise_baseline, to = crime_victim_idx_listwise_baseline)
  )

# 4. Outlier deletion
lbr_citizen <-
  lbr_citizen %>%
  mutate(
    armedrob_num_outlier = na_if(armedrob_num, quantile(armedrob_num, 0.95, 0.95, na.rm = TRUE)),
    simpleassault_num_outlier = na_if(simpleassault_num, quantile(simpleassault_num, 0.95, na.rm = TRUE)),
    other_any_violent_outlier = na_if(other_any_violent, quantile(other_any_violent, 0.95, na.rm = TRUE)),
    burglary_num_outlier = na_if(burglary_num, quantile(burglary_num, 0.95, na.rm = TRUE)),
    other_any_nonviolent_outlier = na_if(other_any_nonviolent, quantile(other_any_nonviolent, 0.95, na.rm = TRUE)),
    carmedrob_num_outlier = na_if(carmedrob_num, quantile(carmedrob_num, 0.95, na.rm = TRUE)),
    caggassault_num_outlier = na_if(caggassault_num, quantile(caggassault_num, 0.95, na.rm = TRUE)),
    csimpleassault_num_outlier = na_if(csimpleassault_num, quantile(csimpleassault_num, 0.95, na.rm = TRUE)),
    csexual_num_outlier = na_if(csexual_num, quantile(csexual_num, 0.95, na.rm = TRUE)),
    cdomestic_phys_num_outlier = na_if(cdomestic_phys_num, quantile(cdomestic_phys_num, 0.95, na.rm = TRUE)),
    cmurder_num_outlier = na_if(cmurder_num, quantile(cmurder_num, 0.95, na.rm = TRUE)),
    cother_any_violent_outlier = na_if(cother_any_violent, quantile(cother_any_violent, 0.95, na.rm = TRUE)),
    cburglary_num_outlier = na_if(cburglary_num, quantile(cburglary_num, 0.95, na.rm = TRUE)),
    cother_any_nonviolent_outlier = na_if(cother_any_nonviolent, quantile(cother_any_nonviolent, 0.95, na.rm = TRUE)),
    
    armedrob_num_outlier_baseline = na_if(armedrob_num_baseline, quantile(armedrob_num_baseline, 0.95, na.rm = TRUE)),
    simpleassault_num_outlier_baseline = na_if(simpleassault_num_baseline, quantile(simpleassault_num_baseline, 0.95, na.rm = TRUE)),
    other_any_violent_outlier_baseline = na_if(other_any_violent_baseline, quantile(other_any_violent_baseline, 0.95, na.rm = TRUE)),
    burglary_num_outlier_baseline = na_if(burglary_num_baseline, quantile(burglary_num_baseline, 0.95, na.rm = TRUE)),
    other_any_nonviolent_outlier_baseline = na_if(other_any_nonviolent_baseline, quantile(other_any_nonviolent_baseline, 0.95, na.rm = TRUE)),
    carmedrob_num_outlier_baseline = na_if(carmedrob_num_baseline, quantile(carmedrob_num_baseline, 0.95, na.rm = TRUE)),
    caggassault_num_outlier_baseline = na_if(caggassault_num_baseline, quantile(caggassault_num_baseline, 0.95, na.rm = TRUE)),
    csimpleassault_num_outlier_baseline = na_if(csimpleassault_num_baseline, quantile(csimpleassault_num_baseline, 0.95, na.rm = TRUE)),
    csexual_num_outlier_baseline = na_if(csexual_num_baseline, quantile(csexual_num_baseline, 0.95, na.rm = TRUE)),
    cdomestic_phys_num_outlier_baseline = na_if(cdomestic_phys_num_baseline, quantile(cdomestic_phys_num_baseline, 0.95, na.rm = TRUE)),
    cmurder_num_outlier_baseline = na_if(cmurder_num_baseline, quantile(cmurder_num_baseline, 0.95, na.rm = TRUE)),
    cother_any_violent_outlier_baseline = na_if(cother_any_violent_baseline, quantile(cother_any_violent_baseline, 0.95, na.rm = TRUE)),
    cburglary_num_outlier_baseline = na_if(cburglary_num_baseline, quantile(cburglary_num_baseline, 0.95, na.rm = TRUE)),
    cother_any_nonviolent_outlier_baseline = na_if(cother_any_nonviolent_baseline, quantile(cother_any_nonviolent_baseline, 0.95, na.rm = TRUE))) %>% 
  rowwise() %>%
  mutate(
    # sum all crimes into categories
    violentcrime_num_outlier = sum(armedrob_num_outlier, simpleassault_num_outlier, other_any_violent_outlier, na.rm = TRUE),
    nonviolentcrime_num_outlier = sum(burglary_num_outlier, other_any_nonviolent_outlier, na.rm = TRUE),
    cviolentcrime_num_outlier = sum(carmedrob_num_outlier, caggassault_num_outlier, csimpleassault_num_outlier, csexual_num_outlier, cdomestic_phys_num_outlier, cmurder_num_outlier, cother_any_violent_outlier, na.rm = TRUE),
    cnonviolentcrime_num_outlier = sum(cburglary_num_outlier, cother_any_nonviolent_outlier, na.rm = TRUE),
    violentcrime_num_outlier_baseline = sum(armedrob_num_outlier_baseline, simpleassault_num_outlier_baseline, other_any_violent_outlier_baseline, na.rm = TRUE),
    nonviolentcrime_num_outlier_baseline = sum(burglary_num_outlier_baseline, other_any_nonviolent_outlier_baseline, na.rm = TRUE),
    cviolentcrime_num_outlier_baseline = sum(carmedrob_num_outlier_baseline, caggassault_num_outlier_baseline, csimpleassault_num_outlier_baseline, csexual_num_outlier_baseline, cdomestic_phys_num_outlier_baseline, cmurder_num_outlier_baseline, cother_any_violent_outlier_baseline, na.rm = TRUE),
    cnonviolentcrime_num_outlier_baseline = sum(cburglary_num_outlier_baseline, cother_any_nonviolent_outlier_baseline, na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(
    # standardize categories
    violentcrime_num_outlier_std = stdize(violentcrime_num_outlier, to = violentcrime_num_outlier_baseline),
    nonviolentcrime_num_outlier_std = stdize(nonviolentcrime_num_outlier, to = nonviolentcrime_num_outlier_baseline),
    cviolentcrime_num_outlier_std = stdize(cviolentcrime_num_outlier, to = cviolentcrime_num_outlier_baseline),
    cnonviolentcrime_num_outlier_std = stdize(cnonviolentcrime_num_outlier, to = cnonviolentcrime_num_outlier_baseline),
    violentcrime_num_outlier_std_baseline = stdize(violentcrime_num_outlier_baseline, to = violentcrime_num_outlier_baseline),
    nonviolentcrime_num_outlier_std_baseline = stdize(nonviolentcrime_num_outlier_baseline, to = nonviolentcrime_num_outlier_baseline),
    cviolentcrime_num_outlier_std_baseline = stdize(cviolentcrime_num_outlier_baseline, to = cviolentcrime_num_outlier_baseline),
    cnonviolentcrime_num_outlier_std_baseline = stdize(cnonviolentcrime_num_outlier_baseline, to = cnonviolentcrime_num_outlier_baseline),
    # standardize individual crimes
    armedrob_num_outlier_std = stdize(armedrob_num, to = armedrob_num_baseline),
    simpleassault_num_outlier_std = stdize(simpleassault_num, to = simpleassault_num_baseline),
    other_any_violent_outlier_std = stdize(other_any_violent, to = other_any_violent_baseline),
    burglary_num_outlier_std = stdize(burglary_num, to = burglary_num_baseline),
    other_any_nonviolent_outlier_std = stdize(other_any_nonviolent, to = other_any_nonviolent_baseline),
    carmedrob_num_outlier_std = stdize(carmedrob_num, to = carmedrob_num_baseline),
    caggassault_num_outlier_std = stdize(caggassault_num, to = caggassault_num_baseline),
    csimpleassault_num_outlier_std = stdize(csimpleassault_num, to = csimpleassault_num_baseline),
    csexual_num_outlier_std = stdize(csexual_num, to = csexual_num_baseline),
    cdomestic_phys_num_outlier_std = stdize(cdomestic_phys_num, to = cdomestic_phys_num_baseline),
    cmurder_num_outlier_std = stdize(cmurder_num, to = cmurder_num_baseline),
    cother_any_violent_outlier_std = stdize(cother_any_violent, to = cother_any_violent_baseline),
    cburglary_num_outlier_std = stdize(cburglary_num, to = cburglary_num_baseline),
    cother_any_nonviolent_outlier_std = stdize(cother_any_nonviolent, to = cother_any_nonviolent_baseline),
    
    armedrob_num_std_outlier_baseline = stdize(armedrob_num_baseline, to = armedrob_num_baseline),
    simpleassault_num_std_outlier_baseline = stdize(simpleassault_num_baseline, to = simpleassault_num_baseline),
    other_any_violent_std_outlier_baseline = stdize(other_any_violent_baseline, to = other_any_violent_baseline),
    burglary_num_std_outlier_baseline = stdize(burglary_num_baseline, to = burglary_num_baseline),
    other_any_nonviolent_std_outlier_baseline = stdize(other_any_nonviolent_baseline, to = other_any_nonviolent_baseline),
    carmedrob_num_std_outlier_baseline = stdize(carmedrob_num_baseline, to = carmedrob_num_baseline),
    caggassault_num_std_outlier_baseline = stdize(caggassault_num_baseline, to = caggassault_num_baseline),
    csimpleassault_num_std_outlier_baseline = stdize(csimpleassault_num_baseline, to = csimpleassault_num_baseline),
    csexual_num_std_outlier_baseline = stdize(csexual_num_baseline, to = csexual_num_baseline),
    cdomestic_phys_num_std_outlier_baseline = stdize(cdomestic_phys_num_baseline, to = cdomestic_phys_num_baseline),
    cmurder_num_std_outlier_baseline = stdize(cmurder_num_baseline, to = cmurder_num_baseline),
    cother_any_violent_std_outlier_baseline = stdize(cother_any_violent_baseline, to = cother_any_violent_baseline),
    cburglary_num_std_outlier_baseline = stdize(cburglary_num_baseline, to = cburglary_num_baseline),
    cother_any_nonviolent_std_outlier_baseline = stdize(cother_any_nonviolent_baseline, to = cother_any_nonviolent_baseline),
    # generate crime index
    crime_victim_idx_outlier = idx_mean(violentcrime_num_outlier_std, nonviolentcrime_num_outlier_std, cviolentcrime_num_outlier_std, cnonviolentcrime_num_outlier_std, tx = Z, fe = police_zones),
    crime_victim_idx_outlier_baseline = idx_mean(violentcrime_num_outlier_std_baseline, nonviolentcrime_num_outlier_std_baseline, cviolentcrime_num_outlier_std_baseline, cnonviolentcrime_num_outlier_std_baseline, tx = Z, fe = police_zones),
    # re-standardize
    crime_victim_idx_outlier = stdize(crime_victim_idx_outlier, to = crime_victim_idx_outlier_baseline),
    crime_victim_idx_outlier_baseline = stdize(crime_victim_idx_outlier_baseline, to = crime_victim_idx_outlier_baseline)
  )

#--------------------------------------------------------------------------------------------------------------
# 2. Hypotheses 1(b): future_insecurity_idx
#--------------------------------------------------------------------------------------------------------------
# 1. IDEAL ANAYSIS
lbr_citizen <- 
  lbr_citizen %>%
  mutate(
    # standardize vars
    fear_violent_std = stdize(fear_violent, to = fear_violent_baseline),
    fear_nonviolent_std = stdize(fear_nonviolent, to = fear_nonviolent_baseline),
    feared_walk_std = stdize(feared_walk, to = feared_walk_baseline),
    fear_violent_std_baseline = stdize(fear_violent_baseline, to = fear_violent_baseline),
    fear_nonviolent_std_baseline = stdize(fear_nonviolent_baseline, to = fear_nonviolent_baseline),
    feared_walk_std_baseline = stdize(feared_walk_baseline, to = feared_walk_baseline),
    # calculate index
    future_insecurity_idx = idx_mean(fear_violent_std, fear_nonviolent_std, feared_walk_std, tx = Z, fe = police_zones),
    future_insecurity_idx_baseline = idx_mean(fear_violent_std_baseline, fear_nonviolent_std_baseline, feared_walk_std_baseline, tx = Z, fe = police_zones),
    # restandardize
    future_insecurity_idx = stdize(future_insecurity_idx, to = future_insecurity_idx_baseline),
    future_insecurity_idx_baseline = stdize(future_insecurity_idx_baseline, to = future_insecurity_idx_baseline)
  )

lbr_citizen_baseline <- 
  lbr_citizen_baseline %>%
  mutate(
    # standardize vars
    fear_violent_std_baseline = stdize(fear_violent_baseline, to = fear_violent_baseline),
    fear_nonviolent_std_baseline = stdize(fear_nonviolent_baseline, to = fear_nonviolent_baseline),
    feared_walk_std_baseline = stdize(feared_walk_baseline, to = feared_walk_baseline),
  )

# 2. COMMON ANALYSIS
lbr_citizen <- 
  lbr_citizen %>%
  mutate(
    # Note: we have dropped fear_nonviolent following committee decision
    # standardize vars
    fear_violent_std_common = stdize(fear_violent, to = fear_violent_baseline),
    feared_walk_std_common = stdize(feared_walk, to = feared_walk_baseline),
    fear_violent_std_common_baseline = stdize(fear_violent_baseline, to = fear_violent_baseline),
    feared_walk_std_common_baseline = stdize(feared_walk_baseline, to = feared_walk_baseline),
    # calculate index
    future_insecurity_idx_common = idx_mean(fear_violent_std_common, feared_walk_std_common, tx = Z, fe = police_zones),
    future_insecurity_idx_common_baseline = idx_mean(fear_violent_std_common_baseline, feared_walk_std_common_baseline, tx = Z, fe = police_zones),
    # restandardize
    future_insecurity_idx_common = stdize(future_insecurity_idx_common, to = future_insecurity_idx_common_baseline),
    future_insecurity_idx_common_baseline = stdize(future_insecurity_idx_common_baseline, to = future_insecurity_idx_common_baseline)
  )

# 3. List-wise deletion
lbr_citizen <-
  lbr_citizen %>%
  mutate(
    future_insecurity_idx_listwise = idx_mean_listwise(fear_violent_std_common, feared_walk_std_common),
    future_insecurity_idx_listwise_baseline = idx_mean_listwise(fear_violent_std_common_baseline, feared_walk_std_common_baseline),
    # restandardize
    future_insecurity_idx_listwise = stdize(future_insecurity_idx_listwise, to = future_insecurity_idx_listwise_baseline),
    future_insecurity_idx_listwise_baseline = stdize(future_insecurity_idx_listwise_baseline, to = future_insecurity_idx_listwise_baseline))

# 4. Outlier analysis
lbr_citizen <- 
  lbr_citizen %>% 
  mutate(
    fear_violent_outlier = na_if(fear_violent, quantile(fear_violent, 0.95, na.rm = TRUE)),
    fear_nonviolent_outlier = na_if(fear_nonviolent, quantile(fear_nonviolent, 0.95, na.rm = TRUE)),
    feared_walk_outlier = na_if(feared_walk, quantile(feared_walk, 0.95, na.rm = TRUE)),
    fear_violent_outlier_baseline = na_if(fear_violent_baseline, quantile(fear_violent_baseline, 0.95, na.rm = TRUE)),
    fear_nonviolent_outlier_baseline = na_if(fear_nonviolent_baseline, quantile(fear_nonviolent_baseline, 0.95, na.rm = TRUE)),
    feared_walk_outlier_baseline = na_if(feared_walk_baseline, quantile(feared_walk_baseline, 0.95, na.rm = TRUE)),
    # standardize vars
    fear_violent_outlier_std = stdize(fear_violent_outlier, to = fear_violent_outlier_baseline),
    fear_nonviolent_outlier_std = stdize(fear_nonviolent_outlier, to = fear_nonviolent_outlier_baseline),
    feared_walk_outlier_std = stdize(feared_walk_outlier, to = feared_walk_outlier_baseline),
    fear_violent_outlier_std_baseline = stdize(fear_violent_outlier_baseline, to = fear_violent_outlier_baseline),
    fear_nonviolent_outlier_std_baseline = stdize(fear_nonviolent_outlier_baseline, to = fear_nonviolent_outlier_baseline),
    feared_walk_outlier_std_baseline = stdize(feared_walk_outlier_baseline, to = feared_walk_outlier_baseline),
    # calculate index
    future_insecurity_idx_outlier = idx_mean(fear_violent_outlier_std, fear_nonviolent_outlier_std, feared_walk_outlier_std, tx = Z, fe = police_zones),
    future_insecurity_idx_outlier_baseline = idx_mean(fear_violent_outlier_std_baseline, fear_nonviolent_outlier_std_baseline, feared_walk_outlier_std_baseline, tx = Z, fe = police_zones),
    # re-standardize
    future_insecurity_idx_outlier = stdize(future_insecurity_idx_outlier, to = future_insecurity_idx_outlier_baseline),
    future_insecurity_idx_outlier_baseline = stdize(future_insecurity_idx_outlier_baseline, to = future_insecurity_idx_outlier_baseline)
  )

#--------------------------------------------------------------------------------------------------------------
# 3. Hypotheses 2: satis_idx
#--------------------------------------------------------------------------------------------------------------
lbr_citizen <- 
  lbr_citizen %>%
  mutate(
    # standardize vars
    satis_trust_std = stdize(satis_trust, to = satis_trust_baseline),
    satis_general_std = stdize(satis_general, to = satis_general_baseline),
    satis_trust_std_baseline = stdize(satis_trust_baseline, to = satis_trust_baseline),
    satis_general_std_baseline = stdize(satis_general_baseline, to = satis_general_baseline),
    # calculate index
    satis_idx = idx_mean(satis_trust_std, satis_general_std, tx = Z, fe = police_zones),
    satis_idx_baseline = idx_mean(satis_trust_std_baseline, satis_general_std_baseline, tx = Z, fe = police_zones),
    # restandardize
    satis_idx = stdize(satis_idx, to = satis_idx_baseline),
    satis_idx_baseline = stdize(satis_idx_baseline, to = satis_idx_baseline)
  )

# clean for baseline (non collapsed data) Added by Fatiq Nadeem on 05-04-2021 to make country tables
lbr_citizen_baseline <- 
  lbr_citizen_baseline %>% 
  mutate(
    # standardize vars
    satis_trust_std_baseline = stdize(satis_trust_baseline, to = satis_trust_baseline),
    satis_general_std_baseline = stdize(satis_general_baseline, to = satis_general_baseline),
  )


# 3. List-wise deletion
lbr_citizen <-
  lbr_citizen %>%
  mutate(
    satis_idx_listwise = idx_mean_listwise(satis_trust_std, satis_general_std),
    satis_idx_listwise_baseline = idx_mean_listwise(satis_trust_std_baseline, satis_general_std_baseline),
    # restandardize
    satis_idx_listwise = stdize(satis_idx_listwise, to = satis_idx_listwise_baseline),
    satis_idx_listwise_baseline = stdize(satis_idx_listwise_baseline, to = satis_idx_listwise_baseline))

# 4. Outlier analysis
lbr_citizen <- 
  lbr_citizen %>% 
  mutate(
    satis_trust_outlier = na_if(satis_trust, quantile(satis_trust, 0.95, na.rm = TRUE)),
    satis_general_outlier = na_if(satis_general, quantile(satis_general, 0.95, na.rm = TRUE)),
    satis_trust_outlier_baseline = na_if(satis_trust_baseline, quantile(satis_trust_baseline, 0.95, na.rm = TRUE)),
    satis_general_outlier_baseline = na_if(satis_general_baseline, quantile(satis_general_baseline, 0.95, na.rm = TRUE)),
    # standardize vars
    satis_trust_outlier_std = stdize(satis_trust_outlier, to = satis_trust_outlier_baseline),
    satis_general_outlier_std = stdize(satis_general_outlier, to = satis_general_outlier_baseline),
    satis_trust_outlier_std_baseline = stdize(satis_trust_outlier_baseline, to = satis_trust_outlier_baseline),
    satis_general_outlier_std_baseline = stdize(satis_general_outlier_baseline, to = satis_general_outlier_baseline),
    # calculate index
    satis_idx_outlier = idx_mean(satis_trust_outlier_std, satis_general_outlier_std, tx = Z, fe = police_zones),
    satis_idx_outlier_baseline = idx_mean(satis_trust_outlier_std_baseline, satis_general_outlier_std_baseline, tx = Z, fe = police_zones),
    # re-standardize
    satis_idx_outlier = stdize(satis_idx_outlier, to = satis_idx_outlier_baseline),
    satis_idx_outlier_baseline = stdize(satis_idx_outlier_baseline, to = satis_idx_outlier_baseline)
  )

#--------------------------------------------------------------------------------------------------------------
# Hypotheses 3(a): officer_attitude_idx
#--------------------------------------------------------------------------------------------------------------
# Note: this is not calculated for Liberia because we specify in the mpap not to conduct any non-experimental comparisons

# --------------------------------------------------------------------------------------------------------------
# 4. Hypotheses 3(b): police_abuse_idx
# --------------------------------------------------------------------------------------------------------------
lbr_citizen <-
  lbr_citizen %>%
  mutate(
    # general police abuse
    policeabuse_any = if_else(policeabuse_verbal_any > 0 | policeabuse_phys_any > 0, 1, 0),
    policeabuse_any_baseline = if_else(policeabuse_verbal_any_baseline > 0 | policeabuse_phys_any_baseline > 0, 1, 0)) %>% 
  rowwise() %>% 
  mutate(
    policeabuse_num = sum(policeabuse_verbal_num, policeabuse_phys_num, na.rm = TRUE),
    policeabuse_num_baseline = sum(policeabuse_verbal_num_baseline, policeabuse_phys_num_baseline, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(
    # bribe amount is divided by the rate of dollar (GH issue number 49)
    bribe_amt = bribe_amt / 3714,
    bribe_amt_baseline = bribe_amt_baseline / 3714,
    # different construction that the M-PAP (GH issues number 31)
    bribe_amt = if_else(bribe_freq == 1, 0, bribe_amt),
    bribe_amt_baseline = if_else(bribe_freq_baseline == 1, 0, bribe_amt_baseline),
    # standardize vars
    policeabuse_any_std = stdize(policeabuse_any, to = policeabuse_any_baseline),
    policeabuse_num_std = stdize(policeabuse_num, to = policeabuse_num_baseline),
    bribe_freq_std = stdize(bribe_freq, to = bribe_freq_baseline),
    bribe_amt_std = stdize(bribe_amt, to = bribe_amt_baseline),
    policeabuse_any_std_baseline = stdize(policeabuse_any_baseline, to = policeabuse_any_baseline),
    policeabuse_num_std_baseline = stdize(policeabuse_num_baseline, to = policeabuse_num_baseline),
    bribe_freq_std_baseline = stdize(bribe_freq_baseline, to = bribe_freq_baseline),
    bribe_amt_std_baseline = stdize(bribe_amt_baseline, to = bribe_amt_baseline),
    # calculate index
    police_abuse_idx = idx_mean(policeabuse_any_std, policeabuse_num_std, bribe_freq_std, bribe_amt_std, tx = Z, fe = police_zones),
    police_abuse_idx_baseline = idx_mean(policeabuse_any_std_baseline, policeabuse_num_std_baseline, bribe_freq_std_baseline, bribe_amt_std_baseline, tx = Z, fe = police_zones),
    # re-standardize
    police_abuse_idx = stdize(police_abuse_idx, to = police_abuse_idx_baseline),
    police_abuse_idx_baseline = stdize(police_abuse_idx_baseline, to = police_abuse_idx_baseline)
  )

# clean for baseline (non collapsed data) Added by Fatiq Nadeem on 05-04-2021 to make country tables
lbr_citizen_baseline <- 
  lbr_citizen_baseline %>% 
  mutate(
    # general police abuse
    policeabuse_any_baseline = if_else(policeabuse_verbal_any_baseline > 0 | policeabuse_phys_any_baseline > 0, 1, 0)) %>% 
  rowwise() %>% 
  mutate(
    policeabuse_num_baseline = sum(policeabuse_verbal_num_baseline, policeabuse_phys_num_baseline, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(
    # bribe amount is divided by the rate of dollar (GH issue number 49)
    bribe_amt_baseline = bribe_amt_baseline / 3714,
    # different construction that the M-PAP (GH issues number 31)
    bribe_amt_baseline = if_else(bribe_freq_baseline == 1, 0, bribe_amt_baseline),
    # standardize vars
    policeabuse_any_std_baseline = stdize(policeabuse_any_baseline, to = policeabuse_any_baseline),
    policeabuse_num_std_baseline = stdize(policeabuse_num_baseline, to = policeabuse_num_baseline),
    bribe_freq_std_baseline = stdize(bribe_freq_baseline, to = bribe_freq_baseline),
    bribe_amt_std_baseline = stdize(bribe_amt_baseline, to = bribe_amt_baseline)
  )



# 2. Common analysis
lbr_citizen <-
  lbr_citizen %>%
  mutate(
    police_abuse_idx_common = idx_mean(policeabuse_any_std, bribe_freq_std, bribe_amt_std, tx = Z, fe = police_zones),
    police_abuse_idx_common_baseline = idx_mean(policeabuse_any_std_baseline, bribe_freq_std_baseline, bribe_amt_std_baseline, tx = Z, fe = police_zones),
    # re-standardize
    police_abuse_idx_common = stdize(police_abuse_idx_common, to = police_abuse_idx_common_baseline),
    police_abuse_idx_common_baseline = stdize(police_abuse_idx_common_baseline, to = police_abuse_idx_common_baseline)
  )

# 3. List-wise deletion
lbr_citizen <-
  lbr_citizen %>%
  mutate(
    police_abuse_idx_listwise = idx_mean_listwise(policeabuse_any_std, bribe_freq_std, bribe_amt_std),
    police_abuse_idx_listwise_baseline = idx_mean_listwise(policeabuse_any_std_baseline, bribe_freq_std_baseline, bribe_amt_std_baseline),
    # re-standardize
    police_abuse_idx_listwise = stdize(police_abuse_idx_listwise, to = police_abuse_idx_listwise_baseline),
    police_abuse_idx_listwise_baseline = stdize(police_abuse_idx_listwise_baseline, to = police_abuse_idx_listwise_baseline))

# 4. Outlier deletion
lbr_citizen <- 
  lbr_citizen %>% 
  mutate(
    policeabuse_any_outlier = na_if(policeabuse_any, quantile(policeabuse_any, 0.95, na.rm = TRUE)),
    policeabuse_num_outlier = na_if(policeabuse_num, quantile(policeabuse_num, 0.95, na.rm = TRUE)),
    bribe_freq_outlier = na_if(bribe_freq, quantile(bribe_freq, 0.95, na.rm = TRUE)),
    bribe_amt_outlier = na_if(bribe_amt, quantile(bribe_amt, 0.95, na.rm = TRUE)),
    policeabuse_any_outlier_baseline = na_if(policeabuse_any_baseline, quantile(policeabuse_any_baseline, 0.95, na.rm = TRUE)),
    policeabuse_num_outlier_baseline = na_if(policeabuse_num_baseline, quantile(policeabuse_num_baseline, 0.95, na.rm = TRUE)),
    bribe_freq_outlier_baseline = na_if(bribe_freq_baseline, quantile(bribe_freq_baseline, 0.95, na.rm = TRUE)),
    bribe_amt_outlier_baseline = na_if(bribe_amt_baseline, quantile(bribe_amt_baseline, 0.95, na.rm = TRUE)),
    
    policeabuse_any_outlier_std = stdize(policeabuse_any_outlier, to = policeabuse_any_outlier_baseline),
    policeabuse_num_outlier_std = stdize(policeabuse_num_outlier, to = policeabuse_num_outlier_baseline),
    bribe_freq_outlier_std = stdize(bribe_freq_outlier, to = bribe_freq_outlier_baseline),
    bribe_amt_outlier_std = stdize(bribe_amt_outlier, to = bribe_amt_outlier_baseline),
    policeabuse_any_outlier_std_baseline = stdize(policeabuse_any_outlier_baseline, to = policeabuse_any_outlier_baseline),
    policeabuse_num_outlier_std_baseline = stdize(policeabuse_num_outlier_baseline, to = policeabuse_num_outlier_baseline),
    bribe_freq_outlier_std_baseline = stdize(bribe_freq_outlier_baseline, to = bribe_freq_outlier_baseline),
    bribe_amt_outlier_std_baseline = stdize(bribe_amt_outlier_baseline, to = bribe_amt_outlier_baseline),
    
    police_abuse_idx_outlier = idx_mean(policeabuse_any_std, policeabuse_num_std, bribe_freq_std, bribe_amt_std, tx = Z, fe = police_zones),
    police_abuse_idx_outlier_baseline = idx_mean(policeabuse_any_std_baseline, bribe_freq_std_baseline, bribe_amt_std_baseline, tx = Z, fe = police_zones),
    # re-standardize
    police_abuse_idx_outlier = stdize(police_abuse_idx_outlier, to = police_abuse_idx_outlier_baseline),
    police_abuse_idx_outlier_baseline = stdize(police_abuse_idx_outlier_baseline, to = police_abuse_idx_outlier_baseline)
  )

#--------------------------------------------------------------------------------------------------------------
#                     # 5. Hypotheses 4(a): crime_reporting_idx
#--------------------------------------------------------------------------------------------------------------
lbr_citizen <- 
  lbr_citizen %>%
  mutate(
    # generate crime reporting categories
    armedrob_report = case_when(
      armedrob_num == 0 | armedrob_report == 0 ~ 0, 
      armedrob_num > 0 & armedrob_report == 1 ~ 1),
    burglary_report = case_when(
      burglary_num == 0 | burglary_report == 0 ~ 0,
      burglary_num > 0 & burglary_report == 1 ~ 1),
    simpleassault_report = case_when(
      simpleassault_num == 0 | simpleassault_report == 0 ~ 0,
      simpleassault_num > 0 & simpleassault_report == 1 ~ 1),
    carmedrob_report = case_when(
      carmedrob_num == 0 | carmedrob_report == 0 ~ 0,
      carmedrob_num > 0 & carmedrob_report == 1 ~ 1),
    cburglary_report = case_when(
      cburglary_num == 0 | cburglary_report == 0 ~ 0,
      cburglary_num > 0 & cburglary_report == 1 ~ 1),
    caggassault_report = case_when(
      caggassault_num == 0 | caggassault_report == 0 ~ 0,
      caggassault_num > 0 & caggassault_report == 1 ~ 1),
    csimpleassault_report = case_when(
      csimpleassault_num == 0 | csimpleassault_report == 0 ~ 0,
      csimpleassault_num > 0 & csimpleassault_report == 1 ~ 1),
    csexual_report = case_when(
      csexual_num == 0 | csexual_report == 0 ~ 0,
      csexual_num > 0 & csexual_report == 1 ~ 1),
    cdomestic_phys_report = case_when(
      cdomestic_phys_num == 0 | cdomestic_phys_report == 0 ~ 0,
      cdomestic_phys_num > 0 & cdomestic_phys_report == 1 ~ 1),
    # Note: cmurder_num not collected for lbr endline as per their pap
    # Note: cmurder_num not collected for lbr baseline as per their pap
    # cmurder_report_baseline = 
    # case_when(
    #  cmurder_num_baseline == 0 | cmurder_report_baseline == 0 ~ 0,
    #  cmurder_num_baseline > 0 & cmurder_report_baseline == 1 ~ 1)
    
    burglaryres = case_when(
      burglaryres == 1 ~ 1L,
      burglaryres == 2 ~ 1L,
      !is.na(burglaryres) ~ 0L),
    dviolres = case_when(
      dviolres == 1 ~ 1L,
      dviolres == 2 ~ 1L,
      !is.na(dviolres) ~ 0L),
    armedrobres = case_when(
      armedrobres == 1 ~ 1L,
      armedrobres == 2 ~ 1L,
      !is.na(armedrobres) ~ 0L)) %>% 
  rowwise() %>% 
  mutate(
    # calculate crime categories
    violentcrime_report_num = sum(armedrob_report, simpleassault_report, other_report_violent, na.rm = TRUE),
    nonviolentcrime_report_num = sum(burglary_report, 
                                     # other_report_nonviolent,
                                     na.rm = TRUE),
    cviolentcrime_report_num = sum(carmedrob_report, caggassault_report, csimpleassault_report, csexual_report, cdomestic_phys_report, cmurder_report, cother_report_violent, na.rm = TRUE),
    cnonviolentcrime_report_num = sum(cburglary_report, 
                                      # cother_report_nonviolent, 
                                      na.rm = TRUE),
    
    violentcrime_report_num_baseline = sum(armedrob_report_baseline, simpleassault_report_baseline, other_report_violent_baseline, na.rm = TRUE),
    nonviolentcrime_report_num_baseline = sum(burglary_report_baseline, other_report_nonviolent_baseline, na.rm = TRUE),
    cviolentcrime_report_num_baseline = sum(carmedrob_report_baseline, caggassault_report_baseline, csimpleassault_report_baseline, csexual_report_baseline, cdomestic_phys_report_baseline, 
                                            # cmurder_report_baseline, # cmurder_baseline was not collected as per mpap
                                            cother_report_violent_baseline, na.rm = TRUE),
    cnonviolentcrime_report_num_baseline = sum(cburglary_report_baseline, 
                                               # cother_report_nonviolent_baseline, 
                                               na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(
    # standarduze all vars
    burglaryres_std = stdize(burglaryres, to = burglaryres_baseline),
    dviolres_std = stdize(dviolres, to = dviolres_baseline),
    armedrobres_std = stdize(armedrobres, to = armedrobres_baseline),
    violentcrime_report_num_std = stdize(violentcrime_report_num, to = violentcrime_report_num_baseline),
    nonviolentcrime_report_num_std = stdize(nonviolentcrime_report_num, to = nonviolentcrime_report_num_baseline),
    cviolentcrime_report_num_std = stdize(cviolentcrime_report_num, to = cviolentcrime_report_num_baseline),
    
    burglaryres_std_baseline = stdize(burglaryres_baseline, to = burglaryres_baseline),
    dviolres_std_baseline = stdize(dviolres_baseline, to = dviolres_baseline),
    armedrobres_std_baseline = stdize(armedrobres_baseline, to = armedrobres_baseline),
    
    cnonviolentcrime_report_num_std = stdize(cnonviolentcrime_report_num, to = cnonviolentcrime_report_num_baseline),
    violentcrime_report_num_std_baseline = stdize(violentcrime_report_num_baseline, to = violentcrime_report_num_baseline),
    nonviolentcrime_report_num_std_baseline = stdize(nonviolentcrime_report_num_baseline, to = nonviolentcrime_report_num_baseline),
    cviolentcrime_report_num_std_baseline = stdize(cviolentcrime_report_num_baseline, to = cviolentcrime_report_num_baseline),
    cnonviolentcrime_report_num_std_baseline = stdize(cnonviolentcrime_report_num_baseline, to = cnonviolentcrime_report_num_baseline),
    # standardize individual crimes' reporting
    armedrob_report_std = stdize(armedrob_report, to = armedrob_report_baseline),
    simpleassault_report_std = stdize(simpleassault_report, to = simpleassault_report_baseline),
    other_report_violent_std = stdize(other_report_violent, to = other_report_violent_baseline),
    burglary_report_std = stdize(burglary_report, to = burglary_report_baseline),
    carmedrob_report_std = stdize(carmedrob_report, to = carmedrob_report_baseline),
    caggassault_report_std = stdize(caggassault_report, to = caggassault_report_baseline),
    csimpleassault_report_std = stdize(csimpleassault_report, to = csimpleassault_report_baseline),
    csexual_report_std = stdize(csexual_report, to = csexual_report_baseline),
    cdomestic_phys_report_std = stdize(cdomestic_phys_report, to = cdomestic_phys_report_baseline),
    cother_report_violent_std = stdize(cother_report_violent, to = cother_report_violent_baseline),
    cburglary_report_std = stdize(cburglary_report, to = cburglary_report_baseline),
    
    armedrob_report_std_baseline = stdize(armedrob_report_baseline, to = armedrob_report_baseline),
    simpleassault_report_std_baseline = stdize(simpleassault_report_baseline, to = simpleassault_report_baseline),
    other_report_violent_std_baseline = stdize(other_report_violent_baseline, to = other_report_violent_baseline),
    burglary_report_std_baseline = stdize(burglary_report_baseline, to = burglary_report_baseline),
    carmedrob_report_std_baseline = stdize(carmedrob_report_baseline, to = carmedrob_report_baseline),
    caggassault_report_std_baseline = stdize(caggassault_report_baseline, to = caggassault_report_baseline),
    csimpleassault_report_std_baseline = stdize(csimpleassault_report_baseline, to = csimpleassault_report_baseline),
    csexual_report_std_baseline = stdize(csexual_report_baseline, to = csexual_report_baseline),
    cdomestic_phys_report_std_baseline = stdize(cdomestic_phys_report_baseline, to = cdomestic_phys_report_baseline),
    cother_report_violent_std_baseline = stdize(cother_report_violent_baseline, to = cother_report_violent_baseline),
    cburglary_report_std_baseline = stdize(cburglary_report_baseline, to = cburglary_report_baseline),
    # calculate sub-indices
    crimeres_idx = idx_mean(burglaryres_std, dviolres_std, armedrobres_std, tx = Z, fe = police_zones),
    crimeres_idx_baseline = idx_mean(burglaryres_std_baseline, dviolres_std_baseline, armedrobres_std_baseline, tx = Z, fe = police_zones),
    # restandardize
    crimeres_idx = stdize(crimeres_idx, to = crimeres_idx_baseline),
    crimeres_idx_baseline = stdize(crimeres_idx_baseline, to = crimeres_idx_baseline),
    # calculate indices
    crime_reporting_idx = idx_mean(violentcrime_report_num_std, nonviolentcrime_report_num_std, cviolentcrime_report_num_std, cnonviolentcrime_report_num_std, crimeres_idx, tx = Z, fe = police_zones),
    crime_reporting_idx_baseline = idx_mean(violentcrime_report_num_std_baseline, nonviolentcrime_report_num_std_baseline, cviolentcrime_report_num_std_baseline, cnonviolentcrime_report_num_std_baseline, crimeres_idx_baseline, tx = Z, fe = police_zones),
    # restandardize
    crime_reporting_idx = stdize(crime_reporting_idx, to = crime_reporting_idx_baseline), 
    crime_reporting_idx_baseline = stdize(crime_reporting_idx_baseline, to = crime_reporting_idx_baseline)
  )

# clean for baseline (non collapsed data) Added by Fatiq Nadeem on 05-04-2021 to make country tables
lbr_citizen_baseline <- 
  lbr_citizen_baseline %>% 
  rowwise() %>% 
  mutate(
    # generate crime reporting categories
    armedrob_report_baseline = case_when(
      armedrob_num_baseline == 0 | armedrob_report_baseline == 0 ~ 0, 
      armedrob_num_baseline > 0 & armedrob_report_baseline == 1 ~ 1),
    burglary_report_baseline = case_when(
      burglary_num_baseline == 0 | burglary_report_baseline == 0 ~ 0,
      burglary_num_baseline > 0 & burglary_report_baseline == 1 ~ 1),
    simpleassault_report_baseline = case_when(
      simpleassault_num_baseline == 0 | simpleassault_report_baseline == 0 ~ 0,
      simpleassault_num_baseline > 0 & simpleassault_report_baseline == 1 ~ 1),
    carmedrob_report = case_when(
      carmedrob_num_baseline == 0 | carmedrob_report_baseline == 0 ~ 0,
      carmedrob_num_baseline > 0 & carmedrob_report_baseline == 1 ~ 1),
    cburglary_report_baseline = case_when(
      cburglary_num_baseline == 0 | cburglary_report_baseline == 0 ~ 0,
      cburglary_num_baseline > 0 & cburglary_report_baseline == 1 ~ 1),
    caggassault_report_baseline = case_when(
      caggassault_num_baseline == 0 | caggassault_report_baseline == 0 ~ 0,
      caggassault_num_baseline > 0 & caggassault_report_baseline == 1 ~ 1),
    csimpleassault_report_baseline = case_when(
      csimpleassault_num_baseline == 0 | csimpleassault_report_baseline == 0 ~ 0,
      csimpleassault_num_baseline > 0 & csimpleassault_report_baseline == 1 ~ 1),
    csexual_report_baseline = case_when(
      csexual_num_baseline == 0 | csexual_report_baseline == 0 ~ 0,
      csexual_num_baseline > 0 & csexual_report_baseline == 1 ~ 1),
    cdomestic_phys_report_baseline = case_when(
      cdomestic_phys_num_baseline == 0 | cdomestic_phys_report_baseline == 0 ~ 0,
      cdomestic_phys_num_baseline > 0 & cdomestic_phys_report_baseline == 1 ~ 1),
    # Note: cmurder_num not collected for lbr endline as per their pap
    # Note: cmurder_num not collected for lbr baseline as per their pap
    # cmurder_report_baseline = 
    # case_when(
    #  cmurder_num_baseline == 0 | cmurder_report_baseline == 0 ~ 0,
    #  cmurder_num_baseline > 0 & cmurder_report_baseline == 1 ~ 1)
    
    burglaryres_baseline = case_when(
      burglaryres_baseline == 1 ~ 1L,
      burglaryres_baseline == 2 ~ 1L,
      TRUE ~ 0L),
    dviolres_baseline = case_when(
      dviolres_baseline == 1 ~ 1L,
      dviolres_baseline == 2 ~ 1L,
      TRUE ~ 0L),
    armedrobres_baseline = case_when(
      armedrobres_baseline == 1 ~ 1L,
      armedrobres_baseline == 2 ~ 1L,
      TRUE ~ 0L),
    
    # calculate crime categories
    violentcrime_report_num_baseline = sum(armedrob_report_baseline, simpleassault_report_baseline, other_report_violent_baseline, na.rm = TRUE),
    nonviolentcrime_report_num_baseline = sum(burglary_report_baseline, other_report_nonviolent_baseline, na.rm = TRUE),
    cviolentcrime_report_num_baseline = sum(carmedrob_report_baseline, caggassault_report_baseline, csimpleassault_report_baseline, csexual_report_baseline, cdomestic_phys_report_baseline, 
                                            # cmurder_report_baseline, # cmurder_baseline was not collected as per mpap
                                            cother_report_violent_baseline, na.rm = TRUE),
    cnonviolentcrime_report_num_baseline = sum(cburglary_report_baseline, 
                                               # cother_report_nonviolent_baseline, 
                                               na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(
    # standardize all vars
    burglaryres_std_baseline = stdize(burglaryres_baseline, to = burglaryres_baseline),
    dviolres_std_baseline = stdize(dviolres_baseline, to = dviolres_baseline),
    armedrobres_std_baseline = stdize(armedrobres_baseline, to = armedrobres_baseline),
    
    violentcrime_report_num_std_baseline = stdize(violentcrime_report_num_baseline, to = violentcrime_report_num_baseline),
    nonviolentcrime_report_num_std_baseline = stdize(nonviolentcrime_report_num_baseline, to = nonviolentcrime_report_num_baseline),
    cviolentcrime_report_num_std_baseline = stdize(cviolentcrime_report_num_baseline, to = cviolentcrime_report_num_baseline),
    cnonviolentcrime_report_num_std_baseline = stdize(cnonviolentcrime_report_num_baseline, to = cnonviolentcrime_report_num_baseline),
    # standardize individual crimes' reporting
    armedrob_report_std_baseline = stdize(armedrob_report_baseline, to = armedrob_report_baseline),
    simpleassault_report_std_baseline = stdize(simpleassault_report_baseline, to = simpleassault_report_baseline),
    other_report_violent_std_baseline = stdize(other_report_violent_baseline, to = other_report_violent_baseline),
    burglary_report_std_baseline = stdize(burglary_report_baseline, to = burglary_report_baseline),
    carmedrob_report_std_baseline = stdize(carmedrob_report_baseline, to = carmedrob_report_baseline),
    caggassault_report_std_baseline = stdize(caggassault_report_baseline, to = caggassault_report_baseline),
    csimpleassault_report_std_baseline = stdize(csimpleassault_report_baseline, to = csimpleassault_report_baseline),
    csexual_report_std_baseline = stdize(csexual_report_baseline, to = csexual_report_baseline),
    cdomestic_phys_report_std_baseline = stdize(cdomestic_phys_report_baseline, to = cdomestic_phys_report_baseline),
    cother_report_violent_std_baseline = stdize(cother_report_violent_baseline, to = cother_report_violent_baseline),
    cburglary_report_std_baseline = stdize(cburglary_report_baseline, to = cburglary_report_baseline)
  )

# 2. common analysis
lbr_citizen <- 
  lbr_citizen %>% 
  rowwise() %>% 
  mutate(
    violentcrime_report_num_common = sum(armedrob_report, simpleassault_report, na.rm = TRUE),
    nonviolentcrime_report_num_common = burglary_report,
    cviolentcrime_report_num_common = sum(carmedrob_report, csimpleassault_report, csexual_report, cdomestic_phys_report, na.rm = TRUE),
    cnonviolentcrime_report_num_common = cburglary_report,
    
    violentcrime_report_num_common_baseline = sum(armedrob_report_baseline, simpleassault_report_baseline, na.rm = TRUE),
    nonviolentcrime_report_num_common_baseline = burglary_report_baseline,
    cviolentcrime_report_num_common_baseline = sum(carmedrob_report_baseline, csimpleassault_report_baseline, csexual_report_baseline, cdomestic_phys_report_baseline, na.rm = TRUE),
    cnonviolentcrime_report_num_common_baseline = cburglary_report_baseline) %>%
  ungroup() %>%
  mutate(    
    # standarduze all vars
    violentcrime_report_num_common_std = stdize(violentcrime_report_num, to = violentcrime_report_num_baseline),
    nonviolentcrime_report_num_common_std = stdize(nonviolentcrime_report_num, to = nonviolentcrime_report_num_baseline),
    cviolentcrime_report_num_common_std = stdize(cviolentcrime_report_num, to = cviolentcrime_report_num_baseline),
    cnonviolentcrime_report_num_common_std = stdize(cnonviolentcrime_report_num, to = cnonviolentcrime_report_num_baseline),
    
    violentcrime_report_num_common_std_baseline = stdize(violentcrime_report_num_baseline, to = violentcrime_report_num_baseline),
    nonviolentcrime_report_num_common_std_baseline = stdize(nonviolentcrime_report_num_baseline, to = nonviolentcrime_report_num_baseline),
    cviolentcrime_report_num_common_std_baseline = stdize(cviolentcrime_report_num_baseline, to = cviolentcrime_report_num_baseline),
    cnonviolentcrime_report_num_common_std_baseline = stdize(cnonviolentcrime_report_num_baseline, to = cnonviolentcrime_report_num_baseline),
    # calculate sub-index
    crimeres_idx_common = idx_mean(burglaryres_std, dviolres_std, tx = Z, fe = police_zones),
    crimeres_idx_common_baseline = idx_mean(burglaryres_std_baseline, dviolres_std_baseline, tx = Z, fe = police_zones),
    # restandardize
    crimeres_idx_common = stdize(crimeres_idx_common, to = crimeres_idx_common_baseline),
    crimeres_idx_common_baseline = stdize(crimeres_idx_common, to = crimeres_idx_common_baseline),
    # calculate index
    crime_reporting_idx_common = idx_mean(violentcrime_report_num_common_std, nonviolentcrime_report_num_common_std, cviolentcrime_report_num_common_std, cnonviolentcrime_report_num_common_std, crimeres_idx_common, tx = Z, fe = police_zones),
    crime_reporting_idx_common_baseline = idx_mean(violentcrime_report_num_common_std_baseline, nonviolentcrime_report_num_common_std_baseline, cviolentcrime_report_num_common_std_baseline, cnonviolentcrime_report_num_common_std_baseline, crimeres_idx_common_baseline, tx = Z, fe = police_zones),
    # restandardize
    crime_reporting_idx_common = stdize(crime_reporting_idx_common, to = crime_reporting_idx_common_baseline), 
    crime_reporting_idx_common_baseline = stdize(crime_reporting_idx_common_baseline, to = crime_reporting_idx_common_baseline)
    
  )

# 3. List-wise deletion
lbr_citizen <-
  lbr_citizen %>%
  mutate(
    # calculate sub-index
    crimeres_idx_listwise = idx_mean_listwise(burglaryres_std, dviolres_std),
    crimeres_idx_listwise_baseline = idx_mean_listwise(burglaryres_std_baseline, dviolres_std_baseline),
    # restandardize
    crimeres_idx_listwise = stdize(crimeres_idx_listwise, to = crimeres_idx_listwise_baseline),
    crimeres_idx_listwise_baseline = stdize(crimeres_idx_listwise, to = crimeres_idx_listwise_baseline),
    # calculate index
    crime_reporting_idx_listwise = idx_mean_listwise(violentcrime_report_num_common_std, nonviolentcrime_report_num_common_std, cviolentcrime_report_num_common_std, cnonviolentcrime_report_num_common_std, crimeres_idx_common),
    crime_reporting_idx_listwise_baseline = idx_mean_listwise(violentcrime_report_num_common_std_baseline, nonviolentcrime_report_num_common_std_baseline, cviolentcrime_report_num_common_std_baseline, cnonviolentcrime_report_num_common_std_baseline, crimeres_idx_common_baseline),
    # restandardize
    crime_reporting_idx_listwise = stdize(crime_reporting_idx_listwise, to = crime_reporting_idx_listwise_baseline), 
    crime_reporting_idx_listwise_baseline = stdize(crime_reporting_idx_listwise_baseline, to = crime_reporting_idx_listwise_baseline)
  )


# 4. Outlier deletion
lbr_citizen <-
  lbr_citizen %>%
  mutate(
    armedrob_report_outlier = na_if(armedrob_report, quantile(armedrob_report, 0.95, 0.95, na.rm = TRUE)),
    simpleassault_report_outlier = na_if(simpleassault_report, quantile(simpleassault_report, 0.95, na.rm = TRUE)),
    other_report_violent_outlier = na_if(other_report_violent, quantile(other_report_violent, 0.95, na.rm = TRUE)),
    burglary_report_outlier = na_if(burglary_report, quantile(burglary_report, 0.95, na.rm = TRUE)),
    # other_report_nonviolent_outlier = na_if(other_report_nonviolent, quantile(other_report_nonviolent, 0.95, na.rm = TRUE)),
    carmedrob_report_outlier = na_if(carmedrob_report, quantile(carmedrob_report, 0.95, na.rm = TRUE)),
    caggassault_report_outlier = na_if(caggassault_report, quantile(caggassault_report, 0.95, na.rm = TRUE)),
    csimpleassault_report_outlier = na_if(csimpleassault_report, quantile(csimpleassault_report, 0.95, na.rm = TRUE)),
    csexual_report_outlier = na_if(csexual_report, quantile(csexual_report, 0.95, na.rm = TRUE)),
    cdomestic_phys_report_outlier = na_if(cdomestic_phys_report, quantile(cdomestic_phys_report, 0.95, na.rm = TRUE)),
    cmurder_report_outlier = na_if(cmurder_report, quantile(cmurder_report, 0.95, na.rm = TRUE)),
    cother_report_violent_outlier = na_if(cother_report_violent, quantile(cother_report_violent, 0.95, na.rm = TRUE)),
    cburglary_report_outlier = na_if(cburglary_report, quantile(cburglary_report, 0.95, na.rm = TRUE)),
    # cother_report_nonviolent_outlier = na_if(cother_report_nonviolent, quantile(cother_report_nonviolent, 0.95, na.rm = TRUE)),
    
    armedrob_report_outlier_baseline = na_if(armedrob_report_baseline, quantile(armedrob_report_baseline, 0.95, na.rm = TRUE)),
    simpleassault_report_outlier_baseline = na_if(simpleassault_report_baseline, quantile(simpleassault_report_baseline, 0.95, na.rm = TRUE)),
    other_report_violent_outlier_baseline = na_if(other_report_violent_baseline, quantile(other_report_violent_baseline, 0.95, na.rm = TRUE)),
    burglary_report_outlier_baseline = na_if(burglary_report_baseline, quantile(burglary_report_baseline, 0.95, na.rm = TRUE)),
    # other_report_nonviolent_outlier_baseline = na_if(other_report_nonviolent_baseline, quantile(other_report_nonviolent_baseline, 0.95, na.rm = TRUE)),
    carmedrob_report_outlier_baseline = na_if(carmedrob_report_baseline, quantile(carmedrob_report_baseline, 0.95, na.rm = TRUE)),
    caggassault_report_outlier_baseline = na_if(caggassault_report_baseline, quantile(caggassault_report_baseline, 0.95, na.rm = TRUE)),
    csimpleassault_report_outlier_baseline = na_if(csimpleassault_report_baseline, quantile(csimpleassault_report_baseline, 0.95, na.rm = TRUE)),
    csexual_report_outlier_baseline = na_if(csexual_report_baseline, quantile(csexual_report_baseline, 0.95, na.rm = TRUE)),
    cdomestic_phys_report_outlier_baseline = na_if(cdomestic_phys_report_baseline, quantile(cdomestic_phys_report_baseline, 0.95, na.rm = TRUE)),
    # cmurder_report_outlier_baseline = na_if(cmurder_report_baseline, quantile(cmurder_report_baseline, 0.95, na.rm = TRUE)),
    cother_report_violent_outlier_baseline = na_if(cother_report_violent_baseline, quantile(cother_report_violent_baseline, 0.95, na.rm = TRUE)),
    cburglary_report_outlier_baseline = na_if(cburglary_report_baseline, quantile(cburglary_report_baseline, 0.95, na.rm = TRUE))
    # cother_report_nonviolent_outlier_baseline = na_if(cother_report_nonviolent_baseline, quantile(cother_report_nonviolent_baseline, 0.95, na.rm = TRUE))
  ) %>% 
  rowwise() %>%
  mutate(
    # sum all crimes into categories
    violentcrime_report_outlier = sum(armedrob_report_outlier, simpleassault_report_outlier, other_report_violent_outlier, na.rm = TRUE),
    nonviolentcrime_report_outlier = burglary_report_outlier,
    #, other_report_nonviolent_outlier, na.rm = TRUE),
    cviolentcrime_report_outlier = sum(carmedrob_report_outlier, caggassault_report_outlier, csimpleassault_report_outlier, csexual_report_outlier, cdomestic_phys_report_outlier, cmurder_report_outlier, cother_report_violent_outlier, na.rm = TRUE),
    cnonviolentcrime_report_outlier = cburglary_report_outlier, 
    # cother_report_nonviolent_outlier, na.rm = TRUE),
    violentcrime_report_outlier_baseline = sum(armedrob_report_outlier_baseline, simpleassault_report_outlier_baseline, other_report_violent_outlier_baseline, na.rm = TRUE),
    nonviolentcrime_report_outlier_baseline = burglary_report_outlier_baseline, 
    # other_report_nonviolent_outlier_baseline, na.rm = TRUE),
    cviolentcrime_report_outlier_baseline = sum(carmedrob_report_outlier_baseline, caggassault_report_outlier_baseline, csimpleassault_report_outlier_baseline, csexual_report_outlier_baseline, cdomestic_phys_report_outlier_baseline, cother_report_violent_outlier_baseline, na.rm = TRUE),
    cnonviolentcrime_report_outlier_baseline = cburglary_report_outlier_baseline, 
    # cother_report_nonviolent_outlier_baseline, na.rm = TRUE)
  ) %>%
  ungroup() %>% 
  mutate(
    # standardize categories
    violentcrime_report_outlier_std = stdize(violentcrime_report_outlier, to = violentcrime_report_outlier_baseline),
    nonviolentcrime_report_outlier_std = stdize(nonviolentcrime_report_outlier, to = nonviolentcrime_report_outlier_baseline),
    cviolentcrime_report_outlier_std = stdize(cviolentcrime_report_outlier, to = cviolentcrime_report_outlier_baseline),
    cnonviolentcrime_report_outlier_std = stdize(cnonviolentcrime_report_outlier, to = cnonviolentcrime_report_outlier_baseline),
    violentcrime_report_outlier_std_baseline = stdize(violentcrime_report_outlier_baseline, to = violentcrime_report_outlier_baseline),
    nonviolentcrime_report_outlier_std_baseline = stdize(nonviolentcrime_report_outlier_baseline, to = nonviolentcrime_report_outlier_baseline),
    cviolentcrime_report_outlier_std_baseline = stdize(cviolentcrime_report_outlier_baseline, to = cviolentcrime_report_outlier_baseline),
    cnonviolentcrime_report_outlier_std_baseline = stdize(cnonviolentcrime_report_outlier_baseline, to = cnonviolentcrime_report_outlier_baseline),
    # standardize individual crimes
    armedrob_report_outlier_std = stdize(armedrob_report, to = armedrob_report_baseline),
    simpleassault_report_outlier_std = stdize(simpleassault_report, to = simpleassault_report_baseline),
    other_report_violent_outlier_std = stdize(other_report_violent, to = other_report_violent_baseline),
    burglary_report_outlier_std = stdize(burglary_report, to = burglary_report_baseline),
    # other_report_nonviolent_outlier_std = stdize(other_report_nonviolent, to = other_report_nonviolent_baseline),
    carmedrob_report_outlier_std = stdize(carmedrob_report, to = carmedrob_report_baseline),
    caggassault_report_outlier_std = stdize(caggassault_report, to = caggassault_report_baseline),
    csimpleassault_report_outlier_std = stdize(csimpleassault_report, to = csimpleassault_report_baseline),
    csexual_report_outlier_std = stdize(csexual_report, to = csexual_report_baseline),
    cdomestic_phys_report_outlier_std = stdize(cdomestic_phys_report, to = cdomestic_phys_report_baseline),
    # cmurder_report_outlier_std = stdize(cmurder_report, to = cmurder_report_baseline),
    cother_report_violent_outlier_std = stdize(cother_report_violent, to = cother_report_violent_baseline),
    cburglary_report_outlier_std = stdize(cburglary_report, to = cburglary_report_baseline),
    # cother_report_nonviolent_outlier_std = stdize(cother_report_nonviolent, to = cother_report_nonviolent_baseline),
    
    armedrob_report_std_outlier_baseline = stdize(armedrob_report_baseline, to = armedrob_report_baseline),
    simpleassault_report_std_outlier_baseline = stdize(simpleassault_report_baseline, to = simpleassault_report_baseline),
    # other_report_violent_std_outlier_baseline = stdize(other_report_violent_baseline, to = other_report_violent_baseline),
    burglary_report_std_outlier_baseline = stdize(burglary_report_baseline, to = burglary_report_baseline),
    other_report_nonviolent_std_outlier_baseline = stdize(other_report_nonviolent_baseline, to = other_report_nonviolent_baseline),
    carmedrob_report_std_outlier_baseline = stdize(carmedrob_report_baseline, to = carmedrob_report_baseline),
    caggassault_report_std_outlier_baseline = stdize(caggassault_report_baseline, to = caggassault_report_baseline),
    csimpleassault_report_std_outlier_baseline = stdize(csimpleassault_report_baseline, to = csimpleassault_report_baseline),
    csexual_report_std_outlier_baseline = stdize(csexual_report_baseline, to = csexual_report_baseline),
    cdomestic_phys_report_std_outlier_baseline = stdize(cdomestic_phys_report_baseline, to = cdomestic_phys_report_baseline),
    # cmurder_report_std_outlier_baseline = stdize(cmurder_report_baseline, to = cmurder_report_baseline),
    cother_report_violent_std_outlier_baseline = stdize(cother_report_violent_baseline, to = cother_report_violent_baseline),
    cburglary_report_std_outlier_baseline = stdize(cburglary_report_baseline, to = cburglary_report_baseline),
    # cother_report_nonviolent_std_outlier_baseline = stdize(cother_report_nonviolent_baseline, to = cother_report_nonviolent_baseline),
    # generate crime index
    crime_report_idx_outlier = idx_mean(violentcrime_report_outlier_std, nonviolentcrime_report_outlier_std, cviolentcrime_report_outlier_std, cnonviolentcrime_report_outlier_std, tx = Z, fe = police_zones),
    crime_report_idx_outlier_baseline = idx_mean(violentcrime_report_outlier_std_baseline, nonviolentcrime_report_outlier_std_baseline, cviolentcrime_report_outlier_std_baseline, cnonviolentcrime_report_outlier_std_baseline, tx = Z, fe = police_zones),
    # re-standardize
    crime_report_idx_outlier = stdize(crime_report_idx_outlier, to = crime_report_idx_outlier_baseline),
    crime_report_idx_outlier_baseline = stdize(crime_report_idx_outlier_baseline, to = crime_report_idx_outlier_baseline)
  )


#--------------------------------------------------------------------------------------------------------------
# 6. Hypotheses 4(b): tips_idx
#--------------------------------------------------------------------------------------------------------------
lbr_citizen <- 
  lbr_citizen %>%
  mutate(
    # standardize vars
    contact_pol_susp_activity_std = stdize(contact_pol_susp_activity, to = contact_pol_susp_activity_baseline),
    give_info_pol_investigation_std = stdize(give_info_pol_investigation, to = give_info_pol_investigation_baseline),
    contact_pol_susp_activity_std_baseline = stdize(contact_pol_susp_activity_baseline, to = contact_pol_susp_activity_baseline),
    give_info_pol_investigation_std_baseline = stdize(give_info_pol_investigation_baseline, to = give_info_pol_investigation_baseline),
    # generate index
    crime_tips_idx = idx_mean(contact_pol_susp_activity_std, give_info_pol_investigation_std, tx = Z, fe = police_zones),
    crime_tips_idx_baseline = idx_mean(contact_pol_susp_activity_std_baseline, give_info_pol_investigation_std_baseline, tx = Z, fe = police_zones),
    # restandardize
    crime_tips_idx = stdize(crime_tips_idx, to = crime_tips_idx_baseline),
    crime_tips_idx_baseline = stdize(crime_tips_idx_baseline, to = crime_tips_idx_baseline),
    # make names consistent with mpap
    tips_idx = crime_tips_idx,
    tips_idx_baseline = crime_tips_idx_baseline
  )

# clean for baseline (non collapsed data) Added by Fatiq Nadeem on 05-04-2021 to make country tables
lbr_citizen_baseline <- 
  lbr_citizen_baseline %>% 
  mutate(
    contact_pol_susp_activity_std_baseline = stdize(contact_pol_susp_activity_baseline, to = contact_pol_susp_activity_baseline),
    give_info_pol_investigation_std_baseline = stdize(give_info_pol_investigation_baseline, to = give_info_pol_investigation_baseline),
  )


# 3. List-wise deletion
lbr_citizen <-
  lbr_citizen %>%
  mutate(
    crime_tips_idx_listwise = idx_mean_listwise(contact_pol_susp_activity_std, give_info_pol_investigation_std),
    crime_tips_idx_listwise_baseline = idx_mean_listwise(contact_pol_susp_activity_std_baseline, give_info_pol_investigation_std_baseline),
    # restandardize
    crime_tips_idx_listwise = stdize(crime_tips_idx_listwise, to = crime_tips_idx_listwise_baseline),
    crime_tips_idx_listwise_baseline = stdize(crime_tips_idx_listwise_baseline, to = crime_tips_idx_listwise_baseline),
    # make names consistent with mpap
    tips_idx_listwise = crime_tips_idx_listwise,
    tips_idx_listwise_baseline = crime_tips_idx_listwise_baseline)


# 4. Outlier analysis
lbr_citizen <- 
  lbr_citizen %>% 
  mutate(
    contact_pol_susp_activity_outlier = na_if(contact_pol_susp_activity, quantile(contact_pol_susp_activity, 0.95, na.rm = TRUE)),
    give_info_pol_investigation_outlier = na_if(give_info_pol_investigation, quantile(give_info_pol_investigation, 0.95, na.rm = TRUE)),
    contact_pol_susp_activity_outlier_baseline = na_if(contact_pol_susp_activity_baseline, quantile(contact_pol_susp_activity_baseline, 0.95, na.rm = TRUE)),
    give_info_pol_investigation_outlier_baseline = na_if(give_info_pol_investigation_baseline, quantile(give_info_pol_investigation_baseline, 0.95, na.rm = TRUE)),
    # standardize vars
    contact_pol_susp_activity_outlier_std = stdize(contact_pol_susp_activity_outlier, to = contact_pol_susp_activity_outlier_baseline),
    give_info_pol_investigation_outlier_std = stdize(give_info_pol_investigation_outlier, to = give_info_pol_investigation_outlier_baseline),
    contact_pol_susp_activity_outlier_std_baseline = stdize(contact_pol_susp_activity_outlier_baseline, to = contact_pol_susp_activity_outlier_baseline),
    give_info_pol_investigation_outlier_std_baseline = stdize(give_info_pol_investigation_outlier_baseline, to = give_info_pol_investigation_outlier_baseline),
    # generate index
    crime_tips_idx_outlier = idx_mean(contact_pol_susp_activity_outlier_std, give_info_pol_investigation_outlier_std, tx = Z, fe = police_zones),
    crime_tips_idx_outlier_baseline = idx_mean(contact_pol_susp_activity_outlier_std_baseline, give_info_pol_investigation_outlier_std_baseline, tx = Z, fe = police_zones),
    # re-standardize
    crime_tips_idx_outlier = stdize(crime_tips_idx_outlier, to = crime_tips_idx_outlier_baseline),
    crime_tips_idx_outlier_baseline = stdize(crime_tips_idx_outlier_baseline, to = crime_tips_idx_outlier_baseline),
    # make names consistent with mpap
    tips_idx_outlier = crime_tips_idx_outlier,
    tips_idx_outlier_baseline = crime_tips_idx_outlier_baseline)

#--------------------------------------------------------------------------------------------------------------
# 7. Hypotheses 4(c): police_abuse_report_idx
#--------------------------------------------------------------------------------------------------------------
lbr_citizen <- 
  lbr_citizen %>%
  mutate(
    # general police abuse components
    policeabuse_report = case_when(
      str_detect(policeabuse_verbal_report, paste0("\\b", "1", "\\b")) | str_detect(policeabuse_phys_report, paste0("\\b", "1", "\\b")) ~ 1L,
      !is.na(policeabuse_verbal_any) & !is.na(policeabuse_phys_any) ~ 0L), 
    policeabuse_report_baseline = case_when(
      policeabuse_verbal_report_baseline == 1 |
        policeabuse_phys_report_baseline == 1 ~ 1L,
      TRUE ~ 0L), 
    # standardize vars
    policeabuse_report_std = stdize(policeabuse_report, to = policeabuse_report_baseline),
    dutydrink_report_std = stdize(dutydrink_report, to = dutydrink_report_baseline),
    policebeating_report_std = stdize(policebeating_report, to = policebeating_report_baseline),
    policeabuse_report_std_baseline = stdize(policeabuse_report_baseline, to = policeabuse_report_baseline),
    dutydrink_report_std_baseline = stdize(dutydrink_report_baseline, to = dutydrink_report_baseline),
    policebeating_report_std_baseline = stdize(policebeating_report_baseline, to = policebeating_report_baseline),
    # calculate index
    police_abuse_report_idx = idx_mean(policeabuse_report_std, dutydrink_report_std, policebeating_report_std, tx = Z, fe = police_zones),
    police_abuse_report_idx_baseline = idx_mean(policeabuse_report_std_baseline, dutydrink_report_std_baseline, policebeating_report_std_baseline, tx = Z, fe = police_zones),
    # restandardize
    police_abuse_report_idx = stdize(police_abuse_report_idx, to = police_abuse_report_idx_baseline),
    police_abuse_report_idx_baseline = stdize(police_abuse_report_idx_baseline, to = police_abuse_report_idx_baseline)
  )

# clean for baseline (non collapsed data) Added by Fatiq Nadeem on 05-04-2021 to make country tables
lbr_citizen_baseline <- 
  lbr_citizen_baseline %>% 
  mutate(
    policeabuse_report_baseline = case_when(
      policeabuse_verbal_report_baseline == 1 |
        policeabuse_phys_report_baseline == 1 ~ 1L,
      !is.na(policeabuse_verbal_any_baseline) & !is.na(policeabuse_phys_any_baseline) ~ 0L), 
    # standardize vars
    policeabuse_report_std_baseline = stdize(policeabuse_report_baseline, to = policeabuse_report_baseline),
    dutydrink_report_std_baseline = stdize(dutydrink_report_baseline, to = dutydrink_report_baseline),
    policebeating_report_std_baseline = stdize(policebeating_report_baseline, to = policebeating_report_baseline)
  )


# 2. Common analysis
lbr_citizen <- 
  lbr_citizen %>%
  mutate(
    # general police abuse components
    policeabuse_report_common = case_when(
      policeabuse_verbal_report == 1 ~ 1L,
      !is.na(policeabuse_verbal_any) ~ 0L), 
    policeabuse_report_common_baseline = case_when(
      policeabuse_verbal_report_baseline == 1 ~ 1L,
      TRUE ~ 0L), 
    # standardize vars
    policeabuse_report_common_std = stdize(policeabuse_report_common, to = policeabuse_report_common_baseline),
    policeabuse_report_common_std_baseline = stdize(policeabuse_report_common_baseline, to = policeabuse_report_common_baseline),
    # calculate index
    police_abuse_report_idx_common = idx_mean(policeabuse_report_common_std, policebeating_report_std, tx = Z, fe = police_zones),
    police_abuse_report_idx_common_baseline = idx_mean(policeabuse_report_common_std_baseline, policebeating_report_std_baseline, tx = Z, fe = police_zones),
    # restandardize
    police_abuse_report_idx_common = stdize(police_abuse_report_idx_common, to = police_abuse_report_idx_common_baseline),
    police_abuse_report_idx_common_baseline = stdize(police_abuse_report_idx_common_baseline, to = police_abuse_report_idx_common_baseline)
  )

# Note: hypothesis 4c admin part not calculated because variables not collected

# 3. List-wise deletion
lbr_citizen <-
  lbr_citizen %>%
  mutate(
    police_abuse_report_idx_listwise = idx_mean_listwise(policeabuse_report_common_std, policebeating_report_std),
    police_abuse_report_idx_listwise_baseline = idx_mean_listwise(policeabuse_report_common_std_baseline, policebeating_report_std_baseline),
    # restandardize
    police_abuse_report_idx_listwise = stdize(police_abuse_report_idx_listwise, to = police_abuse_report_idx_listwise_baseline),
    police_abuse_report_idx_listwise_baseline = stdize(police_abuse_report_idx_listwise_baseline, to = police_abuse_report_idx_listwise_baseline))

# 4. Outlier analysis
lbr_citizen <- 
  lbr_citizen %>% 
  mutate(
    policeabuse_report_outlier = policeabuse_report,
    dutydrink_report_outlier = na_if(dutydrink_report, quantile(dutydrink_report, 0.95, na.rm = TRUE)),
    policebeating_report_outlier = na_if(policebeating_report, quantile(policebeating_report, 0.95, na.rm = TRUE)),
    policeabuse_report_outlier_baseline = policeabuse_report_baseline,
    dutydrink_report_outlier_baseline = na_if(dutydrink_report_baseline, quantile(dutydrink_report_baseline, 0.95, na.rm = TRUE)),
    policebeating_report_outlier_baseline = na_if(policebeating_report_baseline, quantile(policebeating_report_baseline, 0.95, na.rm = TRUE)),
    # standardize vars
    policeabuse_report_outlier_std = stdize(policeabuse_report_outlier, to = policeabuse_report_outlier_baseline),
    dutydrink_report_outlier_std = stdize(dutydrink_report_outlier, to = dutydrink_report_outlier_baseline),
    policebeating_report_outlier_std = stdize(policebeating_report_outlier, to = policebeating_report_outlier_baseline),
    policeabuse_report_outlier_std_baseline = stdize(policeabuse_report_outlier_baseline, to = policeabuse_report_outlier_baseline),
    dutydrink_report_outlier_std_baseline = stdize(dutydrink_report_outlier_baseline, to = dutydrink_report_outlier_baseline),
    policebeating_report_outlier_std_baseline = stdize(policebeating_report_outlier_baseline, to = policebeating_report_outlier_baseline),
    # generate index
    police_abuse_report_idx_outlier = idx_mean(policeabuse_report_outlier_std, dutydrink_report_outlier_std, policebeating_report_outlier_std, tx = Z, fe = police_zones),
    police_abuse_report_idx_outlier_baseline = idx_mean(policeabuse_report_outlier_std_baseline, dutydrink_report_outlier_std_baseline, policebeating_report_outlier_std_baseline, tx = Z, fe = police_zones),
    # re-standardize
    police_abuse_report_idx_outlier = stdize(police_abuse_report_idx_outlier, to = police_abuse_report_idx_outlier_baseline),
    police_abuse_report_idx_outlier_baseline = stdize(police_abuse_report_idx_outlier_baseline, to = police_abuse_report_idx_outlier_baseline)
  )

#--------------------------------------------------------------------------------------------------------------
# 8. Hypotheses M1a: intentions_idx
#--------------------------------------------------------------------------------------------------------------
lbr_citizen <- 
  lbr_citizen %>%
  mutate(
    polint_corrupt = 4 - polint_corrupt, 
    polint_corrupt_baseline = polint_corrupt_baseline,
    
    polint_corrupt_std = stdize(polint_corrupt, to = polint_corrupt_baseline),
    polint_quality_std = stdize(polint_quality, to = polint_quality_baseline),
    polint_corrupt_std_baseline = stdize(polint_corrupt_baseline, to = polint_corrupt_baseline),
    polint_quality_std_baseline = stdize(polint_quality_baseline, to = polint_quality_baseline),
    
    polcaseserious_std = stdize(polcaseserious, to = polcaseserious_baseline),
    polcasefair_std = stdize(polcasefair, to = polcasefair_baseline),
    polcaseserious_std_baseline = stdize(polcaseserious_baseline, to = polcaseserious_baseline),
    polcasefair_std_baseline = stdize(polcasefair_baseline, to = polcasefair_baseline), 
    
    polint_idx = idx_mean(polint_corrupt_std, polint_quality_std, tx = Z, fe = police_zones),
    polint_idx_baseline = idx_mean(polint_corrupt_std_baseline, polint_quality_std_baseline, tx = Z, fe = police_zones),
    
    intentions_idx = idx_mean(polint_idx, polcaseserious_std, polcasefair_std, tx = Z, fe = police_zones),
    intentions_idx_baseline = idx_mean(polint_idx_baseline, polcaseserious_std_baseline, polcasefair_std_baseline, tx = Z, fe = police_zones),
    # restandardize
    intentions_idx = stdize(intentions_idx, to = intentions_idx_baseline),
    intentions_idx_baseline = stdize(intentions_idx_baseline, to = intentions_idx_baseline)
  )

# clean for baseline (non collapsed data) Added by Fatiq Nadeem on 05-04-2021 to make country tables
lbr_citizen_baseline <- 
  lbr_citizen_baseline %>% 
  mutate(
    polint_corrupt_baseline = polint_corrupt_baseline,
    
    polint_corrupt_std_baseline = stdize(polint_corrupt_baseline, to = polint_corrupt_baseline),
    polint_quality_std_baseline = stdize(polint_quality_baseline, to = polint_quality_baseline),
    
    polcaseserious_std_baseline = stdize(polcaseserious_baseline, to = polcaseserious_baseline),
    polcasefair_std_baseline = stdize(polcasefair_baseline, to = polcasefair_baseline)
  )

# 3. List-wise deletion
lbr_citizen <-
  lbr_citizen %>%
  mutate(
    polint_idx_listwise = idx_mean_listwise(polint_corrupt_std, polint_quality_std),
    polint_idx_listwise_baseline = idx_mean_listwise(polint_corrupt_std_baseline, polint_quality_std_baseline),
    
    intentions_idx_listwise = idx_mean_listwise(polint_idx_listwise, polcaseserious_std, polcasefair_std),
    intentions_idx_listwise_baseline = idx_mean_listwise(polint_idx_listwise_baseline, polcaseserious_std_baseline, polcasefair_std_baseline),
    # restandardize
    intentions_idx_listwise = stdize(intentions_idx_listwise, to = intentions_idx_listwise_baseline),
    intentions_idx_listwise_baseline = stdize(intentions_idx_listwise_baseline, to = intentions_idx_listwise_baseline))

# 4. Outlier analysis
lbr_citizen <- 
  lbr_citizen %>% 
  mutate(
    polint_corrupt_outlier = na_if(polint_corrupt, quantile(polint_corrupt, 0.95, na.rm = TRUE)),
    polint_quality_outlier = na_if(polint_quality, quantile(polint_quality, 0.95, na.rm = TRUE)),
    polcaseserious_outlier = na_if(polcaseserious, quantile(polcaseserious, 0.95, na.rm = TRUE)),
    polcasefair_outlier = na_if(polcasefair, quantile(polcasefair, 0.95, na.rm = TRUE)),
    polint_corrupt_outlier_baseline = na_if(polint_corrupt_baseline, quantile(polint_corrupt_baseline, 0.95, na.rm = TRUE)),
    polint_quality_outlier_baseline = na_if(polint_quality_baseline, quantile(polint_quality_baseline, 0.95, na.rm = TRUE)),
    polcaseserious_outlier_baseline = na_if(polcaseserious_baseline, quantile(polcaseserious_baseline, 0.95, na.rm = TRUE)),
    polcasefair_outlier_baseline = na_if(polcasefair_baseline, quantile(polcasefair_baseline, 0.95, na.rm = TRUE)),
    # standardize vars
    polint_corrupt_outlier_std = stdize(polint_corrupt_outlier, to = polint_corrupt_outlier_baseline),
    polint_quality_outlier_std = stdize(polint_quality_outlier, to = polint_quality_outlier_baseline),
    polcaseserious_outlier_std = stdize(polcaseserious_outlier, to = polcaseserious_outlier_baseline),
    polcasefair_outlier_std = stdize(polcasefair_outlier, to = polcasefair_outlier_baseline),
    polint_corrupt_outlier_std_baseline = stdize(polint_corrupt_outlier_baseline, to = polint_corrupt_outlier_baseline),
    polint_quality_outlier_std_baseline = stdize(polint_quality_outlier_baseline, to = polint_quality_outlier_baseline),
    polcaseserious_outlier_std_baseline = stdize(polcaseserious_outlier_baseline, to = polcaseserious_outlier_baseline),
    polcasefair_outlier_std_baseline = stdize(polcasefair_outlier_baseline, to = polcasefair_outlier_baseline),
    # sub-index
    polint_idx_outlier = idx_mean(polint_corrupt_outlier_std, polint_quality_outlier_std, tx = Z, fe = police_zones),
    polint_idx_outlier_baseline = idx_mean(polint_corrupt_outlier_std_baseline, polint_quality_outlier_std_baseline, tx = Z, fe = police_zones),
    # re-standardize
    crime_victim_idx_outlier = stdize(polint_idx_outlier, to = polint_idx_outlier_baseline),
    polint_idx_outlier_baseline = stdize(polint_idx_outlier_baseline, to = polint_idx_outlier_baseline),
    # generate index
    intentions_idx = idx_mean(polint_idx_outlier, polcaseserious_outlier_std, polcasefair_outlier_std, tx = Z, fe = police_zones),
    intentions_idx_baseline = idx_mean(polint_idx_outlier_baseline, polcaseserious_outlier_std_baseline, polcasefair_outlier_std_baseline, tx = Z, fe = police_zones),
    # re-standardize
    intentions_idx = stdize(intentions_idx, to = intentions_idx_baseline),
    intentions_idx_baseline = stdize(intentions_idx_baseline, to = intentions_idx_baseline)
  )

#--------------------------------------------------------------------------------------------------------------
# 9. Hypotheses M1b: know_idx
#--------------------------------------------------------------------------------------------------------------
lbr_citizen <- 
  lbr_citizen %>%
  mutate(
    # rescale to recode into correct answer
    know_law_fees_rescaled = case_when(
      know_law_fees == 0 ~ 1L,
      know_law_fees == 1 ~ 0L,
      TRUE ~ as.integer(know_law_fees)),
    # know_law_fees_rescaled_baseline = case_when(
    #     know_law_fees_baseline == 0 ~ 1L,
    #     know_law_fees_baseline == 1 ~ 0L,
    #     TRUE ~ as.integer(know_law_fees_baseline)),
    know_law_fees_rescaled_baseline = know_law_fees_baseline,
    
    know_law_suspect_rescaled = case_when(
      know_law_suspect == 0 ~ 1L,
      know_law_suspect == 1 ~ 0L,
      TRUE ~ as.integer(know_law_suspect)),
    # These are rescaled in the construction file
    # know_law_suspect_rescaled_baseline = case_when(
    #     know_law_suspect_baseline == 0 ~ 1L,
    #     know_law_suspect_baseline == 1 ~ 0L,
    #     TRUE ~ as.integer(know_law_suspect_baseline)),
    # Note: know_law_vaw, know_report_followup, know_report_station and know_report_station were not collected at endline
    know_law_suspect_std = stdize(know_law_suspect_rescaled, to = know_law_suspect_baseline),
    know_law_lawyer_std = stdize(know_law_lawyer, to = know_law_lawyer_baseline),
    know_law_fees_std = stdize(know_law_fees_rescaled, to = know_law_fees_rescaled_baseline),
    # know_law_vaw_std = stdize(know_law_vaw, to = know_law_vaw_baseline),
    
    know_law_suspect_std_baseline = stdize(know_law_suspect_baseline, to = know_law_suspect_baseline),
    know_law_lawyer_std_baseline = stdize(know_law_lawyer_baseline, to = know_law_lawyer_baseline),
    know_law_fees_std_baseline = stdize(know_law_fees_rescaled_baseline, to = know_law_fees_rescaled_baseline),
    # know_law_vaw_baseline_std = stdize(know_law_vaw_baseline, to = know_law_vaw_baseline),
    # know_report_followup_std = stdize(know_report_followup, to = know_report_followup_baseline),
    know_report_station_std = stdize(know_report_station, to = know_report_station_baseline),
    # know_report_followup_baseline_std = stdize(know_report_followup_baseline, to = know_report_followup_baseline),
    know_report_station_std_baseline = stdize(know_report_station_baseline, to = know_report_station_baseline),
    # calculate index
    know_law_idx = idx_mean(know_law_suspect_std, know_law_lawyer_std, know_law_fees_std, tx = Z, fe = police_zones ),
    know_law_idx_baseline = idx_mean(know_law_suspect_std_baseline, know_law_lawyer_std_baseline, know_law_fees_std_baseline, tx = Z, fe = police_zones),
    
    know_report_idx = know_report_station_std,
    know_report_idx_baseline = know_report_station_std_baseline,
    
    know_idx = idx_mean(know_law_idx, know_report_station_std, tx = Z, fe = police_zones),
    know_idx_baseline = idx_mean(know_law_idx_baseline, know_report_station_std_baseline, tx = Z, fe = police_zones),
    # restandardize
    know_idx = stdize(know_idx, to = know_idx_baseline),
    know_idx_baseline = stdize(know_idx_baseline, to = know_idx_baseline)
  )

# clean for baseline (non collapsed data) Added by Fatiq Nadeem on 05-04-2021 to make country tables
lbr_citizen_baseline <- 
  lbr_citizen_baseline %>% 
  mutate(
    # rescale to recode into correct answer
    know_law_fees_rescaled_baseline = know_law_fees_baseline,
    
    know_law_suspect_std_baseline = stdize(know_law_suspect_baseline, to = know_law_suspect_baseline),
    know_law_lawyer_std_baseline = stdize(know_law_lawyer_baseline, to = know_law_lawyer_baseline),
    know_law_fees_std_baseline = stdize(know_law_fees_rescaled_baseline, to = know_law_fees_rescaled_baseline),
    know_report_station_std_baseline = stdize(know_report_station_baseline, to = know_report_station_baseline)
  )


# 2. common
lbr_citizen <-
  lbr_citizen %>%
  mutate(
    # calculate index
    know_idx_common = idx_mean(know_law_idx, know_report_station_std, tx = Z, fe = police_zones),
    know_idx_common_baseline = idx_mean(know_law_idx_baseline, know_report_station_std_baseline, tx = Z, fe = police_zones),
    # restandardize
    know_idx_common = stdize(know_idx_common, to = know_idx_common_baseline),
    know_idx_common_baseline = stdize(know_idx_common_baseline, to = know_idx_common_baseline)
    
  )

# 3. List-wise deletion
lbr_citizen <-
  lbr_citizen %>%
  mutate(
    know_report_idx_listwise = know_report_station_std,
    know_report_idx_listwise_baseline = know_report_station_std_baseline,
    
    know_law_idx_listwise = idx_mean_listwise(know_law_suspect_std, know_law_lawyer_std, know_law_fees_std),
    know_law_idx_listwise_baseline = idx_mean_listwise(know_law_suspect_std_baseline, know_law_lawyer_std_baseline, know_law_fees_std_baseline),
    
    know_idx_listwise = idx_mean_listwise(know_law_idx, know_report_station_std),
    know_idx_listwise_baseline = idx_mean_listwise(know_law_idx, know_report_station_std),
    
    # restandardize
    know_idx_listwise = stdize(know_idx_listwise, to = know_idx_listwise_baseline),
    know_idx_listwise_baseline = stdize(know_idx_listwise_baseline, to = know_idx_listwise_baseline)
    
  )

# 4. Outlier analysis
lbr_citizen <- 
  lbr_citizen %>% 
  mutate(
    know_report_station_outlier = na_if(know_report_station, quantile(know_report_station, 0.95, na.rm = TRUE)),
    know_law_suspect_outlier = na_if(know_law_suspect, quantile(know_law_suspect, 0.95, na.rm = TRUE)),
    know_law_lawyer_outlier = na_if(know_law_lawyer, quantile(know_law_lawyer, 0.95, na.rm = TRUE)),
    know_law_fees_outlier = na_if(know_law_fees, quantile(know_law_fees, 0.95, na.rm = TRUE)),
    
    know_report_station_outlier_baseline = na_if(know_report_station_baseline, quantile(know_report_station_baseline, 0.95, na.rm = TRUE)),
    know_law_suspect_outlier_baseline = na_if(know_law_suspect_baseline, quantile(know_law_suspect_baseline, 0.95, na.rm = TRUE)),
    know_law_lawyer_outlier_baseline = na_if(know_law_lawyer_baseline, quantile(know_law_lawyer_baseline, 0.95, na.rm = TRUE)),
    know_law_fees_outlier_baseline = na_if(know_law_fees_baseline, quantile(know_law_fees_baseline, 0.95, na.rm = TRUE)),
    # standardize vars
    know_report_station_outlier_std = stdize(know_report_station_outlier, to = know_report_station_outlier_baseline),
    know_law_suspect_outlier_std = stdize(know_law_suspect_outlier, to = know_law_suspect_outlier_baseline),
    know_law_lawyer_outlier_std = stdize(know_law_lawyer_outlier, to = know_law_lawyer_outlier_baseline),
    know_law_fees_outlier_std = stdize(know_law_fees_outlier, to = know_law_fees_outlier_baseline),
    
    know_report_station_outlier_std_baseline = stdize(know_report_station_outlier_baseline, to = know_report_station_outlier_baseline),
    know_law_suspect_outlier_std_baseline = stdize(know_law_suspect_outlier_baseline, to = know_law_suspect_outlier_baseline),
    know_law_lawyer_outlier_std_baseline = stdize(know_law_lawyer_outlier_baseline, to = know_law_lawyer_outlier_baseline),
    know_law_fees_outlier_std_baseline = stdize(know_law_fees_outlier_baseline, to = know_law_fees_outlier_baseline),
    # sub-index
    know_report_idx_outlier = know_report_station_outlier_std,
    know_report_idx_outlier_baseline = know_report_station_outlier_std_baseline,
    # re-standardize
    know_report_idx_outlier = stdize(know_report_idx_outlier, to = know_report_idx_outlier_baseline),
    know_report_idx_outlier_baseline = stdize(know_report_idx_outlier_baseline, to = know_report_idx_outlier_baseline),
    
    know_law_idx_outlier = idx_mean(know_law_suspect_outlier_std, know_law_lawyer_outlier_std, know_law_fees_outlier_std, tx = Z, fe = police_zones ),
    know_law_idx_outlier_baseline = idx_mean(know_law_suspect_outlier_std_baseline, know_law_lawyer_outlier_std_baseline, know_law_fees_outlier_std_baseline, tx = Z, fe = police_zones),
    # re-standardize
    know_law_idx_outlier = stdize(know_law_idx_outlier, to = know_law_idx_outlier_baseline),
    know_law_idx_outlier_baseline = stdize(know_law_idx_outlier_baseline, to = know_law_idx_outlier_baseline),
    # generate index
    know_idx_outlier = idx_mean(know_law_idx_outlier, know_report_idx_outlier, tx = Z, fe = police_zones),
    know_idx_outlier_baseline = idx_mean(know_law_idx_outlier_baseline, know_report_idx_outlier_baseline, tx = Z, fe = police_zones),
    # re-standardize
    know_idx_outlier = stdize(know_idx_outlier, to = know_idx_outlier_baseline),
    know_idx_outlier_baseline = stdize(know_idx_outlier_baseline, to = know_idx_outlier_baseline)
  )

#--------------------------------------------------------------------------------------------------------------
# 10. Hypotheses M1c: norm_idx
#--------------------------------------------------------------------------------------------------------------
lbr_citizen <- 
  lbr_citizen %>%
  mutate(
    # reverse order of variables to be consistent with the direction of the MPAP (MPAP page 75-76)
    reportnorm_theft_rescaled = 4 - reportnorm_theft,
    reportnorm_abuse_rescaled = 4 - reportnorm_abuse,
    # obeynorm_rescaled = 4 - obeynorm,  # removed following issue # 84 on Gh
    reportnorm_theft_rescaled_baseline = 4 - reportnorm_theft_baseline,
    reportnorm_abuse_rescaled_baseline = 4 - reportnorm_abuse_baseline,
    # obeynorm_rescaled_baseline = 4 - obeynorm_baseline, # removed following issue # 84 on GH
    # standardize the vars
    reportnorm_theft_std = stdize(reportnorm_theft_rescaled, to = reportnorm_theft_rescaled_baseline),
    reportnorm_abuse_std = stdize(reportnorm_abuse_rescaled, to = reportnorm_abuse_rescaled_baseline),
    obeynorm_std = stdize(obeynorm, to = obeynorm_baseline), # following issue # 84 on GH
    reportnorm_theft_std_baseline = stdize(reportnorm_theft_rescaled_baseline, to = reportnorm_theft_rescaled_baseline),
    reportnorm_abuse_std_baseline = stdize(reportnorm_abuse_rescaled_baseline, to = reportnorm_abuse_rescaled_baseline),
    obeynorm_std_baseline = stdize(obeynorm_baseline, to = obeynorm_baseline), # removed following issue # 84 on GH
    # calculate index
    norm_idx = idx_mean(reportnorm_theft_std, reportnorm_abuse_std, obeynorm_std, tx = Z, fe = police_zones),
    norm_idx_baseline = idx_mean(reportnorm_theft_std_baseline, reportnorm_abuse_std_baseline, obeynorm_std_baseline, tx = Z, fe = police_zones),
    # restandardize
    norm_idx = stdize(norm_idx, to = norm_idx_baseline),
    norm_idx_baseline = stdize(norm_idx_baseline, to = norm_idx_baseline)
  )

# clean for baseline (non collapsed data) Added by Fatiq Nadeem on 05-04-2021 to make country tables
lbr_citizen_baseline <- 
  lbr_citizen_baseline %>% 
  mutate(
    # reverse order of variables to be consistent with the direction of the MPAP (MPAP page 75-76)
    reportnorm_theft_rescaled_baseline = 4 - reportnorm_theft_baseline,
    reportnorm_abuse_rescaled_baseline = 4 - reportnorm_abuse_baseline,
    reportnorm_theft_std_baseline = stdize(reportnorm_theft_rescaled_baseline, to = reportnorm_theft_rescaled_baseline),
    reportnorm_abuse_std_baseline = stdize(reportnorm_abuse_rescaled_baseline, to = reportnorm_abuse_rescaled_baseline),
    obeynorm_std_baseline = stdize(obeynorm_baseline, to = obeynorm_baseline), # removed following issue # 84 on GH
  )


# 3. List-wise deletion
lbr_citizen <-
  lbr_citizen %>%
  mutate(
    norm_idx_listwise = idx_mean_listwise(reportnorm_theft_std, reportnorm_abuse_std, obeynorm_std),
    norm_idx_listwise_baseline = idx_mean_listwise(reportnorm_theft_std_baseline, reportnorm_abuse_std_baseline, obeynorm_std_baseline),
    # restandardize
    norm_idx_listwise = stdize(norm_idx_listwise, to = norm_idx_listwise_baseline),
    norm_idx_listwise_baseline = stdize(norm_idx_listwise_baseline, to = norm_idx_listwise_baseline)
  )

# 4. Outlier analysis
lbr_citizen <- 
  lbr_citizen %>% 
  mutate(
    reportnorm_theft_outlier = na_if(reportnorm_theft, quantile(reportnorm_theft, 0.95, na.rm = TRUE)),
    reportnorm_abuse_outlier = na_if(reportnorm_abuse, quantile(reportnorm_abuse, 0.95, na.rm = TRUE)),
    obeynorm_outlier = na_if(obeynorm, quantile(obeynorm, 0.95, na.rm = TRUE)),
    reportnorm_theft_outlier_baseline = na_if(reportnorm_theft_baseline, quantile(reportnorm_theft_baseline, 0.95, na.rm = TRUE)),
    reportnorm_abuse_outlier_baseline = na_if(reportnorm_abuse_baseline, quantile(reportnorm_abuse_baseline, 0.95, na.rm = TRUE)),
    obeynorm_outlier_baseline = na_if(obeynorm_baseline, quantile(obeynorm_baseline, 0.95, na.rm = TRUE)),
    # standardize vars
    reportnorm_theft_outlier_std = stdize(reportnorm_theft_outlier, to = reportnorm_theft_outlier_baseline),
    reportnorm_abuse_outlier_std = stdize(reportnorm_abuse_outlier, to = reportnorm_abuse_outlier_baseline),
    obeynorm_outlier_std = stdize(obeynorm_outlier, to = obeynorm_outlier_baseline),
    reportnorm_theft_outlier_std_baseline = stdize(reportnorm_theft_outlier_baseline, to = reportnorm_theft_outlier_baseline),
    reportnorm_abuse_outlier_std_baseline = stdize(reportnorm_abuse_outlier_baseline, to = reportnorm_abuse_outlier_baseline),
    obeynorm_outlier_std_baseline = stdize(obeynorm_outlier_baseline, to = obeynorm_outlier_baseline),
    # calculate index
    norm_idx_outlier = idx_mean(reportnorm_theft_outlier_std, reportnorm_abuse_outlier_std, obeynorm_outlier_std, tx = Z, fe = police_zones),
    norm_idx_outlier_baseline = idx_mean(reportnorm_theft_outlier_std_baseline, reportnorm_abuse_outlier_std_baseline, obeynorm_outlier_std_baseline, tx = Z, fe = police_zones),
    # re-standardize
    norm_idx_outlier = stdize(norm_idx_outlier, to = norm_idx_outlier_baseline),
    norm_idx_outlier_baseline = stdize(norm_idx_outlier_baseline, to = norm_idx_outlier_baseline)
  )

#--------------------------------------------------------------------------------------------------------------
# 11. Hypotheses M2a: police_capacity_idx
#--------------------------------------------------------------------------------------------------------------
lbr_citizen <- 
  lbr_citizen %>%
  mutate(
    # standardize the vars
    polcap_timely_std = stdize(polcap_timely, to = polcap_timely_baseline),
    polcap_investigate_std = stdize(polcap_investigate, to = polcap_investigate_baseline),
    polcap_timely_std_baseline = stdize(polcap_timely_baseline, to = polcap_timely_baseline),
    polcap_investigate_std_baseline = stdize(polcap_investigate_baseline, to = polcap_investigate_baseline),
    # calculate index
    police_capacity_idx = idx_mean(polcap_timely_std, polcap_investigate_std, tx = Z, fe = police_zones),
    police_capacity_idx_baseline = idx_mean(polcap_timely_std_baseline, polcap_investigate_std_baseline, tx = Z, fe = police_zones),
    # restandardize
    police_capacity_idx = stdize(police_capacity_idx, to = police_capacity_idx_baseline),
    police_capacity_idx_baseline = stdize(police_capacity_idx_baseline, to = police_capacity_idx_baseline)
  )

# clean for baseline (non collapsed data) Added by Fatiq Nadeem on 05-04-2021 to make country tables
lbr_citizen_baseline <- 
  lbr_citizen_baseline %>% 
  mutate(
    # standardize the vars
    polcap_timely_std_baseline = stdize(polcap_timely_baseline, to = polcap_timely_baseline),
    polcap_investigate_std_baseline = stdize(polcap_investigate_baseline, to = polcap_investigate_baseline)
  )



# 3. List-wise deletion
lbr_citizen <-
  lbr_citizen %>%
  mutate(
    police_capacity_idx_listwise = idx_mean_listwise(polcap_timely_std, polcap_investigate_std),
    police_capacity_idx_listwise_baseline = idx_mean_listwise(polcap_timely_std_baseline, polcap_investigate_std_baseline),
    # restandardize
    police_capacity_idx_listwise = stdize(police_capacity_idx_listwise, to = police_capacity_idx_listwise_baseline),
    police_capacity_idx_listwise_baseline = stdize(police_capacity_idx_listwise_baseline, to = police_capacity_idx_listwise_baseline)
  )

# 4. Outlier analysis
lbr_citizen <- 
  lbr_citizen %>% 
  mutate(
    polcap_timely_outlier = na_if(polcap_timely, quantile(polcap_timely, 0.95, na.rm = TRUE)),
    polcap_investigate_outlier = na_if(polcap_investigate, quantile(polcap_investigate, 0.95, na.rm = TRUE)),
    polcap_timely_outlier_baseline = na_if(polcap_timely_baseline, quantile(polcap_timely_baseline, 0.95, na.rm = TRUE)),
    polcap_investigate_outlier_baseline = na_if(polcap_investigate_baseline, quantile(polcap_investigate_baseline, 0.95, na.rm = TRUE)),
    # standardize vars
    polcap_timely_outlier_std = stdize(polcap_timely_outlier, to = polcap_timely_outlier_baseline),
    polcap_investigate_outlier_std = stdize(polcap_investigate_outlier, to = polcap_investigate_outlier_baseline),
    polcap_timely_outlier_std_baseline = stdize(polcap_timely_outlier_baseline, to = polcap_timely_outlier_baseline),
    polcap_investigate_outlier_std_baseline = stdize(polcap_investigate_outlier_baseline, to = polcap_investigate_outlier_baseline),
    # calculate index
    police_capacity_idx_outlier = idx_mean(polcap_timely_outlier_std, polcap_investigate_outlier_std, tx = Z, fe = police_zones),
    police_capacity_idx_outlier_baseline = idx_mean(polcap_timely_outlier_std_baseline, polcap_investigate_outlier_std_baseline, tx = Z, fe = police_zones),
    # re-standardize
    police_capacity_idx_outlier = stdize(police_capacity_idx_outlier, to = police_capacity_idx_outlier_baseline),
    police_capacity_idx_outlier_baseline = stdize(police_capacity_idx_outlier_baseline, to = police_capacity_idx_outlier_baseline)
  )

#--------------------------------------------------------------------------------------------------------------
# 12. Hypotheses M2b: responsive_act
#--------------------------------------------------------------------------------------------------------------
lbr_citizen <- 
  lbr_citizen %>%
  mutate(
    # retain the raw definition
    responsive_act_raw = responsive_act, 
    responsive_act_raw_baseline = responsive_act_baseline,
    # standardize the vars
    responsive_act_std = stdize(responsive_act, to = responsive_act_baseline),
    responsive_act_std_baseline = stdize(responsive_act_baseline, to = responsive_act_baseline),
    # generate similar name to the one on the mpap
    responsive_act = responsive_act_std,
    responsive_act_baseline = responsive_act_std_baseline
  )

# clean for baseline (non collapsed data) Added by Fatiq Nadeem on 05-04-2021 to make country tables
lbr_citizen_baseline <- 
  lbr_citizen_baseline %>% 
  mutate(
    # retain the raw definition
    responsive_act_raw_baseline = responsive_act_baseline,
  )


# 4. Outlier analysis
lbr_citizen <- 
  lbr_citizen %>% 
  mutate(
    responsive_act_outlier = na_if(responsive_act, quantile(responsive_act, 0.95, na.rm = TRUE)),
    responsive_act_outlier_baseline = na_if(responsive_act_baseline, quantile(responsive_act_baseline, 0.95, na.rm = TRUE)),
    # standardize vars
    responsive_act_outlier_std = stdize(responsive_act, to = responsive_act_baseline),
    responsive_act_outlier_std_baseline = stdize(responsive_act_baseline, to = responsive_act_baseline),
    # generate similar name to the one on the mpap
    responsive_act_outlier = responsive_act_outlier_std,
    responsive_act_outlier_baseline = responsive_act_outlier_std_baseline
  )

#--------------------------------------------------------------------------------------------------------------
# 13. Hypotheses S1: legit_trust
#--------------------------------------------------------------------------------------------------------------
lbr_citizen <- 
  lbr_citizen %>%
  mutate(
    # retain the raw definition
    legit_trust_raw = legit_trust, 
    legit_trust_raw_baseline = legit_trust_baseline,
    # standardize the vars
    legit_trust_std = stdize(legit_trust, to = legit_trust_baseline),
    legit_trust_std_baseline = stdize(legit_trust_baseline, to = legit_trust_baseline),
    # generate similar name to the one on the mpap
    legit_trust = legit_trust_std,
    legit_trust_baseline = legit_trust_std_baseline
  )

# clean for baseline (non collapsed data) Added by Fatiq Nadeem on 05-04-2021 to make country tables
lbr_citizen_baseline <- 
  lbr_citizen_baseline %>% 
  mutate(
    # retain the raw definition
    legit_trust_raw_baseline = legit_trust_baseline
  )


# 4. Outlier analysis
lbr_citizen <- 
  lbr_citizen %>% 
  mutate(
    legit_trust_outlier = na_if(legit_trust, quantile(legit_trust, 0.95, na.rm = TRUE)),
    legit_trust_outlier_baseline = na_if(legit_trust_baseline, quantile(legit_trust_baseline, 0.95, na.rm = TRUE)),
    # standardize vars
    legit_trust_outlier_std = stdize(legit_trust_outlier, to = legit_trust_outlier_baseline),
    legit_trust_outlier_std_baseline = stdize(legit_trust_outlier_baseline, to = legit_trust_outlier_baseline),
    # generate similar name to the one on the mpap
    legit_trust_outlier = legit_trust_outlier_std,
    legit_trust_outlier_baseline = legit_trust_outlier_std_baseline
  )


#--------------------------------------------------------------------------------------------------------------
# 14. Hypotheses S2: trust_community
#--------------------------------------------------------------------------------------------------------------
lbr_citizen <- 
  lbr_citizen %>%
  mutate(
    # retain the raw definition
    trust_community_raw = trust_community, 
    trust_community_raw_baseline = trust_community_baseline,
    # standardize the vars
    trust_community_std = stdize(trust_community, to = trust_community_baseline),
    trust_community_std_baseline = stdize(trust_community_baseline, to = trust_community_baseline),
    # generate similar name to the one on the mpap
    trust_community = trust_community_std,
    trust_community_baseline = trust_community_std_baseline
  )

# clean for baseline (non collapsed data) Added by Fatiq Nadeem on 05-04-2021 to make country tables
lbr_citizen_baseline <- 
  lbr_citizen_baseline %>% 
  mutate(
    # retain the raw definition
    trust_community_raw_baseline = trust_community_baseline
  )



# 4. Outlier analysis
lbr_citizen <- 
  lbr_citizen %>% 
  mutate(
    trust_community_outlier = na_if(trust_community, quantile(trust_community, 0.95, na.rm = TRUE)),
    trust_community_outlier_baseline = na_if(trust_community_baseline, quantile(trust_community_baseline, 0.95, na.rm = TRUE)),
    # standardize vars
    trust_community_outlier_std = stdize(trust_community_outlier, to = trust_community_outlier_baseline),
    trust_community_outlier_std_baseline = stdize(trust_community_outlier_baseline, to = trust_community_outlier_baseline),
    # generate similar name to the one on the mpap
    trust_community_outlier = legit_trust_outlier_std,
    trust_community_outlier_baseline = legit_trust_outlier_std_baseline
  )


#--------------------------------------------------------------------------------------------------------------
# 15. Hypotheses C: compliance_idx
#--------------------------------------------------------------------------------------------------------------
lbr_citizen <- 
  lbr_citizen %>%
  mutate(
    # reverse order of variables to be consistent with the direction of the MPAP (MPAP page 75-76)
    compliance_patrol_rescaled = (6 - compliance_patrol),
    compliance_freq_rescaled = (6 - compliance_freq),
    compliance_meeting_rescaled = (compliance_meeting),
    # standardize the vars
    compliance_patrol_std = stdize(compliance_patrol_rescaled, to = compliance_patrol_rescaled_baseline),
    compliance_freq_std = stdize(compliance_freq_rescaled, to = compliance_freq_rescaled_baseline),
    compliance_meeting_std = stdize(compliance_meeting_rescaled, to = compliance_meeting_rescaled_baseline),
    compliance_patrol_std_baseline = stdize(compliance_patrol_rescaled_baseline, to = compliance_patrol_rescaled_baseline),
    compliance_freq_std_baseline = stdize(compliance_freq_rescaled_baseline, to = compliance_freq_rescaled_baseline),
    compliance_meeting_std_baseline = stdize(compliance_meeting_rescaled_baseline, to = compliance_meeting_rescaled_baseline),
    # calculate index
    compliance_idx = idx_mean(compliance_patrol_std, compliance_freq_std, compliance_meeting_std, tx = Z, fe = police_zones),
    compliance_idx_baseline = idx_mean(compliance_patrol_std_baseline, compliance_freq_std_baseline, compliance_meeting_std_baseline, tx = Z, fe = police_zones),
    # restandardize
    compliance_idx = stdize(compliance_idx, to = compliance_idx_baseline),
    compliance_idx_baseline = stdize(compliance_idx_baseline, to = compliance_idx_baseline)
  )

# clean for baseline (non collapsed data) Added by Fatiq Nadeem on 05-04-2021 to make country tables
lbr_citizen_baseline <- 
  lbr_citizen_baseline %>% 
  mutate(
    # reverse order of variables to be consistent with the direction of the MPAP (MPAP page 75-76)
    compliance_patrol_rescaled_baseline = (6 - compliance_patrol_baseline),
    compliance_freq_rescaled_baseline = (6 - compliance_freq_baseline),
    compliance_meeting_rescaled_baseline = (compliance_meeting_baseline),
    # standardize the vars
    compliance_patrol_std_baseline = stdize(compliance_patrol_rescaled_baseline, to = compliance_patrol_rescaled_baseline),
    compliance_freq_std_baseline = stdize(compliance_freq_rescaled_baseline, to = compliance_freq_rescaled_baseline),
    compliance_meeting_std_baseline = stdize(compliance_meeting_rescaled_baseline, to = compliance_meeting_rescaled_baseline),
  )


# 3. List-wise deletion
lbr_citizen <-
  lbr_citizen %>%
  mutate(
    compliance_idx_listwise = idx_mean_listwise(compliance_patrol_std, compliance_freq_std, compliance_meeting_std),
    compliance_idx_listwise_baseline = idx_mean_listwise(compliance_patrol_std_baseline, compliance_freq_std_baseline, compliance_meeting_std_baseline),
    # restandardize
    compliance_idx_listwise = stdize(compliance_idx_listwise, to = compliance_idx_listwise_baseline),
    compliance_idx_listwise_baseline = stdize(compliance_idx_listwise_baseline, to = compliance_idx_listwise_baseline)
  )


# 4. Outlier analysis
lbr_citizen <- 
  lbr_citizen %>% 
  mutate(
    compliance_patrol_outlier = na_if(compliance_patrol, quantile(compliance_patrol, 0.95, na.rm = TRUE)),
    compliance_freq_outlier = na_if(compliance_freq, quantile(compliance_freq, 0.95, na.rm = TRUE)),
    compliance_meeting_outlier = na_if(compliance_meeting, quantile(compliance_meeting, 0.95, na.rm = TRUE)),
    
    compliance_patrol_outlier_baseline = na_if(compliance_patrol_baseline, quantile(compliance_patrol_baseline, 0.95, na.rm = TRUE)),
    compliance_freq_outlier_baseline = na_if(compliance_freq_baseline, quantile(compliance_freq_baseline, 0.95, na.rm = TRUE)),
    compliance_meeting_outlier_baseline = na_if(compliance_meeting_baseline, quantile(compliance_meeting_baseline, 0.95, na.rm = TRUE)),
    # standardize vars
    compliance_patrol_outlier_std = stdize(compliance_patrol_outlier, to = compliance_patrol_outlier_baseline),
    compliance_freq_outlier_std = stdize(compliance_freq_outlier, to = compliance_freq_outlier_baseline),
    compliance_meeting_outlier_std = stdize(compliance_meeting_outlier, to = compliance_meeting_outlier_baseline),
    
    compliance_patrol_outlier_std_baseline = stdize(compliance_patrol_outlier_baseline, to = compliance_patrol_outlier_baseline),
    compliance_freq_outlier_std_baseline = stdize(compliance_freq_outlier_baseline, to = compliance_freq_outlier_baseline),
    compliance_meeting_outlier_std_baseline = stdize(compliance_meeting_outlier_baseline, to = compliance_meeting_outlier_baseline),
    # generate similar name to the one on the mpap
    compliance_idx_outlier = idx_mean(compliance_patrol_outlier_std, compliance_freq_outlier_std, compliance_meeting_outlier_std, tx = Z, fe = police_zones),
    compliance_idx_outlier_baseline = idx_mean(compliance_patrol_outlier_std_baseline, compliance_freq_outlier_std_baseline, compliance_meeting_outlier_std_baseline, tx = Z, fe = police_zones),
    # re-standardize
    compliance_idx_outlier = stdize(compliance_idx_outlier, to = compliance_idx_outlier_baseline),
    compliance_idx_outlier_baseline = stdize(compliance_idx_outlier_baseline, to = compliance_idx_outlier_baseline)
  )

#--------------------------------------------------------------------------------------------------------------
# SECONDARY HYPOTHESES
#--------------------------------------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------------------------------
# 16. Hyp 1a. (alt. i): crime_victim_idx_admin
#--------------------------------------------------------------------------------------------------------------
lbr_admin <-
  lbr_admin %>%
  rowwise() %>%
  mutate(
    # generate categories of crimes
    aviolentcrime_num = sum(aarmedrob_num, asimpleassault_num, aaggassault_num, asexual_num, adomestic_phys_num, amurder_num, na.rm = TRUE),
    anonviolentcrime_num = aburglary_num,
    # other crimes are not differentiated based on violent and non-violent
    aviolentcrime_num_baseline = sum(aarmedrob_num_baseline, aaggassault_num_baseline, asexual_num_baseline, adomestic_phys_num_baseline, amurder_num_baseline, na.rm = TRUE),
    anonviolentcrime_num_baseline = sum(aburglary_num_baseline, na.rm = TRUE)) %>%
  ungroup %>%
  mutate(
    # standardize crimes
    aviolentcrime_num_std = stdize(aviolentcrime_num, to = aviolentcrime_num_baseline),
    anonviolentcrime_num_std = stdize(anonviolentcrime_num, to = anonviolentcrime_num_baseline),
    
    aviolentcrime_num_std_baseline = stdize(aviolentcrime_num_baseline, to = aviolentcrime_num_baseline),
    anonviolentcrime_num_std_baseline = stdize(anonviolentcrime_num_baseline, to = anonviolentcrime_num_baseline),
    
    aarmedrob_num_std = stdize(aarmedrob_num, to = aarmedrob_num_baseline),
    aaggassault_num_std = stdize(aaggassault_num, to = aaggassault_num_baseline),
    asexual_num_std = stdize(asexual_num, to = asexual_num_baseline),
    adomestic_phys_num_std = stdize(adomestic_phys_num, to = adomestic_phys_num_baseline),
    amurder_num_std = stdize(amurder_num, to = amurder_num_baseline),
    # aother_num_violent_std = stdize(aother_num_violent, to = aother_num_violent_baseline),
    aburglary_num_std = stdize(aburglary_num, to = aburglary_num_baseline),
    # aother_num_nonviolent_std = stdize(aother_num_nonviolent, to = aother_num_nonviolent_baseline),
    
    aarmedrob_num_std_baseline = stdize(aarmedrob_num_baseline, to = aarmedrob_num_baseline),
    aaggassault_num_std_baseline = stdize(aaggassault_num_baseline, to = aaggassault_num_baseline),
    asexual_num_std_baseline = stdize(asexual_num_baseline, to = asexual_num_baseline),
    adomestic_phys_num_std_baseline = stdize(adomestic_phys_num_baseline, to = adomestic_phys_num_baseline),
    amurder_num_std_baseline = stdize(amurder_num_baseline, to = amurder_num_baseline),
    # aother_num_violent_std_baseline = stdize(aother_num_violent_baseline, to = aother_num_violent_baseline),
    aburglary_num_std_baseline = stdize(aburglary_num_baseline, to = aburglary_num_baseline),
    # aother_num_nonviolent_std_baseline = stdize(aother_num_nonviolent_baseline, to = aother_num_nonviolent_baseline),
    
    # generate index
    crime_victim_idx_admin = idx_mean(aviolentcrime_num_std, anonviolentcrime_num_std, tx = Z, fe = police_zones),
    crime_victim_idx_admin_baseline = idx_mean(aviolentcrime_num_std_baseline, anonviolentcrime_num_std_baseline, tx = Z, fe = police_zones),
    # restandardize
    crime_victim_idx_admin = stdize(crime_victim_idx_admin, to = crime_victim_idx_admin_baseline),
    crime_victim_idx_admin_baseline = stdize(crime_victim_idx_admin_baseline, to = crime_victim_idx_admin_baseline)
  )

# 3. List-wise deletion
lbr_admin <-
  lbr_admin %>%
  mutate(
    crime_victim_idx_admin_listwise = idx_mean_listwise(aviolentcrime_num_std, anonviolentcrime_num_std),
    crime_victim_idx_admin_listwise_baseline = idx_mean_listwise(aviolentcrime_num_std_baseline, anonviolentcrime_num_std_baseline),
    # restandardize
    crime_victim_idx_admin_listwise = stdize(crime_victim_idx_admin_listwise, to = crime_victim_idx_admin_listwise_baseline),
    crime_victim_idx_admin_listwise_baseline = stdize(crime_victim_idx_admin_listwise_baseline, to = crime_victim_idx_admin_listwise_baseline)
  )

#--------------------------------------------------------------------------------------------------------------
# 17. Hyp 1a. (alt. ii): crime_victim_idx_exp
#--------------------------------------------------------------------------------------------------------------
lbr_citizen <- 
  lbr_citizen %>% 
  rowwise() %>% 
  mutate(
    # generate categories of crimes
    violentcrime_num_exp = sum(armedrob_num, aggassault_num, sexual_num, domestic_phys_num, simpleassault_num, other_any_violent, na.rm = TRUE),
    nonviolentcrime_num_exp = sum(burglary_num, domestic_verbal_num, other_any_nonviolent, na.rm = TRUE),
    cviolentcrime_num_exp = sum(carmedrob_num, caggassault_num, csimpleassault_num, csexual_num, cdomestic_phys_num, cmurder_num, cmob_num, cother_any_violent, na.rm = TRUE),
    cnonviolentcrime_num_exp = sum(cburglary_num, cdomestic_verbal_num, cother_any_nonviolent, na.rm = TRUE),
    
    violentcrime_num_exp_baseline = sum(armedrob_num_baseline, aggassault_num_baseline, domestic_phys_num_baseline, simpleassault_num_baseline, other_any_violent_baseline, na.rm = TRUE),
    # Note: sexual_num_baseline not asked # see github issue # 101 land_any
    nonviolentcrime_num_exp_baseline = sum(burglary_num_baseline, land_any_baseline, other_any_nonviolent_baseline, na.rm = TRUE),
    cviolentcrime_num_exp_baseline = sum(carmedrob_num_baseline, caggassault_num_baseline, csimpleassault_num_baseline, csexual_num_baseline, cdomestic_phys_num_baseline, cmurder_num_baseline, cmob_num_baseline, cother_any_violent_baseline, na.rm = TRUE),
    cnonviolentcrime_num_exp_baseline = sum(cburglary_num_baseline, cother_any_nonviolent_baseline, na.rm = TRUE)
    # Note: domestic_verbal_num, cdomestic_verbal_num_baseline was not asked and cland_any were not collected cland_any
  ) %>% 
  ungroup() %>% 
  mutate(
    # standardize crimes
    violentcrime_num_exp_std = stdize(violentcrime_num_exp, to = violentcrime_num_exp_baseline),
    nonviolentcrime_num_exp_std = stdize(nonviolentcrime_num_exp, to = nonviolentcrime_num_exp_baseline),
    cviolentcrime_num_exp_std = stdize(cviolentcrime_num_exp, to = cviolentcrime_num_exp_baseline),
    cnonviolentcrime_num_exp_std = stdize(cnonviolentcrime_num_exp, to = cnonviolentcrime_num_exp_baseline),
    
    violentcrime_num_exp_std_baseline = stdize(violentcrime_num_exp_baseline, to = violentcrime_num_exp_baseline),
    nonviolentcrime_num_exp_std_baseline = stdize(nonviolentcrime_num_exp_baseline, to = nonviolentcrime_num_exp_baseline),
    cviolentcrime_num_exp_std_baseline = stdize(cviolentcrime_num_exp_baseline, to = cviolentcrime_num_exp_baseline),
    cnonviolentcrime_num_exp_std_baseline = stdize(cnonviolentcrime_num_exp_baseline, to = cnonviolentcrime_num_exp_baseline),
    
    armedrob_num_std = stdize(armedrob_num, to = armedrob_num_baseline),
    aggassault_num_std = stdize(aggassault_num, to = aggassault_num_baseline),
    sexual_num_std = stdize(sexual_num, to = sexual_num_baseline),
    domestic_phys_num_std = stdize(domestic_phys_num, to = domestic_phys_num_baseline),
    simpleassault_num_std = stdize(simpleassault_num, to = simpleassault_num_baseline),
    other_any_violent_std = stdize(other_any_violent, to = other_any_violent_baseline),
    burglary_num_std = stdize(burglary_num, to = burglary_num_baseline),
    domestic_verbal_num_std = stdize(domestic_verbal_num, domestic_verbal_num_baseline),
    # land_any_std = stdize(land_any, to = land_any_baseline),
    other_any_nonviolent_std = stdize(other_any_nonviolent, to = other_any_nonviolent_baseline),
    carmedrob_num_std = stdize(carmedrob_num, to = carmedrob_num_baseline),
    caggassault_num_std = stdize(caggassault_num, to = caggassault_num_baseline),
    csimpleassault_num_std = stdize(csimpleassault_num, to = csimpleassault_num_baseline),
    csexual_num_std = stdize(csexual_num, to = csexual_num_baseline),
    cdomestic_phys_num_std = stdize(cdomestic_phys_num, to = cdomestic_phys_num_baseline),
    cmurder_num_std = stdize(cmurder_num, to = cmurder_num_baseline),
    cmob_num_std = stdize(cmob_num, to = cmob_num_baseline),
    cother_any_violent_std = stdize(cother_any_violent, to = cother_any_violent_baseline),
    cburglary_num_std = stdize(cburglary_num, to = cburglary_num_baseline),
    cdomestic_verbal_num_std = stdize(cdomestic_verbal_num, cdomestic_verbal_num_baseline),
    # cland_any_std = stdize(cland_any, condition = Z_control == 1),
    cother_any_nonviolent_std = stdize(cother_any_nonviolent, to = cother_any_nonviolent_baseline),
    
    armedrob_num_std_baseline = stdize(armedrob_num_baseline, to = armedrob_num_baseline),
    aggassault_num_std_baseline = stdize(aggassault_num_baseline, to = aggassault_num_baseline),
    domestic_phys_num_std_baseline = stdize(domestic_phys_num_baseline, to = domestic_phys_num_baseline),
    simpleassault_num_std_baseline = stdize(simpleassault_num_baseline, to = simpleassault_num_baseline),
    other_any_violent_std_baseline = stdize(other_any_violent_baseline, to = other_any_violent_baseline),
    burglary_num_std_baseline = stdize(burglary_num_baseline, to = burglary_num_baseline),
    land_any_std_baseline = stdize(land_any_baseline, to = land_any_baseline),
    other_any_nonviolent_std_baseline = stdize(other_any_nonviolent_baseline, to = other_any_nonviolent_baseline),
    carmedrob_num_std_baseline = stdize(carmedrob_num_baseline, to = carmedrob_num_baseline),
    caggassault_num_std_baseline = stdize(caggassault_num_baseline, to = caggassault_num_baseline),
    csimpleassault_num_std_baseline = stdize(csimpleassault_num_baseline, to = csimpleassault_num_baseline),
    csexual_num_std_baseline = stdize(csexual_num_baseline, to = csexual_num_baseline),
    cmurder_num_std_baseline = stdize(cmurder_num_baseline, to = cmurder_num_baseline),
    cmob_num_std_baseline = stdize(cmob_num_baseline, to = cmob_num_baseline),
    cother_any_violent_std_baseline = stdize(cother_any_violent_baseline, to = cother_any_violent_baseline),
    cburglary_num_std_baseline = stdize(cburglary_num_baseline, to = cburglary_num_baseline),
    cother_any_nonviolent_std_baseline = stdize(cother_any_nonviolent_baseline, to = cother_any_nonviolent_baseline),
    sexual_num_std_baseline = stdize(sexual_num_baseline, sexual_num_baseline),
    domestic_verbal_num_std_baseline = stdize(domestic_verbal_num_baseline, to = domestic_verbal_num_baseline),
    cdomestic_verbal_num_std_baseline = stdize(cdomestic_verbal_num_baseline, to = cdomestic_verbal_num_baseline),
    
    # generate index
    crime_victim_idx_exp = idx_mean(violentcrime_num_exp_std, nonviolentcrime_num_exp_std, cviolentcrime_num_exp_std, cnonviolentcrime_num_exp_std, tx = Z, fe = police_zones),
    crime_victim_idx_exp_baseline = idx_mean(violentcrime_num_exp_std_baseline, nonviolentcrime_num_exp_std_baseline, cviolentcrime_num_exp_std_baseline, cnonviolentcrime_num_exp_std_baseline, tx = Z, fe = police_zones),
    # restandardize
    crime_victim_idx_exp = stdize(crime_victim_idx_exp, to = crime_victim_idx_exp_baseline),
    crime_victim_idx_exp_baseline = stdize(crime_victim_idx_exp_baseline, to = crime_victim_idx_exp_baseline)
  )

# 3. List-wise deletion
lbr_citizen <-
  lbr_citizen %>%
  mutate(
    crime_victim_idx_exp_listwise = idx_mean_listwise(violentcrime_num_exp_std, nonviolentcrime_num_exp_std, cviolentcrime_num_exp_std, cnonviolentcrime_num_exp_std),
    crime_victim_idx_exp_listwise_baseline = idx_mean_listwise(violentcrime_num_exp_std_baseline, nonviolentcrime_num_exp_std_baseline, cviolentcrime_num_exp_std_baseline, cnonviolentcrime_num_exp_std_baseline),
    # restandardize
    crime_victim_idx_exp_listwise = stdize(crime_victim_idx_exp_listwise, to = crime_victim_idx_exp_listwise_baseline),
    crime_victim_idx_exp_listwise_baseline = stdize(crime_victim_idx_exp_listwise_baseline, to = crime_victim_idx_exp_listwise_baseline)
  )


#--------------------------------------------------------------------------------------------------------------
# 18. Hyp 1a. (alt. iii) crime_victim_idx_bin
#--------------------------------------------------------------------------------------------------------------
lbr_citizen <- 
  lbr_citizen %>% 
  mutate(
    armedrob_bin = case_when(
      armedrob_num > 0 ~ 1L,
      armedrob_num == 0 ~ 0L,
      TRUE ~ NA_integer_),
    simpleassault_bin =  case_when(
      simpleassault_num > 0 ~ 1L,
      simpleassault_num == 0 ~ 0L,
      TRUE ~ NA_integer_),
    burglary_bin = case_when(
      burglary_num > 0 ~ 1L,
      burglary_num == 0 ~ 0L,
      TRUE ~ NA_integer_),
    carmedrob_bin = case_when(
      carmedrob_num > 0 ~ 1L,
      carmedrob_num == 0 ~ 0L,
      TRUE ~ NA_integer_),
    caggassault_bin = case_when(
      caggassault_num > 0 ~ 1L,
      caggassault_num == 0 ~ 0L,
      TRUE ~ NA_integer_),
    csexual_bin = case_when(
      csexual_num > 0 ~ 1L,
      csexual_num == 0 ~ 0L,
      TRUE ~ NA_integer_),
    cdomestic_phys_bin = case_when(
      cdomestic_phys_num > 0 ~ 1L,
      cdomestic_phys_num == 0 ~ 0L,
      TRUE ~ NA_integer_),
    cmurder_bin = case_when(
      cmurder_num > 0 ~ 1L,
      cmurder_num == 0 ~ 0L,
      TRUE ~ NA_integer_),
    cburglary_bin = case_when(
      cburglary_num > 0 ~ 1L,
      cburglary_num == 0 ~ 0L,
      TRUE ~ NA_integer_),
    csimpleassault_bin = case_when(
      csimpleassault_num > 0 ~ 1L,
      csimpleassault_num == 0 ~ 0L,
      TRUE ~ NA_integer_),
    
    armedrob_bin_baseline = case_when(
      armedrob_num_baseline > 0 ~ 1L,
      armedrob_num_baseline == 0 ~ 0L,
      TRUE ~ NA_integer_),
    simpleassault_bin_baseline = case_when(
      simpleassault_num_baseline > 0 ~ 1L,
      simpleassault_num_baseline == 0 ~ 0L,
      TRUE ~ NA_integer_),
    burglary_bin_baseline = case_when(
      burglary_num_baseline > 0 ~ 1L,
      burglary_num_baseline == 0 ~ 0L,
      TRUE ~ NA_integer_),
    carmedrob_bin_baseline = case_when(
      carmedrob_num_baseline > 0 ~ 1L,
      carmedrob_num_baseline == 0 ~ 0L,
      TRUE ~ NA_integer_),
    caggassault_bin_baseline = case_when(
      caggassault_num_baseline > 0 ~ 1L,
      caggassault_num_baseline == 0 ~ 0L,
      TRUE ~ NA_integer_),
    csexual_bin_baseline = case_when(
      csexual_num_baseline > 0 ~ 1L,
      csexual_num_baseline == 0 ~ 0L,
      TRUE ~ NA_integer_),
    cdomestic_phys_bin_baseline = case_when(
      cdomestic_phys_num_baseline > 0 ~ 1L,
      cdomestic_phys_num_baseline == 0 ~ 0L,
      TRUE ~ NA_integer_),
    cmurder_bin_baseline = case_when(
      cmurder_num_baseline > 0 ~ 1L,
      cmurder_num_baseline == 0 ~ 0L,
      TRUE ~ NA_integer_),
    cburglary_bin_baseline = case_when(
      cburglary_num_baseline > 0 ~ 1L,
      cburglary_num_baseline == 0 ~ 0L,
      TRUE ~ NA_integer_),
    csimpleassault_bin_baseline = case_when(
      csimpleassault_num_baseline > 0 ~ 1L,
      csimpleassault_num_baseline == 0 ~ 0L,
      TRUE ~ NA_integer_)) %>% 
  rowwise() %>% 
  mutate(
    # sum all crimes into categories
    violentcrime_bin =  sum(armedrob_bin, simpleassault_bin, other_any_violent, na.rm = TRUE),
    nonviolentcrime_bin = sum(burglary_bin, other_any_nonviolent, na.rm = TRUE),
    cviolentcrime_bin = sum(carmedrob_bin, caggassault_bin, csimpleassault_bin, csexual_bin, cdomestic_phys_bin, cmurder_bin, cother_any_violent, na.rm = TRUE),
    cnonviolentcrime_bin = sum(cburglary_bin, cother_any_nonviolent, na.rm = TRUE),
    
    violentcrime_bin_baseline = sum(armedrob_bin_baseline, simpleassault_bin_baseline, other_any_violent_baseline, na.rm = TRUE),
    nonviolentcrime_bin_baseline = sum(burglary_bin_baseline, other_any_nonviolent_baseline, na.rm = TRUE),
    cviolentcrime_bin_baseline = sum(carmedrob_bin_baseline, caggassault_bin_baseline, csimpleassault_bin_baseline, csexual_bin_baseline, cdomestic_phys_bin_baseline, cmurder_bin_baseline, cother_any_violent_baseline, na.rm = TRUE),
    cnonviolentcrime_bin_baseline = sum(cburglary_bin_baseline, cother_any_nonviolent_baseline, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(
    violentcrime_bin_std = stdize(violentcrime_bin, to = violentcrime_bin_baseline),
    nonviolentcrime_bin_std = stdize(nonviolentcrime_bin, to = nonviolentcrime_bin_baseline),
    cviolentcrime_bin_std = stdize(cviolentcrime_bin, to = cviolentcrime_bin_baseline),
    cnonviolentcrime_bin_std = stdize(cnonviolentcrime_bin, to = cnonviolentcrime_bin_baseline),
    
    violentcrime_bin_std_baseline = stdize(violentcrime_bin_baseline, to = violentcrime_bin_baseline),
    nonviolentcrime_bin_std_baseline = stdize(nonviolentcrime_bin_baseline, to = nonviolentcrime_bin_baseline),
    cviolentcrime_bin_std_baseline = stdize(cviolentcrime_bin_baseline, to = cviolentcrime_bin_baseline),
    cnonviolentcrime_bin_std_baseline = stdize(cnonviolentcrime_bin_baseline, to = cnonviolentcrime_bin_baseline),
    
    armedrob_bin_std = stdize(armedrob_bin, to = armedrob_bin_baseline),
    simpleassault_bin_std = stdize(simpleassault_bin, to = simpleassault_bin_baseline),
    other_any_violent_std = stdize(other_any_violent, to = other_any_violent_baseline),
    burglary_bin_std = stdize(burglary_bin, to = burglary_bin_baseline),
    other_any_nonviolent_std = stdize(other_any_nonviolent, to = other_any_nonviolent_baseline),
    carmedrob_bin_std = stdize(carmedrob_bin, to = carmedrob_bin_baseline),
    caggassault_bin_std = stdize(caggassault_bin, to = caggassault_bin_baseline),
    csimpleassault_bin_std = stdize(csimpleassault_bin, to = csimpleassault_bin_baseline),
    csexual_bin_std = stdize(csexual_bin, to = csexual_bin_baseline),
    cdomestic_phys_bin_std = stdize(cdomestic_phys_bin, to = cdomestic_phys_bin_baseline),
    cmurder_bin_std = stdize(cmurder_bin, to = cmurder_bin_baseline),
    cother_any_violent_std = stdize(cother_any_violent, to = cother_any_violent_baseline),
    cburglary_bin_std = stdize(cburglary_bin, to = cburglary_bin_baseline),
    cother_any_nonviolent_std = stdize(cother_any_nonviolent, to = cother_any_nonviolent_baseline),
    
    armedrob_bin_std_baseline = stdize(armedrob_bin_baseline, to = armedrob_bin_baseline),
    simpleassault_bin_std_baseline = stdize(simpleassault_bin_baseline, to = simpleassault_bin_baseline),
    other_any_violent_std_baseline = stdize(other_any_violent_baseline, to = other_any_violent_baseline),
    burglary_bin_std_baseline = stdize(burglary_bin_baseline, to = burglary_bin_baseline),
    other_any_nonviolent_std_baseline = stdize(other_any_nonviolent_baseline, to = other_any_nonviolent_baseline),
    carmedrob_bin_std_baseline = stdize(carmedrob_bin_baseline, to = carmedrob_bin_baseline),
    caggassault_bin_std_baseline = stdize(caggassault_bin_baseline, to = caggassault_bin_baseline),
    csimpleassault_bin_std_baseline = stdize(csimpleassault_bin_baseline, to = csimpleassault_bin_baseline),
    csexual_bin_std_baseline = stdize(csexual_bin_baseline, to = csexual_bin_baseline),
    cdomestic_phys_bin_std_baseline = stdize(cdomestic_phys_bin_baseline, to = cdomestic_phys_bin_baseline),
    cmurder_bin_std_baseline = stdize(cmurder_bin_baseline, to = cmurder_bin_baseline),
    cother_any_violent_std_baseline = stdize(cother_any_violent_baseline, to = cother_any_violent_baseline),
    cburglary_bin_std_baseline = stdize(cburglary_bin_baseline, to = cburglary_bin_baseline),
    cother_any_nonviolent_std_baseline = stdize(cother_any_nonviolent_baseline, to = cother_any_nonviolent_baseline),
    
    crime_victim_idx_bin = idx_mean(violentcrime_bin_std, nonviolentcrime_bin_std, cviolentcrime_bin_std, cnonviolentcrime_bin_std, tx = Z, fe = police_zones),
    crime_victim_idx_bin_baseline = idx_mean(violentcrime_bin_std_baseline, nonviolentcrime_bin_std_baseline, cviolentcrime_bin_std_baseline, cnonviolentcrime_bin_std_baseline, tx = Z, fe = police_zones),
    # restandardize
    crime_victim_idx_bin = stdize(crime_victim_idx_bin, to = crime_victim_idx_bin_baseline),
    crime_victim_idx_bin_baseline = stdize(crime_victim_idx_bin_baseline, to = crime_victim_idx_bin_baseline)
    
  )

# 3. List-wise deletion
lbr_citizen <-
  lbr_citizen %>%
  mutate(
    crime_victim_idx_bin_listwise = idx_mean_listwise(violentcrime_bin_std, nonviolentcrime_bin_std, cviolentcrime_bin_std, cnonviolentcrime_bin_std),
    crime_victim_idx_bin_listwise_baseline = idx_mean_listwise(violentcrime_bin_std_baseline, nonviolentcrime_bin_std_baseline, cviolentcrime_bin_std_baseline, cnonviolentcrime_bin_std_baseline),
    # restandardize
    crime_victim_idx_bin_listwise = stdize(crime_victim_idx_bin_listwise, to = crime_victim_idx_bin_listwise_baseline),
    crime_victim_idx_bin_listwise_baseline = stdize(crime_victim_idx_bin_listwise_baseline, to = crime_victim_idx_bin_listwise_baseline)
  )

#--------------------------------------------------------------------------------------------------------------
# 21. REMOVE ALL NON RESCALED VARIABLES
#--------------------------------------------------------------------------------------------------------------
rescaled_vars <-
  names(lbr_citizen)[str_ends(names(lbr_citizen), "_rescaled")] %>%
  str_replace("_rescaled", "")

lbr_citizen <- 
  lbr_citizen %>% 
  select(-rescaled_vars)


#--------------------------------------------------------------------------------------------------------------
# Save admin and citizen data
#--------------------------------------------------------------------------------------------------------------
saveRDS(lbr_citizen, file = "data/out/lbr-citizen-construct.RDS")
saveRDS(lbr_admin, file = "data/out/lbr-admin-construct.RDS")
saveRDS(lbr_citizen_baseline, "data/out/lbr-citizen-baseline-construct.RDS")

