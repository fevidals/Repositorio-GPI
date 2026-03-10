suppressMessages({
  library(tidyverse)
})

main_hypotheses <- rbind(
  c("1a", "crime_victim_idx_common", "Crime victimization idx."), 
  c("1b", "future_insecurity_idx_common", "Perceived future insecurity idx."), 
  c("2", "satis_idx", "Overall perceptions of police idx."), 
  c("3a", "officer_attitude_idx", "Police perceptions of citizens idx."),
  c("3b", "police_abuse_idx_common", "Police abuse idx."), 
  c("4a", "crime_reporting_idx_common", "Crime reporting idx."), 
  c("4b", "tips_idx", "Crime tips idx."), # admin data
  c("4c", "police_abuse_report_idx_common", "Police abuse reporting idx."), # admin data
  c("M1a", "intentions_idx", "Perceived police intentions idx."), 
  c("M1b", "know_idx_common", "Knowledge of criminal justice idx."),
  c("M1c", "norm_idx", "Cooperation norms idx."), 
  c("M2a", "police_capacity_idx", "Perceived police capacity idx."), 
  c("M2b", "responsive_act_std", "Perceived police responsiveness"), 
  c("S1", "legit_trust_std", "Perceived state legitimacy"), 
  c("S2", "trust_community_std", "Community trust"),
  c("C", "compliance_idx", "Compliance idx.")
)
colnames(main_hypotheses) <- c("hypothesis", "outcome", "label")
main_hypotheses <- as_tibble(main_hypotheses)

main_hypotheses_original <- rbind(
  c("1a", "crime_victim_idx", "Crime victimization idx."), 
  c("1b", "future_insecurity_idx", "Perceived future insecurity idx."), 
  c("2", "satis_idx", "Overall perceptions of police idx."), 
  c("3a", "officer_attitude_idx", "Police perceptions of citizens idx."),
  c("3b", "police_abuse_idx", "Police abuse idx."), 
  c("4a", "crime_reporting_idx", "Crime reporting idx."), 
  c("4b", "tips_idx", "Crime tips idx."), # admin data
  c("4c", "police_abuse_report_idx", "Police abuse reporting idx."), # admin data
  c("M1a", "intentions_idx", "Perceived police intentions idx."), 
  c("M1b", "know_idx", "Knowledge of criminal justice idx."),
  c("M1c", "norm_idx", "Cooperation norms idx."), 
  c("M2a", "police_capacity_idx", "Perceived police capacity idx."), 
  c("M2b", "responsive_act_std", "Perceived police responsiveness"), 
  c("S1", "legit_trust_std", "Perceived state legitimacy"), # repeating perceptions of
  c("S2", "trust_community_std", "Community trust"),
  c("C", "compliance_idx", "Compliance idx.")
)
colnames(main_hypotheses_original) <- c("hypothesis", "outcome", "label")
main_hypotheses_original <- as_tibble(main_hypotheses_original)

main_hypotheses_listwise <- rbind(
  c("1a", "crime_victim_idx_listwise", "Crime victimization idx."), 
  c("1b", "future_insecurity_idx_listwise", "Perceived future insecurity idx."), 
  c("2", "satis_idx_listwise", "Overall perceptions of police idx."), 
  c("3a", "officer_attitude_idx_listwise", "Police perceptions of citizens idx."),
  c("3b", "police_abuse_idx_listwise", "Police abuse idx."), 
  c("4a", "crime_reporting_idx_listwise", "Crime reporting idx."), 
  c("4b", "tips_idx_listwise", "Crime tips idx."), # admin data
  c("4c", "police_abuse_report_idx_listwise", "Police abuse reporting idx."), # admin data
  c("M1a", "intentions_idx_listwise", "Perceived police intentions idx."), 
  c("M1b", "know_idx_listwise", "Knowledge of criminal justice idx."),
  c("M1c", "norm_idx_listwise", "Cooperation norms idx."), 
  c("M2a", "police_capacity_idx_listwise", "Perceived police capacity idx."), 
  c("M2b", "responsive_act_std", "Perceived police responsiveness"), 
  c("S1", "legit_trust_std", "Perceived state legitimacy"), # repeating perceptions of
  c("S2", "trust_community_std", "Community trust"),
  c("C", "compliance_idx_listwise", "Compliance idx.")
)
colnames(main_hypotheses_listwise) <- c("hypothesis", "outcome", "label")
main_hypotheses_listwise <- as_tibble(main_hypotheses_listwise)

all_indices_listwise <- rbind(
  c("1a", "crime_victim_idx_listwise", "Crime victimization idx."), 
  c("1b", "future_insecurity_idx_listwise", "Perceived future insecurity idx."), 
  c("2", "satis_idx_listwise", "Overall perceptions of police idx."), 
  c("3a", "officer_attitude_idx_listwise", "Police perceptions of citizens idx."),
  c("3b", "police_abuse_idx_listwise", "Police abuse idx."), 
  c("4a", "crime_reporting_idx_listwise", "Crime reporting idx."), 
  c("4b", "tips_idx_listwise", "Crime tips idx."), # admin data
  c("4c", "police_abuse_report_idx_listwise", "Police abuse reporting idx."), # admin data
  c("M1a", "intentions_idx_listwise", "Perceived police intentions idx."), 
  c("M1b", "know_idx_listwise", "Knowledge of criminal justice idx."),
  c("M1c", "norm_idx_listwise", "Cooperation norms idx."), 
  c("M2a", "police_capacity_idx_listwise", "Perceived police capacity idx."), 
  c("M2b", "responsive_act_std", "Perceived police responsiveness"), 
  c("S1", "legit_trust_std", "Perceived state legitimacy"), # repeating perceptions of
  c("S2", "trust_community_std", "Community trust"),
  c("C", "compliance_idx_listwise", "Compliance idx."),
  
  c("3a", "empathy_idx","Empathy"),
  c("3a", "accountability_idx","Accountability"),
  c("3a", "abuse_idx", "Abuse"),
  c("3a", "corrupt_idx", "Corruption"),
  
  c("4a", "crimeres_idx", "Resolution of crime"), 
  c("M1a", "polint_idx", "Corruption"),
  
  c("M1b", "know_law_idx", "Law"),
  c("M1b", "know_report_idx", "Crime reporting")
  
)
colnames(all_indices_listwise) <- c("hypothesis", "outcome", "label")
all_indices_listwise <- as_tibble(all_indices_listwise)



secondary_hypotheses_listwise <- rbind(
  c("1a. (alt. i)", "crime_victim_idx_admin_listwise", "Crime victimization idx. (administrative data)"),
  c("1a. (alt. ii)", "crime_victim_idx_exp_listwise", "Crime victimization idx. (expanded crimes)"), 
  c("1a. (alt. iii)", "crime_victim_idx_bin_listwise", "Crime victimization idx. (binary survey measures)"))

colnames(secondary_hypotheses_listwise) <- c("hypothesis", "outcome", "label")
secondary_hypotheses_listwise <- as_tibble(secondary_hypotheses_listwise)


secondary_hypotheses <- rbind(
  c("1a. (alt. i)", "crime_victim_idx_admin", "Crime victimization idx. (administrative data)", "1"),
  c("1a. (alt. i)", "aviolentcrime_num_std", "Violent crimes (administrative data)", "2"),
  c("1a. (alt. i)", "aarmedrob_num_std", "Armed robbery (administrative data)", "3"),
  c("1a. (alt. i)", "aaggassault_num_std", "Aggravated assault (administrative data)", "4"),
  c("1a. (alt. i)", "asimpleassault_num_std", "Simple assault (administrative data)", "5"),
  c("1a. (alt. i)", "asexual_num_std", "Sexual assault (administrative data)", "6"),
  c("1a. (alt. i)", "adomestic_phys_num_std", "Domestic abuse (physical) (administrative data)", "7"),
  c("1a. (alt. i)", "amurder_num_std", "Murder (administrative data)", "8"),
  c("1a. (alt. i)", "aother_num_violent_std", "Other violent crimes (administrative data)", "9"),
  c("1a. (alt. i)", "anonviolentcrime_num_std", "Non-violent crimes (administrative data)", "10"),
  c("1a. (alt. i)", "aburglary_num_std", "Burglary (administrative data)", "11"),
  c("1a. (alt. i)", "aother_num_nonviolent_std", "Other non-violent crimes (administrative data)", "12"),
  c("1a. (alt. ii)", "crime_victim_idx_exp", "Crime victimization idx. (expanded crimes)", "13"),
  c("1a. (alt. ii)", "violentcrime_num_exp_std", "Violent crimes (expanded, personal)", "14"),
  c("1a. (alt. ii)", "armedrob_num_std", "Armed Robbery (expanded, personal)", "15"),
  c("1a. (alt. ii)", "aggassault_num_std", "Aggravated assault (expanded, personal)", "16"),
  c("1a. (alt. ii)", "sexual_num_std", "Sexual assault (expanded, personal)", "17"),
  c("1a. (alt. ii)", "domestic_phys_num_std", "Domestic abuse (physical) (expanded, personal)", "18"),
  c("1a. (alt. ii)", "simpleassault_num_std", "Simple assault (expanded, personal)", "19"),
  c("1a. (alt. ii)", "other_any_violent_std", "Other violent crimes (expanded, personal)", "20"),
  c("1a. (alt. ii)", "nonviolentcrime_num_exp_std", "Non-violent crimes  (expanded, personal)", "21"),
  c("1a. (alt. ii)", "burglary_num_std", "Burglary (expanded, personal)", "22"),
  c("1a. (alt. ii)", "domestic_verbal_num_std", "Domestic abuse (verbal) (expanded, personal)", "23"),
  c("1a. (alt. ii)", "land_any_std", "Land crimes (expanded, personal)", "24"),
  c("1a. (alt. ii)", "other_any_nonviolent_std", "Other non-violent crimes (expanded, personal)", "25"),
  c("1a. (alt. ii)", "cviolentcrime_num_exp_std", "Violent crimes (community, expanded)", "26"),
  c("1a. (alt. ii)", "carmedrob_num_std", "Armed robbery (community, expanded)", "27"),
  c("1a. (alt. ii)", "caggassault_num_std", "Aggravated assault (community, expanded)", "28"),
  c("1a. (alt. ii)", "csimpleassault_num_std", "Simple assault (community, expanded)", "29"),
  c("1a. (alt. ii)", "csexual_num_std", "Sexual assault  (community, expanded)", "30"),
  c("1a. (alt. ii)", "cdomestic_phys_num_std", "Domestic abuse (physical) (community, expanded)", "31"),
  c("1a. (alt. ii)", "cmurder_num_std", "Murder (community, expanded)", "32"),
  c("1a. (alt. ii)", "cmob_num_std", "Mob (community, expanded)", "33"),
  c("1a. (alt. ii)", "cother_any_violent_std", "Other violent crimes (community, expanded)", "34"),
  c("1a. (alt. ii)", "cnonviolentcrime_num_exp_std", "Non-violent crimes (community, expanded)", "35"),
  c("1a. (alt. ii)", "cburglary_num_std", "Burglary (community, expanded)", "36"),
  c("1a. (alt. ii)", "cland_any_std", "Land crimes (community, expanded)", "37"),
  c("1a. (alt. ii)", "cdomestic_verbal_num_std", "Domestic abuse (verbal)  (community, expanded)", "38"),
  c("1a. (alt. ii)", "cother_any_nonviolent_std", "Other non-violent crimes (community, expanded)", "39"),
  c("1a. (alt. iii)", "crime_victim_idx_bin", "Crime victimization idx. (binary survey measures)", "40"),
  c("1a. (alt. iii)", "violentcrime_bin_std", "Violent crime (personal, binary)", "41"),
  c("1a. (alt. iii)", "armedrob_bin_std", "Armed robbery (personal, binary)", "42"),
  c("1a. (alt. iii)", "simpleassault_bin_std", "Simple assault (personal, binary)", "43"),
  c("1a. (alt. iii)", "other_any_violent_std", "Other violent crimes (personal, binary)", "44"),
  c("1a. (alt. iii)", "nonviolentcrime_bin_std", "Non-violent crimes (personal, binary)", "45"),
  c("1a. (alt. iii)", "burglary_bin_std", "Burglary (personal, binary)", "46"),
  c("1a. (alt. iii)", "other_any_nonviolent_std", "Other non-violent crimes (personal, binary)", "47"),
  c("1a. (alt. iii)", "cviolentcrime_bin_std", "Violent crimes (community, binary)", "48"),
  c("1a. (alt. iii)", "carmedrob_bin_std", "Armed Robbery (community, binary)", "49"),
  c("1a. (alt. iii)", "caggassault_bin_std", "Aggravated assault (community, binary)", "50"),
  c("1a. (alt. iii)", "csimpleassault_bin_std", "Simple assault (community, binary)", "51"),
  c("1a. (alt. iii)", "csexual_bin_std", "Sexual assault (community, binary)", "52"),
  c("1a. (alt. iii)", "cdomestic_phys_bin_std", "Domestic abuse (physical) (community, binary)", "53"),
  c("1a. (alt. iii)", "cmurder_bin_std", "Murder (community, binary)", "54"),
  c("1a. (alt. iii)", "cother_any_violent_std", "Other violent crimes (community, binary)", "55"),
  c("1a. (alt. iii)", "cnonviolentcrime_bin_std", "Non-violent crimes (community, binary)", "56"),
  c("1a. (alt. iii)", "cburglary_bin_std", "Burglary (community, binary)", "57"),
  c("1a. (alt. iii)", "cother_any_nonviolent_std", "Other non-violent crimes (community, binary)", "58"),
  c("4a (alt.)", "crime_reporting_idx_admin", "Crime reporting idx. (admin data)", "59"))
colnames(secondary_hypotheses) <- c("hypothesis", "outcome", "label", "order")
secondary_hypotheses <- as_tibble(secondary_hypotheses) %>% mutate(order = as.numeric(order))


sub_hypotheses <- rbind(
  c("1a", "crime_victim_idx", "Non-violent crime (community)", "cnonviolentcrime_num_std"), 
  c("1a", "crime_victim_idx", "Non-violent crime (personal)", "nonviolentcrime_num_std"), 
  c("1a", "crime_victim_idx", "Violent crime (community)", "cviolentcrime_num_std"), 
  c("1a", "crime_victim_idx", "Violent crime (personal)", "violentcrime_num_std"), 
  
  c("3a", "officer_attitude_idx", "Empathy", "empathy_idx"),
  c("3a", "officer_attitude_idx", "Accountability", "accountability_idx"),
  c("3a", "officer_attitude_idx", "Abuse", "abuse_idx"),
  c("3a", "officer_attitude_idx", "Corruption", "corrupt_idx"),
  
  c("3b",   "police_abuse_idx", "Abuse (binary)", "policeabuse_any_std"), 
  c("3b",   "police_abuse_idx", "Abuse", "policeabuse_num_std"), 
  c("3b",   "police_abuse_idx", "Bribe amount", "bribe_amt_std"), 
  c("3b",   "police_abuse_idx", "Bribe frequency", "bribe_freq_std"), 
  
  c("4a",    "crime_reporting_idx", "Violent crime reporting (community)", "cviolentcrime_report_num_std"), 
  c("4a",    "crime_reporting_idx", "Violent crime reporting (personal)", "violentcrime_report_num_std"), 
  c("4a",    "crime_reporting_idx", "Non-violent crime (community)", "cnonviolentcrime_report_num_std"),
  c("4a",    "crime_reporting_idx", "Non-violent crime (personal)", "nonviolentcrime_report_num_std"), 
  c("4a",    "crime_reporting_idx", "Resolution of crime", "crimeres_idx"), 
  
  c("4c",   "police_abuse_report_idx", "Physical/verbal abuse", "policeabuse_report_std"), 
  c("4c",   "police_abuse_report_idx", "Beating community member", "policebeating_report_std"), 
  c("4c",   "police_abuse_report_idx", "Drinking on duty", "dutydrink_report_std"),
  
  c("M1a", "intentions_idx", "Treat seriously", "polcaseserious_std"),
  c("M1a", "intentions_idx", "Treat fairly", "polcasefair_std"),
  c("M1a", "intentions_idx", "Corruption", "polint_idx"),
  
  c("M1b", "know_idx", "Law", "know_law_idx"),
  c("M1b", "know_idx", "Crime reporting", "know_report_idx"),
  
  c("M2a", "police_capacity_idx", "Investigation capacity", "polcap_investigate_std"),
  c("M2a", "police_capacity_idx", "Timeliness", "polcap_timely_std"),
  
  c("C", "compliance_idx", "Foot patrol frequency", "compliance_patrol_std"),
  c("C", "compliance_idx", "Vehicle patrol frequency", "compliance_freq_std"),
  c("C", "compliance_idx", "Community meeting awareness", "compliance_meeting_std")
  
)
colnames(sub_hypotheses) <- c("hypothesis", "idx", "label_component", "outcome")
sub_hypotheses <- as_tibble(sub_hypotheses)

all_components <- rbind(  
  c("1a", "crime_victim_idx_common", "Crime victimization idx."), 
  c("1a", "violentcrime_num_std", "Violent crimes (personal)"),
  c("1a", "armedrob_num_std", "Armed robbery (personal)"),
  c("1a", "simpleassault_num_std", "Simple assault (personal)"),
  c("1a", "other_any_violent_std", "Other violent crimes (personal)"),
  
  c("1a", "nonviolentcrime_num_std", "Non-violent crimes (personal)"),
  c("1a", "burglary_num_std", "Burglary (personal)"),
  c("1a", "other_any_nonviolent_std", "Other non-violent crimes (personal)"),
  
  c("1a", "cviolentcrime_num_std", "Violent crimes (community)"),
  c("1a", "carmedrob_num_std", "Armed robbery (community)"),
  c("1a", "caggassault_num_std", "Aggravated assault (community)"),
  c("1a", "csimpleassault_num_std", "Simple assault (community)"),
  c("1a", "csexual_num_std", "Sexual assault (community)"),
  c("1a", "cdomestic_phys_num_std", "Domestic abuse (community)"),
  c("1a", "cmurder_num_std", "Murder (community)"),
  c("1a", "cother_any_violent_std", "Other violent crimes (community)"),
  
  c("1a", "cnonviolentcrime_num_std", "Non-violent crimes (community)"),
  c("1a", "cburglary_num_std", "Burglary (community)"),
  c("1a", "cother_any_nonviolent_std", "Other non-violent crimes (community)"),
  
  c("1b", "future_insecurity_idx_common", "Perceived future insecurity idx."), 
  c("1b", "fear_violent_std", "Feared violent crime"),
  c("1b", "fear_nonviolent_std", "Fear non-violent crime"),
  c("1b", "feared_walk_std", "Feared walking"),
  
  c("2", "satis_idx", "Overall perceptions of police idx."), 
  c("2", "satis_trust_std", "Trust in police"),
  c("2", "satis_general_std", "Trust in service of police"),
  
  c("3a", "officer_attitude_idx", "Police perceptions of citizens idx."),
  c("3a", "empathy_idx", "Emapthy idx."),
  c("3a", "empathy_complaints_std", "Empathy (complaints)"),
  c("3a", "empathy_reports_std", "Empathy (reports)"),
  c("3a", "accountability_idx", "Police accountability idx."),
  c("3a", "account_pol_matter_std", "Police takes complaints seriously"),
  c("3a", "hypothetical2_punishment_std", "Hypothetical 2: discipliniary punishment"),
  c("3a", "hypothetical2_reportself_std", "Hypothetical 2: report fellow officer"),
  c("3a", "hypothetical2_reportothers_std", "Hypothetical 2: reports by other officers"),
  c("3a", "hypothetical3_punishment_std", "Hypothetical 3: discipliniary punishment"),
  c("3a", "hypothetical3_reportself_std", "Hypothetical 3: report fellow officer"),
  c("3a", "hypothetical3_reportothers_std", "Hypothetical 3: reports by other officers"),
  c("3a", "hypothetical5_punishment_std", "Hypothetical 5: discipliniary punishment"),
  c("3a", "hypothetical5_reportself_std", "Hypothetical 5: report fellow officer"),
  c("3a", "hypothetical5_reportothers_std", "Hypothetical 5: reports by other officers"),
  c("3a", "abuse_idx", "Police abuse idx."),
  c("3a", "hypothetical5_abuseself_std", "Hypothetical 5: own misconduct"),
  c("3a", "hypothetical5_abuseother_std", "Hypothetical 5: others' misconduct"),
  c("3a", "corrupt_idx", "Police corruption idx."),
  c("3a", "hypothetical2_corruptself_std", "Hypothetical 2: own misconduct (corruption)"),
  c("3a", "hypothetical2_corruptother_std", "Hypothetical 2: others' misconduct (corruption)"),
  c("3a", "hypothetical3_corruptself_std", "Hypothetical 3: own misconduct (corruption)"),
  c("3a", "hypothetical3_corruptother_std", "Hypothetical 3: others' misconduct (corruption)"),
  
  c("3b", "police_abuse_idx_common", "Police abuse idx."), 
  c("3b", "policeabuse_any_std", "Police abuse"),
  c("3b", "policeabuse_num_std", "Police abuse"),
  c("3b", "bribe_freq_std", "Bribe frequency"),
  c("3b", "bribe_amt_std", "Bribe amount"),
  
  c("4a", "crime_reporting_idx_common", "Crime reporting idx."), 
  c("4a", "violentcrime_report_num_std", "Violent crimes reported (personal)"),
  c("4a", "armedrob_report_std", "Armed robbery reported (personal)"),
  c("4a", "simpleassault_report_std", "Simple assault reported (personal)"),
  c("4a", "other_report_violent_std", "Other violent crimes reported (personal)"),
  
  c("4a", "nonviolentcrime_report_num_std", "Non-violent crimes reported (personal)"),
  c("4a", "burglary_report_std", "Burglary reported (personal)"),
  c("4a", "other_report_nonviolent_std", "Other non-violent crimes reported (personal)"),
  
  c("4a", "cviolentcrime_report_num_std", "Violent crimes reported (community)"),
  c("4a", "carmedrob_report_std", "Armed robbery reported (community)"),
  c("4a", "caggassault_report_std", "Aggravated assault reported (community)"),
  c("4a", "csimpleassault_report_std", "Simple assault reported (community)"),
  c("4a", "csexual_report_std", "Sexual assault reported (community)"),
  c("4a", "cdomestic_phys_report_std", "Domestic physical abuse reported (community)"),
  c("4a", "cmurder_report_std", "Murder reported (community)"),
  c("4a", "cother_report_violent_std", "Other violent crime reported (community)"),
  
  c("4a", "cnonviolentcrime_report_num_std", "Non-violent crime reported (community)"),
  c("4a", "cburglary_report_std", "Burglary reported (community)"),
  c("4a", "cother_report_nonviolent_std", "Other non-violent crime reported (community)"),
  
  c("4a", "crimeres_idx", "Resolution of crime index"),
  c("4a", "burglaryres_std", "Burglary resolution"),
  c("4a", "dviolres_std", "Domestic abuse resolution"),
  c("4a", "armedrobres_std", "Armed robbery resolution"),
  
  c("4b", "tips_idx", "Crime tips idx."),
  c("4b", "crime_tips_idx", "Crime tips idx."),
  c("4b", "contact_pol_susp_activity_std", "Contacted police for suspicious activity"),
  c("4b", "give_info_pol_investigation_std", "Gave information to police"),
  
  c("4c", "police_abuse_report_idx_common", "Police abuse reporting idx."),
  c("4c", "dutydrink_report_std", "Reported drinking on duty"),
  c("4c", "policebeating_report_std", "Reported police beating"),
  c("4c", "policeabuse_report_std", "Reported police abuse"),
  c("4c", "apolvtm_hline_std", "Victimization reported to police by hotline"),
  c("4c", "apolvtm_cmtbox_std", "Victimization reported to police by comment box"),
  c("4c", "apolvtm_station_std", "Victimization reported to police station"),
  
  c("M1a", "intentions_idx", "Perceived police intentions idx."), 
  c("M1a", "polcaseserious_std", "Police will investigate"),
  c("M1a", "polcasefair_std", "Police will be fair"),
  c("M1a", "polint_idx", "Political interest idx."),
  c("M1a", "polint_corrupt_std", "Police are corrupt"),
  c("M1a", "polint_quality_std", "Police serve equally"),
  
  c("M1b", "know_idx_common", "Knowledge of criminal justice idx."),
  c("M1b", "know_law_idx", "Legal knowledge idx."),
  c("M1b", "know_law_suspect_std", "Legal Knowledge (suspect)"),
  c("M1b", "know_law_lawyer_std", "Legal Knowledge (lawyer)"), 
  c("M1b", "know_law_fees_std", "Legal Knowledge (fees)"), 
  c("M1b", "know_law_vaw_std", "Legal Knowledge (domestic abuse)"),
  
  c("M1b", "know_report_idx", "Reporting knowledge idx."),
  c("M1b", "know_report_followup_std", "Police Knowledge (followup)"),
  c("M1b", "know_report_station_std", "Police Knowledge (where is station)"),
  
  c("M1c", "norm_idx", "Cooperation norms idx."),
  c("M1c", "reportnorm_theft_std", "Reporting norm (theft)"),
  c("M1c", "reportnorm_abuse_std", "Reporting norm (domestic abuse)"),
  c("M1c", "obeynorm_std", "Obey police norm"),
  
  c("M2a", "police_capacity_idx", "Perceived police capacity idx."),
  c("M2a", "polcap_timely_std", "Police timeliness"),
  c("M2a", "polcap_investigate_std", "Police investigation capacity"),
  
  c("M2b", "responsive_act_std", "Perceived police responsiveness"), 
  c("S1", "legit_trust_std", "Perceived state legitimacy"), 
  c("S2", "trust_community_std", "Community trust"),
  
  c("C", "compliance_idx", "Compliance idx."),
  c("C", "compliance_patrol_std", "Foot patrol frequency"),
  c("C", "compliance_freq_std", "Vehicle patrol frequency"),
  c("C", "compliance_meeting_std", "Community meeting awareness")
  )


all_components_original <- rbind(  
  c("1a", "crime_victim_idx", "Crime victimization idx."), 
  c("1a", "violentcrime_num", "Violent crimes (personal)"),
  c("1a", "armedrob_num", "Armed robbery (personal)"),
  c("1a", "simpleassault_num", "Simple assault (personal)"),
  c("1a", "other_any_violent", "Other violent crimes (personal)"),
  
  c("1a", "nonviolentcrime_num", "Non-violent crimes (personal)"),
  c("1a", "burglary_num", "Burglary (personal)"),
  c("1a", "other_any_nonviolent", "Other non-violent crimes (personal)"),
  
  c("1a", "cviolentcrime_num", "Violent crimes (community)"),
  c("1a", "carmedrob_num", "Armed robbery (community)"),
  c("1a", "caggassault_num", "Aggravated assault (community)"),
  c("1a", "csimpleassault_num", "Simple assault (community)"),
  c("1a", "csexual_num", "Sexual assault (community)"),
  c("1a", "cdomestic_phys_num", "Domestic abuse (community)"),
  c("1a", "cmurder_num", "Murder (community)"),
  c("1a", "cother_any_violent", "Other violent crimes (community)"),
  
  c("1a", "cnonviolentcrime_num", "Non-violent crimes (community)"),
  c("1a", "cburglary_num", "Burglary (community)"),
  c("1a", "cother_any_nonviolent", "Other non-violent crimes (community)"),
  
  c("1b", "future_insecurity_idx", "Perceived future insecurity idx."), 
  c("1b", "fear_violent", "Feared violent crime"),
  c("1b", "fear_nonviolent", "Fear non-violent crime"),
  c("1b", "feared_walk", "Feared walking"),
  
  c("2", "satis_idx", "Overall perceptions of police idx."), 
  c("2", "satis_trust", "Trust in police"),
  c("2", "satis_general", "Trust in service of police"),
  
  c("3a", "officer_attitude_idx", "Police perceptions of citizens idx."),
  c("3a", "empathy_idx", "Emapthy idx."),
  c("3a", "empathy_complaints", "Empathy (complaints)"),
  c("3a", "empathy_reports", "Empathy (reports)"),
  c("3a", "accountability_idx", "Police accountability idx."),
  c("3a", "account_pol_matter", "Police takes complaints seriously"),
  c("3a", "hypothetical2_punishmen", ""),
  c("3a", "hypothetical2_reportself", ""),
  c("3a", "hypothetical2_reportothers", ""),
  c("3a", "hypothetical3_punishment", ""),
  c("3a", "hypothetical3_reportself", ""),
  c("3a", "hypothetical3_reportothers", ""),
  c("3a", "hypothetical5_punishment", ""),
  c("3a", "hypothetical5_reportself", ""),
  c("3a", "hypothetical5_reportothers", ""),
  c("3a", "abuse_idx", "Police abuse idx."),
  c("3a", "hypothetical5_abuseself", ""),
  c("3a", "hypothetical5_abuseother", ""),
  c("3a", "corrupt_idx", "Police corruption idx."),
  c("3a", "hypothetical2_corruptself", ""),
  c("3a", "hypothetical2_corruptother", ""),
  c("3a", "hypothetical3_corruptself", ""),
  c("3a", "hypothetical3_corruptother", ""),
  
  c("3b", "police_abuse_idx", "Police abuse idx."), 
  c("3b", "policeabuse_any", "Police abuse"),
  c("3b", "policeabuse_num", "Police abuse"),
  c("3b", "bribe_freq", "Bribe frequency"),
  c("3b", "bribe_amt", "Bribe amount"),
  
  c("4a", "crime_reporting_idx", "Crime reporting idx."), 
  c("4a", "violentcrime_report_num", "Violent crimes reported (personal)"),
  c("4a", "armedrob_report", "Armed robbery reported (personal)"),
  c("4a", "simpleassault_report", "Simple assault reported (personal)"),
  c("4a", "other_report_violent", "Other violent crimes reported (personal)"),
  
  c("4a", "nonviolentcrime_report_num", "Non-violent crimes reported (personal)"),
  c("4a", "burglary_report", "Burglary reported (personal)"),
  c("4a", "other_report_nonviolent", "Other non-violent crimes reported (personal)"),
  
  c("4a", "cviolentcrime_report_num", "Violent crimes reported (community)"),
  c("4a", "carmedrob_report", "Armed robbery reported (community)"),
  c("4a", "caggassault_report", "Aggravated assault reported (community)"),
  c("4a", "csimpleassault_report", "Simple assault reported (community)"),
  c("4a", "csexual_report", "Sexual assault reported (community)"),
  c("4a", "cdomestic_phys_report", "Domestic physical abuse reported (community)"),
  c("4a", "cmurder_report", "Murder reported (community)"),
  c("4a", "cother_report_violent", "Other violent crime reported (community)"),
  
  c("4a", "cnonviolentcrime_report_num", "Non-violent crime reported (community)"),
  c("4a", "cburglary_report", "Burglary reported (community)"),
  c("4a", "cother_report_nonviolent", "Other non-violent crime reported (community)"),
  
  c("4a", "crimeres_idx", "Resolution of crime index"),
  c("4a", "burglaryres", "Burglary resolution"),
  c("4a", "dviolres", "Domestic abuse resolution"),
  c("4a", "armedrobres", "Armed robbery resolution"),
  
  c("4b", "tips_idx", "Crime tips idx."),
  c("4b", "crime_tips_idx", "Crime tips idx."),
  c("4b", "contact_pol_susp_activity", "Contacted police for suspicious activity"),
  c("4b", "give_info_pol_investigation", "Gave information to police"),
  
  c("4c", "police_abuse_report_idx", "Police abuse reporting idx."),
  c("4c", "dutydrink_report", "Reported drinking on duty"),
  c("4c", "policebeating_report", "Reported police beating"),
  c("4c", "policeabuse_report", "Reported police abuse"),
  c("4c", "apolvtm_hline", "Victimization reported to police by hotline"),
  c("4c", "apolvtm_cmtbox", "Victimization reported to police by comment box"),
  c("4c", "apolvtm_station", "Victimization reported to police station"),
  
  c("M1a", "intentions_idx", "Perceived police intentions idx."), 
  c("M1a", "polcaseserious", "Police will investigate"),
  c("M1a", "polcasefair", "Police will be fair"),
  c("M1a", "polint_idx", "Political interest idx."),
  c("M1a", "polint_corrupt", "Police are corrupt"),
  c("M1a", "polint_quality", "Police serve equally"),
  
  c("M1b", "know_idx", "Knowledge of criminal justice idx."),
  c("M1b", "know_law_idx", "Legal knowledge idx."),
  c("M1b", "know_law_suspect", "Legal Knowledge (suspect)"),
  c("M1b", "know_law_lawyer", "Legal Knowledge (lawyer)"), 
  c("M1b", "know_law_fees", "Legal Knowledge (fees)"), 
  c("M1b", "know_law_vaw", "Legal Knowledge (domestic abuse)"),
  
  c("M1b", "know_report_idx", "Reporting knowledge idx."),
  c("M1b", "know_report_followup", "Police Knowledge (followup)"),
  c("M1b", "know_report_station", "Police Knowledge (where is station)"),
  
  c("M1c", "norm_idx", "Cooperation norms idx."),
  c("M1c", "reportnorm_theft", "Reporting norm (theft)"),
  c("M1c", "reportnorm_abuse", "Reporting norm (domestic abuse)"),
  c("M1c", "obeynorm", "Obey police norm"),
  
  c("M2a", "police_capacity_idx", "Perceived police capacity idx."),
  c("M2a", "polcap_timely", "Police timeliness"),
  c("M2a", "polcap_investigate", "Police investigation capacity"),
  
  c("M2b", "responsive_act", "Perceived police responsiveness"), 
  c("S1", "legit_trust", "Perceived state legitimacy"), 
  c("S2", "trust_community", "Community trust"),
  
  c("C", "compliance_idx", "Compliance idx."),
  c("C", "compliance_patrol", "Foot patrol frequency"),
  c("C", "compliance_freq", "Vehicle patrol frequency"),
  c("C", "compliance_meeting", "Community meeting awareness")
  )
  
index_components <- rbind(
  c("1a", "violentcrime_num_std", "Violent crimes (personal)"),
  c("1a", "nonviolentcrime_num_std", "Non-violent crimes (personal)"),
  c("1a", "cviolentcrime_num_std", "Violent crimes (community)"),
  c("1a", "cother_any_violent_std", "Other violent crimes (community)"),
  c("1a", "cnonviolentcrime_num_std", "Non-violent crimes (community)"),

  c("1b", "fear_violent_std", "Feared violent crime"),
  c("1b", "fear_nonviolent_std", "Feared non-violent crime"),
  c("1b", "feared_walk_std", "Feared walking"),
  
  c("2", "satis_trust_std", "Trust in police"),
  c("2", "satis_general_std", "Trust in service of police"),
  
  c("3a", "empathy_idx", "Emapthy idx."),
  c("3a", "accountability_idx", "Police accountability idx."),
  c("3a", "abuse_idx", "Police abuse idx."),
  c("3a", "corrupt_idx", "Police corruption idx."),

  c("3b", "policeabuse_any_std", "Police abuse"),
  c("3b", "policeabuse_num_std", "Police abuse"),
  c("3b", "bribe_freq_std", "Bribe frequency"),
  c("3b", "bribe_amt_std", "Bribe amount"),
  
  c("4a", "violentcrime_report_num_std", "Violent crimes reported (personal)"),
  c("4a", "nonviolentcrime_report_num_std", "Non-violent crimes reported (personal)"),
  c("4a", "cviolentcrime_report_num_std", "Violent crimes reported (community)"),
  c("4a", "cnonviolentcrime_report_num_std", "Non-violent crime reported (community)"),
  c("4a", "crimeres_idx", "Resolution of crime index"),

  c("4b", "contact_pol_susp_activity_std", "Contacted police for suspicious activity"),
  c("4b", "give_info_pol_investigation_std", "Gave information to police"),
  
  c("4c", "dutydrink_report_std", "Reported drinking on duty"),
  c("4c", "policebeating_report_std", "Reported police beating"),
  c("4c", "policeabuse_report_std", "Reported police abuse"),
  c("4c", "apolvtm_hline_std", "Victimization reported to police by hotline"),
  c("4c", "apolvtm_cmtbox_std", "Victimization reported to police by comment box"),
  c("4c", "apolvtm_station_std", "Victimization reported to police station"),
  
  c("M1a", "polcaseserious_std", "Police will investigate"),
  c("M1a", "polcasefair_std", "Police will be fair"),
  c("M1a", "polint_idx", "Political interest idx."),

  c("M1b", "know_law_idx", "Legal knowledge idx."),
  c("M1b", "know_report_idx", "Reporting knowledge idx."),
  
  c("M1c", "reportnorm_theft_std", "Reporting norm (theft)"),
  c("M1c", "reportnorm_abuse_std", "Reporting norm (domestic abuse)"),
  c("M1c", "obeynorm_std", "Obey police norm"),
  
  c("M2a", "polcap_timely_std", "Police timeliness"),
  c("M2a", "polcap_investigate_std", "Police investigation capacity"),
  
  c("M2b", "responsive_act_std", "Perceived police responsiveness"), 
  
  c("S1", "legit_trust_std", "Perceived state legitimacy"), 
  c("S2", "trust_community_std", "Community trust"),
  
  c("C", "compliance_patrol_std", "Foot patrol frequency"),
  c("C", "compliance_freq_std", "Vehicle patrol frequency"),
  c("C", "compliance_meeting_std", "Community meeting awareness")
)

colnames(all_components) <-
  c("hypothesis", "outcome", "label")

all_components <- as_tibble(all_components) %>% 
  mutate(order = 1:n())

colnames(all_components_original) <-
  c("hypothesis", "outcome", "label")

all_components_original <- 
  as_tibble(all_components_original) %>% 
  mutate(order = 1:n())

colnames(index_components) <-
  c("hypothesis", "outcome", "label")

index_components <-
  as_tibble(index_components) %>%
  mutate(order = 1:n())



