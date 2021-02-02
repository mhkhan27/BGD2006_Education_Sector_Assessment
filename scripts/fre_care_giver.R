rm(list = ls())

library(dplyr)
library(illuminate)
library(AMR)
library(tidyr)
library(stringr)
library(openxlsx)

hh <- read.csv("outputs/cleaned_data/caregiver/hh.csv") %>% filter(informed_consent == "yes")
indv <- read.csv("outputs/cleaned_data/caregiver/indv.csv") %>% mutate(
  age_grp_crvr_upazila= paste0(age_group,"_",group_of_caregivers,"_",upazila))


# hh_frequecncy -----------------------------------------------------------


hh_cols_not_to_anal<- c("X_index" ,"X_uuid","X_id", "X_uuid", "X_submission_time", "X_validation_status", 
                        "end_note","survey_date", "survey_start", "deviceid", "end_survey", "instance_name", 
                        "audit",
                        "aged_3_10_safety_concerns_by_type_for_girls",
                        "aged_11_14_safety_concerns_by_type_for_girls",
                        "aged_15_18_safety_concerns_by_type_for_girls",
                        "aged_19_24_safety_concerns_by_type_for_girls",
                        "aged_3_10_safety_concerns_by_type_for_boys",
                        "aged_11_14_safety_concerns_by_type_for_boys",
                        "aged_15_18_safety_concerns_by_type_for_boys",
                        "aged_19_24_safety_concerns_by_type_for_boys",
                        "engage_in_different_activities",
                        "access_to_learning_modalities",
                        "learning_acitivities_by_location",
                        "motivated_to_study_by_method",
                        "access_to_learning_materials",
                        "youths_accessing_activities",
                        "remote_learning_activities",
                        "teachers_contact_modality",
                        "most_important_improvements",
"survey_date", "enumerator_id", "kii_id", "informed_consent")

hh_cols_to_ana <- hh %>% select(-ends_with("other"),-hh_cols_not_to_anal,-starts_with("other_")) %>% names


overall_frequecy_hh <- illuminate::survey_frequency_by_choices(df = hh,variables_to_analyze = hh_cols_to_ana)
freq_by_upazila_hh <- illuminate::survey_frequency_by_choices(df = hh,variables_to_analyze = hh_cols_to_ana,aggregation_level = "upazila")
freq_by_care_giver_grp_hh <- illuminate::survey_frequency_by_choices(df = hh,variables_to_analyze = hh_cols_to_ana,
                                                    aggregation_level = "group_of_caregivers")
freq_by_upazila_care_giver_grp_hh <- illuminate::survey_frequency_by_choices(df = hh,variables_to_analyze = hh_cols_to_ana,
                                                                  aggregation_level = c("upazila","group_of_caregivers"))







# indv_frequency ----------------------------------------------------------


indv_cols_not_to_anal<- c("X_uuid","host_attending_education","attend_education_camp",
                          "not_attending_formal_education","not_attending_education",
                          "challenges_to_accessing_education_host",
                          "challenges_to_accessing_education_camp")

indv_cols_to_ana <- indv %>% select(-ends_with("other"),-indv_cols_not_to_anal,-starts_with("other_")) %>% names



overall_frequecy_indv <- illuminate::survey_frequency_by_choices(df = indv,variables_to_analyze = indv_cols_to_ana)
freq_by_ind_gender_indv <- illuminate::survey_frequency_by_choices(df = indv,variables_to_analyze = indv_cols_to_ana,aggregation_level = "ind_gender")
freq_by_age_group_indv <- illuminate::survey_frequency_by_choices(df = indv,variables_to_analyze = indv_cols_to_ana,aggregation_level = "age_group")
freq_by_gender_age_group_indv <- illuminate::survey_frequency_by_choices(df = indv,variables_to_analyze = indv_cols_to_ana,aggregation_level = "gender_age_group")
freq_by_age_group_grp_of_caregiver_indv <- illuminate::survey_frequency_by_choices(df = indv,variables_to_analyze = indv_cols_to_ana,
                                                                         aggregation_level = c("group_of_caregivers","age_group"))
freq_by_gender_age_of_caregiver_indv <- illuminate::survey_frequency_by_choices(df = indv,variables_to_analyze = indv_cols_to_ana,
                                                                                   aggregation_level = c("group_of_caregivers","gender_age_group"))

freq_by_age_grp_crvr_upazila_indv <- illuminate::survey_frequency_by_choices(df = indv,variables_to_analyze = indv_cols_to_ana,
                                                                                aggregation_level = "age_grp_crvr_upazila")

freq_by_gender_age_grp_crvr_upazila_indv <- illuminate::survey_frequency_by_choices(df = indv,variables_to_analyze = indv_cols_to_ana,
                                                        aggregation_level = c("ind_gender","age_grp_crvr_upazila"))

freq_by_host_attending_education_indv <- illuminate::survey_frequency_by_choices(df = indv,variables_to_analyze = indv_cols_to_ana,
                                                                                    aggregation_level = "host_attending_education")


freq_by_level_of_education_host_indv <- illuminate::survey_frequency_by_choices(df = indv,variables_to_analyze = indv_cols_to_ana,
                                                                                 aggregation_level = "level_of_education_host")

freq_by_level_of_education_and_host_attending_edu_indv <- illuminate::survey_frequency_by_choices(df = indv,variables_to_analyze = indv_cols_to_ana,
                                                                aggregation_level = c("level_of_education_host","host_attending_education"))


freq_by_attend_education_camp_indv <- illuminate::survey_frequency_by_choices(df = indv,variables_to_analyze = indv_cols_to_ana,
                                                                                aggregation_level = "attend_education_camp")

freq_by_level_of_education_camp_indv <- illuminate::survey_frequency_by_choices(df = indv,variables_to_analyze = indv_cols_to_ana,
                                                                              aggregation_level = "level_of_education_camp")

freq_by_level_of_education_cmp_and_attend_education_camp_indv <- illuminate::survey_frequency_by_choices(df = indv,variables_to_analyze = indv_cols_to_ana,
                                                                                                  aggregation_level = c("level_of_education_camp","attend_education_camp"))



freq_by_host_attend_edu_upazila_indv <- illuminate::survey_frequency_by_choices(df = indv,variables_to_analyze = indv_cols_to_ana,
                                                                                                         aggregation_level = c("upazila","host_attending_education"))



freq_by_attend_edu_camp_upazila_indv <- illuminate::survey_frequency_by_choices(df = indv,variables_to_analyze = indv_cols_to_ana,
                                                                                                         aggregation_level = c("upazila","attend_education_camp"))


# wwrite ------------------------------------------------------------------

write_list <- list(
  "overall_freq_hh"=overall_frequecy_hh,
  "freq_by_upazila_hh"=freq_by_upazila_hh,
  "freq_by_caregiver_grp_hh"=freq_by_care_giver_grp_hh,
  "freq_by_upazila_caregiver_grp_hh"=freq_by_upazila_care_giver_grp_hh,
  "overall_frequecy_indv"=overall_frequecy_indv,
  "freq_by_ind_gndr_indv"=freq_by_ind_gender_indv,
  "freq_by_age_grp_indv"=freq_by_age_group_indv,
  "freq_by_gender_age_grp_indv"=freq_by_gender_age_group_indv,
  "freq_by_age_group_grp_of_crgvr_indv"=freq_by_age_group_grp_of_caregiver_indv,
  "freq_by_gndr_age_of_crgvr_indv"=freq_by_gender_age_of_caregiver_indv,
  "freq_by_age_grp_crvr_upazila_indv"=freq_by_age_grp_crvr_upazila_indv,
  "freq_by_gndr_age_grp_crvr_upzla_indv"=freq_by_gender_age_grp_crvr_upazila_indv,
  "freq_by_host_attending_education_indv"	=	freq_by_host_attending_education_indv,
  "freq_by_lvl_of_education_host_indv"	=	freq_by_level_of_education_host_indv,
  "freq_by_lvl_of_edu_and_host_atndng_edu_indv" =		freq_by_level_of_education_and_host_attending_edu_indv,
  "freq_by_attend_education_camp_indv"	=	freq_by_attend_education_camp_indv,
  "freq_by_lvl_of_education_camp_indv"	=	freq_by_level_of_education_camp_indv,
  "freq_by_lvl_of_edu_cmp_and_atnd_edu_camp_indv"	=	freq_by_level_of_education_cmp_and_attend_education_camp_indv,
  "freq_by_host_atnd_edu_upzla_indv"	=	freq_by_host_attend_edu_upazila_indv,
  "freq_by_atnd_edu_cmp_upzla_indv"	=	freq_by_attend_edu_camp_upazila_indv
  )

write.xlsx(write_list,paste0("outputs/frequecny/caregiver/",str_replace_all(Sys.Date(),"-","_"),"_caregiver_frequency.xlsx"))
