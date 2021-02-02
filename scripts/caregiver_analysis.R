rm(list = ls())

library(dplyr)
library(butteR)
library(illuminate)
library(AMR)
library(tidyr)
library(stringr)
library(openxlsx)
library(srvyr)

hh <- read.csv("outputs/cleaned_data/caregiver/hh.csv",na.strings = c(""," ","NA","N/A"),stringsAsFactors = F) %>% filter(informed_consent == "yes")
indv <- read.csv("outputs/cleaned_data/caregiver/indv.csv",na.strings = c(""," ","NA","N/A"),stringsAsFactors = F) 
pop <- read.csv("inputs/pop_size.csv")

hh <- hh %>% mutate(
  strata = paste0(group_of_caregivers,"_",upazila)
)


# weights -----------------------------------------------------------------

sample_frame <- hh %>% group_by(strata) %>% dplyr::summarise(
  sample_size = n()
) %>% left_join(pop)


sf_with_weights_host <- sample_frame %>% dplyr::filter(pop== "host") %>% dplyr::mutate(
  sample_global = sum(sample_size),
  pop_global = sum(hh_size),
  survey_weight = (hh_size/pop_global)/(sample_size/sample_global)
)

sf_with_weights_camp <- sample_frame %>% dplyr::filter(pop== "camp") %>% dplyr::mutate(
  sample_global = sum(sample_size),
  pop_global = sum(hh_size),
  survey_weight = (hh_size/pop_global)/(sample_size/sample_global)
)



sf_with_weights <- bind_rows(sf_with_weights_host,sf_with_weights_camp) 

write.csv(sf_with_weights,"outputs/cleaned_data/caregiver/weights")

sf_with_weights <- sf_with_weights%>% select(strata,survey_weight)



# data_with_weights -------------------------------------------------------

hh_with_weights <- hh %>% left_join(sf_with_weights)
indv_with_weights <- indv %>% left_join(hh_with_weights %>% select(X_uuid,strata,survey_weight))


# survey_design -----------------------------------------------------------

hh_svy <- as_survey(hh_with_weights,strata = strata,weights = survey_weight)
indv_svy <- as_survey(indv_with_weights,strata=strata,weights = survey_weight)


# cols_to_analyze ---------------------------------------------------------


hh_cols_not_to_analyz <- c("survey_date", "survey_start", "deviceid", "end_survey", "instance_name","end_note", "end",
                           "audit", "enumerator_id", "kii_id", "int_note", "informed_consent", "start","gps_reading",
                           "X_gps_reading_longitude", "X_gps_reading_altitude", "X_gps_reading_precision", "X_gps_reading_latitude",
                           "X_id", "X_uuid", "X_submission_time", "X_validation_status","X_index", "strata", "survey_weight")


hh_cols_to_analys <- hh_with_weights %>% select( -ends_with("_other"),-starts_with("other_"),-hh_cols_not_to_analyz) %>% names() 


indv_cols_not_to_analyz <- c("strata", "survey_weight","X_index", "X_parent_table_name", "X_parent_index", 
                             "X_submission__id",  "X_uuid", "X_submission__submission_time",
                             "X_submission__validation_status")


indv_cols_to_ana <- indv_with_weights %>% select( -ends_with("_other"),-starts_with("other_"),-indv_cols_not_to_analyz) %>% names() 



# analysis ----------------------------------------------------------------

###hh#########
overall_analysis_hh <- butteR::mean_prop_working(design = hh_svy,list_of_variables = hh_cols_to_analys)
analysis_by_upazila_hh <- butteR::mean_prop_working(design = hh_svy,list_of_variables = hh_cols_to_analys,aggregation_level = "upazila")
analysis_by_grp_of_crvr_hh <- butteR::mean_prop_working(design = hh_svy,list_of_variables = hh_cols_to_analys,aggregation_level = "group_of_caregivers")
analysis_by_upazila_and_grp_of_crvr_hh <- butteR::mean_prop_working(design = hh_svy,list_of_variables = hh_cols_to_analys,
                                                                    aggregation_level = c("upazila","group_of_caregivers"))



### indv ###
overall_analysis_indv <- butteR::mean_prop_working(design =   indv_svy,list_of_variables = indv_cols_to_ana)
analysis_by_ind_gender_indv <- butteR::mean_prop_working(design = indv_svy,list_of_variables = indv_cols_to_ana,aggregation_level = "ind_gender")
analysis_by_age_group_indv <- butteR::mean_prop_working(design = indv_svy,list_of_variables = indv_cols_to_ana,aggregation_level = "age_group")
analysis_by_gender_age_group_indv <- butteR::mean_prop_working(design = indv_svy,list_of_variables = indv_cols_to_ana,aggregation_level = "gender_age_group")
analysis_by_age_group_grp_of_caregiver_indv <- butteR::mean_prop_working(design = indv_svy,list_of_variables = indv_cols_to_ana,
                                                                                   aggregation_level = c("group_of_caregivers","age_group"))
analysis_by_gender_age_of_caregiver_indv <- butteR::mean_prop_working(design = indv_svy,list_of_variables = indv_cols_to_ana,
                                                                                aggregation_level = c("group_of_caregivers","gender_age_group"))

analysis_by_age_grp_crvr_upazila_indv <- butteR::mean_prop_working(design = indv_svy,list_of_variables = indv_cols_to_ana,
                                                            aggregation_level = c("group_of_caregivers","age_group","upazila"))

analysis_by_gender_age_grp_crvr_upazila_indv <- butteR::mean_prop_working(design = indv_svy,list_of_variables = indv_cols_to_ana,
                                                              aggregation_level = c("ind_gender","group_of_caregivers","age_group","upazila"))

analysis_by_level_of_education_host_indv <- butteR::mean_prop_working(design = indv_svy,list_of_variables = indv_cols_to_ana,
                                                                                aggregation_level = "level_of_education_host")

analysis_by_level_of_education_camp_indv <- butteR::mean_prop_working(design = indv_svy,list_of_variables = indv_cols_to_ana,
                                                                                aggregation_level = "level_of_education_camp")





# write.csv ---------------------------------------------------------------

write_list <- list(
  "overall_analysis_hh"=overall_analysis_hh,
  "analysis_by_upazila_hh"=analysis_by_upazila_hh,
  "analysis_by_caregiver_grp_hh"=analysis_by_grp_of_crvr_hh,
  "analysis_by_upazila_caregiver_grp_hh"=analysis_by_upazila_and_grp_of_crvr_hh,
  "overall_analysis_indv"=overall_analysis_indv,
  "analysis_by_ind_gndr_indv"=analysis_by_ind_gender_indv,
  "analysis_by_age_grp_indv"=analysis_by_age_group_indv,
  "analysis_by_gender_age_grp_indv"=analysis_by_gender_age_group_indv,
  "analysis_by_age_group_grp_of_crgvr_indv"=analysis_by_age_group_grp_of_caregiver_indv,
  "analysis_by_gndr_age_of_crgvr_indv"=analysis_by_gender_age_of_caregiver_indv,
  "analysis_by_age_grp_crvr_upazila_indv"=analysis_by_age_grp_crvr_upazila_indv,
  "analysis_by_gndr_age_grp_crvr_upzla_indv"=analysis_by_gender_age_grp_crvr_upazila_indv,
  "analysis_by_lvl_of_education_host_indv"	=	analysis_by_level_of_education_host_indv,
  "analysis_by_lvl_of_education_camp_indv"	=	analysis_by_level_of_education_camp_indv
)

write.xlsx(write_list,paste0("outputs/basic_analysis/caregiver_basic_analysis/",str_replace_all(Sys.Date(),"-","_"),"_caregiver_basic_analysis.xlsx"))

