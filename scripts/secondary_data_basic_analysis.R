rm (list=ls())

library(dplyr)
library(butteR)


# read_data ---------------------------------------------------------------

match_data_raw <- read.csv("outputs/cleanded_5w_data/match_df.csv",na.strings = c(""," ","NA"),stringsAsFactors = F)

cleaned_data <- match_data_raw %>% dplyr::filter(change_value == 0 |change_value ==1) %>% 
  dplyr::filter(change_value >-1) %>% dplyr::filter(childage > 4) #remove demotion and change more than 1

cleaned_data$child_under_5 %>% AMR::freq()

cleaned_data %>% names()

# facility analysis -------------------------------------------------------

facility_df_raw <- cleaned_data %>% dplyr::select(Facility.ID,edu_lvl_teacher_host_2018,edu_lvl_teacher_mynmr_2018)

facility_df_rm_na <- facility_df_raw %>% dplyr::filter(!is.na(edu_lvl_teacher_host_2018)| !is.na(edu_lvl_teacher_mynmr_2018))


unique(facility_df_rm_na$Facility.ID) %>% length()
unique(facility_df_raw$Facility.ID) %>% length()

# add facility information in matched_data --------------------------------

cleaned_data_with_full_facility_data <- cleaned_data %>% 
  dplyr::select(-edu_lvl_teacher_host_2018,-edu_lvl_teacher_mynmr_2018) %>% 
  left_join(facility_df_rm_na)

cleaned_data$edu_lvl_teacher_host_2018 %>% AMR::freq()
cleaned_data_with_full_facility_data$edu_lvl_teacher_host_2018 %>% AMR::freq()


# analysis ----------------------------------------------------------------

data_for_analysis <- cleaned_data_with_full_facility_data %>% dplyr::mutate(
  change_edu_level = paste0("level_",level_of_the_student_2018,"_to_level_",level_of_the_student_2019),
  edu_level_2018 = paste0("level_",level_of_the_student_2018),
  edu_level_2019 = paste0("level_",level_of_the_student_2019),
  age_group= if_else(childage %in% 3:5,"3_to_5",
                     if_else(childage %in% 6:14,"6_to_14",
                             if_else(childage %in% 15:19,"15_to_19","above_range",NULL))),
  teacher_edu_level_host = if_else(edu_lvl_teacher_host_2018 == 0, "no_formal_education",
                                   if_else(edu_lvl_teacher_host_2018 %in% 1:5, "primary",
                                           if_else(edu_lvl_teacher_host_2018 %in% 6:10, "secondary",
                                                   if_else(edu_lvl_teacher_host_2018 %in% 11:12, "higher_secondary",
                                                           if_else(edu_lvl_teacher_host_2018 > 12, "tertiary","error",NULL))))),
  teacher_edu_level_mymnr = if_else(edu_lvl_teacher_mynmr_2018 == 0, "no_formal_education",
                                   if_else(edu_lvl_teacher_mynmr_2018 %in% 1:5, "primary",
                                           if_else(edu_lvl_teacher_mynmr_2018 %in% 6:10, "secondary",
                                                   if_else(edu_lvl_teacher_mynmr_2018 %in% 11:12, "higher_secondary",
                                                           if_else(edu_lvl_teacher_mynmr_2018 > 12, "tertiary","error",NULL)))))
) %>% setnames(old = "change_lable",new = "change_edu_level_status") %>% dplyr::select(-edu_lvl_teacher_host_2018,
                                                                                       -edu_lvl_teacher_mynmr_2018,
                                                                                       -childage,
                                                                                       -level_of_the_student_2018,
                                                                                       -level_of_the_student_2019)


df_svy <- as_survey(data_for_analysis)

df_svy$variables$lived_with_parent_2018 <-  forcats::fct_expand(df_svy$variables$lived_with_parent_2018,c( "yes", "no"))
df_svy$variables$child_under_5 <-  forcats::fct_expand(df_svy$variables$child_under_5,c( "no", "yes"))

col_to_analyse <- c("edu_level_2018","edu_level_2019","age_group",
                      "lived_with_parent_2018", "teacher_edu_level_host","teacher_edu_level_mymnr",
                      "sex_2018", "std_previous_education_2018", 
                      "change_edu_level_status", "child_under_5", "child_under_6", 
                      "edu_lvl_teacher_host_2018", "edu_lvl_teacher_mynmr_2018", "change_edu_level")

overall_analysis <- mean_prop_working(design = df_svy,list_of_variables =col_to_analyse )
analysis_by_gender <- mean_prop_working(design = df_svy,list_of_variables =col_to_analyse,aggregation_level = "sex_2018")
analysis_by_camp <- mean_prop_working(design = df_svy,list_of_variables =col_to_analyse,aggregation_level = "camp_2019" )
analysis_by_age_grp <- mean_prop_working(design = df_svy,list_of_variables =col_to_analyse,aggregation_level = "age_group")
na_response_rate <- butteR::get_na_response_rates(data_for_analysis)


df_write_list <- list("overall_analysis"= overall_analysis,
                      "analysis_by_gender"=analysis_by_gender,
                      "analysis_by_camp"=analysis_by_camp,
                      "analysis_by_age_grp"=analysis_by_age_grp,
                      "na_response_rate"=na_response_rate)

write.xlsx(df_write_list, file = paste0("outputs/basic_analysis_5w_data/",str_replace_all(Sys.Date(),"-",""),"_","basic_analysis_5w_data",".xlsx"))
