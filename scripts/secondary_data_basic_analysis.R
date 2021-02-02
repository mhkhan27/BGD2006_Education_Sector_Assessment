rm (list=ls())

library(dplyr)
library(butteR)
library(data.table)
library(srvyr)
library(openxlsx)
library(stringr)
library(tidyr)
library(illuminate)
#source("scrap/response_frequency.R")

write_output <- c("yes","no")[1]


# camp_name ---------------------------------------------------------------


camp_info <- read.csv("DAP/camp_info/camp_name.csv") %>% select(Upazila,Camp_Name)

# read_data ---------------------------------------------------------------

cleaned_data <- read.csv("outputs/cleaned_data/secondary_data/matched_data.csv",na.strings = c(""," ","NA"),stringsAsFactors = F)

cleaned_data <- cleaned_data %>% left_join(camp_info,by= c("camp_2019"="Camp_Name"))

cleaned_data$child_under_5 %>% AMR::freq()

cleaned_data %>% names()

cleaned_data <- cleaned_data %>% filter(childage > 4)

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
  change_out_of_level_1 = if_else(startsWith(change_edu_level,"level_1"),change_edu_level,NULL,NULL),
  change_out_of_level_2 = if_else(startsWith(change_edu_level,"level_2"),change_edu_level,NULL,NULL),
  change_out_of_level_3 = if_else(startsWith(change_edu_level,"level_3"),change_edu_level,NULL,NULL),
  change_out_of_level_4 = if_else(startsWith(change_edu_level,"level_4"),change_edu_level,NULL,NULL),
  age_group= if_else(childage %in% 5:9,"5_to_9",
                     if_else(childage %in% 10:15,"10_to_15","above_range",NULL)),
  child_age_2018 = childage %>% as.factor(),
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
) %>% setnames(old = c("change_lable","std_previous_education_2018")
,new = c("change_edu_level_status","student_previous_education_2018")) %>% dplyr::select(-ID_2018,
                                                                                       -ID_2019,
                                                                                       -centerid,
                                                                                       -level_of_the_student_2018,
                                                                                       -level_of_the_student_2019)



write.csv(data_for_analysis,"outputs/significant_testing/secondary/data_for_significance_testing/cleaned_data_for_significance_testing.csv")

df_svy <- as_survey(data_for_analysis)

df_svy$variables$lived_with_parent_2018 <-  forcats::fct_expand(df_svy$variables$lived_with_parent_2018,c( "yes", "no"))
df_svy$variables$child_under_5 <-  forcats::fct_expand(df_svy$variables$child_under_5,c( "no", "yes"))
df_svy$variables$change_out_of_level_4 <-  forcats::fct_expand(df_svy$variables$change_out_of_level_4,c( "level_4_to_level_4", "others"))

df_svy$variables$child_age_2018 <- df_svy$variables$childage %>% as.factor()

col_to_analyse <- c("edu_level_2018","edu_level_2019","age_group","child_age_2018","change_out_of_level_1",
                    "change_out_of_level_2","change_out_of_level_3","change_out_of_level_4",
                      "lived_with_parent_2018", "teacher_edu_level_host","teacher_edu_level_mymnr",
                      "sex_2018", "student_previous_education_2018","Upazila", 
                      "change_edu_level_status", "child_under_5", "child_under_6", 
                       "change_edu_level")

# analysis ----------------------------------------------------------------

overall_analysis <- mean_prop_working(design = df_svy,list_of_variables =col_to_analyse)
analysis_by_implementing_partner <- mean_prop_working(design = df_svy,list_of_variables =col_to_analyse,aggregation_level = "Implementing.partner" )
analysis_by_upazila <- mean_prop_working(design = df_svy,list_of_variables =col_to_analyse,aggregation_level = "Upazila")
analysis_by_each_age <- mean_prop_working(design = df_svy,list_of_variables =col_to_analyse,aggregation_level = "child_age_2018" )
analysis_by_gender <- mean_prop_working(design = df_svy,list_of_variables =col_to_analyse,aggregation_level = "sex_2018")
analysis_by_gender_by_age <- mean_prop_working(design = df_svy,list_of_variables =col_to_analyse,aggregation_level = c("sex_2018","age_group"))
analysis_by_camp <- mean_prop_working(design = df_svy,list_of_variables =col_to_analyse,aggregation_level = "camp_2019" )
analysis_by_age_grp <- mean_prop_working(design = df_svy,list_of_variables =col_to_analyse,aggregation_level = "age_group")
analysis_by_previous_education <- mean_prop_working(design = df_svy,list_of_variables =col_to_analyse,aggregation_level = "student_previous_education_2018")
analysis_by_host_teacher_edu_level <- mean_prop_working(design = df_svy,list_of_variables =col_to_analyse,aggregation_level = "teacher_edu_level_host")
analysis_by_mynmr_teacher_edu_level <- mean_prop_working(design = df_svy,list_of_variables =col_to_analyse,aggregation_level = "teacher_edu_level_mymnr")


na_response_rate <- butteR::get_na_response_rates(data_for_analysis)

# frequency_by_choices ----------------------------------------------------
# 
survey_frequency_by_choices_overall <- survey_frequency_by_choices(df = data_for_analysis,variables_to_analyze =  col_to_analyse)
survey_frequency_by_choices_by_each_age <- survey_frequency_by_choices(df = data_for_analysis,variables_to_analyze =col_to_analyse,aggregation_level = "child_age_2018" )
survey_frequency_by_choices_by_implementing_partner <- survey_frequency_by_choices(df = data_for_analysis,variables_to_analyze = col_to_analyse,aggregation_level = "Implementing.partner")
survey_frequency_by_choices_by_gender <- survey_frequency_by_choices(df = data_for_analysis,variables_to_analyze =col_to_analyse,aggregation_level = "sex_2018" )
survey_frequency_by_choices_by_gender_by_age <- survey_frequency_by_choices(df = data_for_analysis,variables_to_analyze = col_to_analyse,aggregation_level = c("sex_2018","age_group"))
survey_frequency_by_choices_by_camp <- survey_frequency_by_choices(df = data_for_analysis,variables_to_analyze =col_to_analyse,aggregation_level = "camp_2019" )
survey_frequency_by_choices_by_age_grp <- survey_frequency_by_choices(df = data_for_analysis,variables_to_analyze =col_to_analyse,aggregation_level = "age_group" )
survey_frequency_by_choices_by_previous_education <- survey_frequency_by_choices(df = data_for_analysis,variables_to_analyze =col_to_analyse,aggregation_level = "student_previous_education_2018" )
survey_frequency_by_choices_by_host_teacher_edu_level <- survey_frequency_by_choices(df = data_for_analysis,variables_to_analyze =col_to_analyse,aggregation_level = "teacher_edu_level_host" )
survey_frequency_by_choices_by_mynmr_teacher_edu_level <- survey_frequency_by_choices(df = data_for_analysis,variables_to_analyze =col_to_analyse,aggregation_level = "teacher_edu_level_mymnr" )
survey_frequency_by_choices_by_upazila <- survey_frequency_by_choices(df = data_for_analysis,variables_to_analyze =col_to_analyse,aggregation_level = "Upazila" )


# frequency_by_questions --------------------------------------------------

survey_frequency_by_questions_overall <- survey_frequency_by_questions(df = data_for_analysis)
survey_frequency_by_questions_by_each_age <- survey_frequency_by_questions(df = data_for_analysis,aggregation_level = "child_age_2018" )
survey_frequency_by_questions_by_implementing_partner <- survey_frequency_by_questions(df = data_for_analysis,aggregation_level = "Implementing.partner")
survey_frequency_by_questions_by_gender <- survey_frequency_by_questions(df = data_for_analysis,aggregation_level = "sex_2018" )
survey_frequency_by_questions_by_gender_by_age <- survey_frequency_by_questions(df = data_for_analysis,aggregation_level = c("sex_2018","age_group"))
survey_frequency_by_questions_by_camp <- survey_frequency_by_questions(df = data_for_analysis,aggregation_level = "camp_2019" )
survey_frequency_by_questions_by_age_grp <- survey_frequency_by_questions(df = data_for_analysis,aggregation_level = "age_group" )
survey_frequency_by_questions_by_previous_education <- survey_frequency_by_questions(df = data_for_analysis,aggregation_level = "student_previous_education_2018" )
survey_frequency_by_questions_by_host_teacher_edu_level <- survey_frequency_by_questions(df = data_for_analysis,aggregation_level = "teacher_edu_level_host" )
survey_frequency_by_questions_by_mynmr_teacher_edu_level <- survey_frequency_by_questions(df = data_for_analysis,aggregation_level = "teacher_edu_level_mymnr" )

survey_frequency_by_questions_by_upazila <- survey_frequency_by_questions(df = data_for_analysis,aggregation_level = "Upazila" )



# aser_data_analysis ------------------------------------------------------

aser_data <- read.csv("inputs/raw_data/secondary_data/2018/Final dataset-ASER plus 19082019.csv",
                      na.strings = c(""," ","N/A","#NULL!"),stringsAsFactors = F) %>% mutate(
                        edu_lvl_teacher_host_2018 = teacherhost_2,
                        edu_lvl_teacher_mynmr_2018 = teachermyan_2
                      ) %>% mutate(
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
                      )

host_teacher_edu <-aser_data$teacher_edu_level_host %>% AMR::freq() %>% as.data.frame() %>% rename("host_edu_level"="item")
mynmr_teacher_edu <-aser_data$teacher_edu_level_mymnr %>% AMR::freq() %>% as.data.frame() %>% rename("mynmr_edu_level"="item")




if(write_output == "yes"){
data_for_analysis <- data_for_analysis %>% dplyr::select(-teacher_edu_level_host,
                                                         -teacher_edu_level_mymnr,
                                                         -child_under_5,
                                                         -change_value,	
                                                         -change_edu_level_status,
                                                         -child_under_6,
                                                         -change_edu_level,
                                                         	-age_group)


df_write_list <- list("ASER_host_teacher_edu" =host_teacher_edu,
                      "ASER_mynmr_teacher_edu"=mynmr_teacher_edu,
                      "cleaned_data" = data_for_analysis,
                      "overall_analysis"= overall_analysis,
                      "analysis_by_upazila"=analysis_by_upazila,
                      "analysis_by_implmntng_prtnr"=analysis_by_implementing_partner,
                      "analysis_by_each_age"=analysis_by_each_age,
                      "analysis_by_gender"=analysis_by_gender,
                      "analysis_by_gender_by_age"=analysis_by_gender_by_age,
                      "analysis_by_age_grp"=analysis_by_age_grp,
                      "analysis_by_camp"=analysis_by_camp,
                      "analysis_by_previous_education"=analysis_by_previous_education,
                      "analysis_by_host_teacher_edu_level"=analysis_by_host_teacher_edu_level,
                      "analysis_by_mynmr_teacher_edu_level"=analysis_by_mynmr_teacher_edu_level,
                      "na_response_rate"=na_response_rate)



df_write_list_freq_by_choices <- list(
  "fre_by_choices_overall" =survey_frequency_by_choices_overall,
  "fre_by_choices_by_upazila"=survey_frequency_by_choices_by_upazila,
  "fre_by_choices_by_each_age"=survey_frequency_by_choices_by_each_age,
  "fre_by_choices_by_implementing_partner"=survey_frequency_by_choices_by_implementing_partner,
  "fre_by_choices_by_gender"=survey_frequency_by_choices_by_gender,
  "fre_by_choices_by_gender_by_age"=survey_frequency_by_choices_by_gender_by_age,
  "fre_by_choices_by_camp"=survey_frequency_by_choices_by_camp,
  "fre_by_choices_by_age_grp"=survey_frequency_by_choices_by_age_grp,
  "fre_by_choices_by_previous_education"=survey_frequency_by_choices_by_previous_education,
  "fre_by_choices_by_host_teacher_edu_level"=survey_frequency_by_choices_by_host_teacher_edu_level,
  "fre_by_choices_by_mynmr_teacher_edu_level"=survey_frequency_by_choices_by_mynmr_teacher_edu_level)



df_write_list_freq_by_questions <- list(
  "fre_by_questions_overall" =survey_frequency_by_questions_overall,
  "fre_by_by_questions_by_upazila"=survey_frequency_by_questions_by_upazila,
  "fre_by_questions_by_each_age"=survey_frequency_by_questions_by_each_age,
  "fre_by_questions_by_implementing_partner"=survey_frequency_by_questions_by_implementing_partner,
  "fre_by_questions_by_gender"=survey_frequency_by_questions_by_gender,
  "fre_by_questions_by_gender_by_age"=survey_frequency_by_questions_by_gender_by_age,
  "fre_by_questions_by_camp"=survey_frequency_by_questions_by_camp,
  "fre_by_questions_by_age_grp"=survey_frequency_by_questions_by_age_grp,
  "fre_by_questions_by_previous_education"=survey_frequency_by_questions_by_previous_education,
  "fre_by_questions_by_host_teacher_edu_level"=survey_frequency_by_questions_by_host_teacher_edu_level,
  "fre_by_questions_by_mynmr_teacher_edu_level"=survey_frequency_by_questions_by_mynmr_teacher_edu_level)


write.xlsx(df_write_list, file = paste0("outputs/basic_analysis/secondary_data_basic_analysis/",str_replace_all(Sys.Date(),"-",""),"_","basic_analysis_secondary_data",".xlsx"))
write.xlsx(df_write_list_freq_by_choices, file = paste0("outputs/response_rate/secondary_data_basic_analysis/",str_replace_all(Sys.Date(),"-",""),"_","freq_by_choices_secondary_data",".xlsx"))
write.xlsx(df_write_list_freq_by_questions, file = paste0("outputs/response_rate/secondary_data_basic_analysis/",str_replace_all(Sys.Date(),"-",""),"_","freq_by_questions_secondary_data",".xlsx"))


}



