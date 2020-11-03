rm (list=ls())

library(pivottabler)
library(data.table)
library(survey)
library(srvyr)
library(dplyr)
library(butteR)
library(stringr)
library(openxlsx)
koboquest <- list.files("scrap/koboquest/R",full.names = T)

for (i in koboquest ){
   source(i)
}


population <- c("teacher","caregiver")[1]
write_output<- c("yes","no")[1]
# read_data ---------------------------------------------------------------
if (population == "teacher") {
teacher_raw_data <- read.csv("inputs/raw_data/teacher/edu_teacher_raw_data.csv",
                             na.strings = c(""," "),stringsAsFactors = F)
}



# read_tool ---------------------------------------------------------------
if (population == "teacher") {
assess_survey<- readxl::read_xls("DAP/tool/teacher/TEACHER_QUANT_Final.xls",sheet = "survey")
assess_choices<-readxl::read_xls("DAP/tool/teacher/TEACHER_QUANT_Final.xls",sheet = "choices")
assessment<-load_questionnaire(data = teacher_raw_data,questions = assess_survey,
                               choices = assess_choices,choices.label.column.to.use = "label::english")
}

# implementing cleanig log ------------------------------------------------
if (population == "teacher") {
cleaning_log <- read.csv("DAP/cleaning_log/teacher/teacher_cleaning_log_final.csv",
                           na.strings = c(""," "),stringsAsFactors = F) %>% dplyr::filter(!is.na(question))

cleaning_log$change_type <- cleaning_log$change_type %>% trimws()
check_log <- butteR::check_cleaning_log(df = teacher_raw_data,df_uuid = "X_uuid",cl = cleaning_log,
                                        cl_uuid = "uuid",cl_change_type_col = "change_type",
                                        cl_change_col = "question",cl_new_val = "new_value" ) 

cleaned_data <- butteR::implement_cleaning_log(df = teacher_raw_data,df_uuid = "X_uuid",cl = cleaning_log,
                                               cl_uuid = "uuid",cl_change_type_col = "change_type",
                                               cl_change_col = "question",cl_new_val = "new_value" )
}
check_log

# write cleaned data ------------------------------------------------------

if (write_output == "yes"){
write.csv(cleaned_data,paste0("outputs/cleaned_data/",population,"/",str_replace_all(Sys.Date(),"-","_"),"_",population,"_cleaned_data.csv"))
write.csv(cleaned_data,paste0("outputs/cleaned_data/",population,"/",population,"_cleaned_data.csv"))
write.xlsx(cleaned_data,paste0("outputs/cleaned_data/",population,"/",str_replace_all(Sys.Date(),"-","_"),"_",population,"_cleaned_data.xlsx"))
}

# consent_count -------------------------------------------------------------------

cleaned_data$upazila <- cleaned_data$upazila %>% tolower()

pt <- PivotTable$new()
pt$addData(cleaned_data)
pt$addColumnDataGroups("upazila")
pt$addRowDataGroups("informed_consent")
pt$defineCalculation(calculationName="upazila", summariseExpression="n()")
pt$renderPivot()
pt_df <- pt$asDataFrame()
consent_table <- pt_df %>% data.table(keep.rownames = T) %>% as.data.frame() %>% setnames(old = "rn",new = "consent")
# recoding ----------------------------------------------------------------
clean_df_yes_consent <- cleaned_data %>% dplyr::filter(informed_consent == "yes")

gender_grp_other_cols <- clean_df_yes_consent %>% dplyr::select(starts_with("gender_groups_other.")) %>% names()

clean_df_yes_consent$ki_code <- clean_df_yes_consent$ki_code %>% toupper()
clean_df_yes_consent$number_of_teachers_per_class <- clean_df_yes_consent$number_of_teachers_per_class %>% as.numeric()
clean_df_yes_consent$number_of_children <- clean_df_yes_consent$number_of_children %>% as.numeric()
clean_df_yes_consent$average_attrition <- clean_df_yes_consent$average_attrition %>% as.numeric()


 composite_indicator_df <- clean_df_yes_consent %>% dplyr::mutate(
   I.TEACHER.type_teacher= case_when(startsWith(ki_code,"A")| startsWith(ki_code,"B") ~ "bangla_camp_teacher",
                                     startsWith(ki_code,"C")| startsWith(ki_code,"D") ~ "burmese_teacher",
                                     startsWith(ki_code,"E")| startsWith(ki_code,"F") ~ "bangla_hc_teacher",
                                     T~NA_character_),
   gender_grp_other_rowsum = rowSums(clean_df_yes_consent[,gender_grp_other_cols],na.rm = T),
   I.SCHOOL.school_only_girls = if_else(gender_grp_other_rowsum == 1 & gender_groups_other.female ==1,"yes","no",NULL),
   I.SCHOOL.proportion_girls =	proportion_of_girls/total_students_in_school,
   I.TEACHER.home_based_teacher =	if_else(teaching_at_home_level == "always" ,"yes","no",NULL),
   I.TEACHER.home_based_teacher_camps	=if_else(teaching_at_home_level== "always" & teacher_groups == "camps","yes","no",NULL),
   I.TEACHER.home_based_teacher_hc	= if_else(teaching_at_home_level== "always" & teacher_groups == "host_communities","yes","no",NULL),
   I.TEACHER.gender_home_based_teacher = if_else(teaching_at_home_level== "always", resp_gender,NA_character_,NULL),
   I.SCHOOL.ratio_teacher_students = if_else(teaching_at_home_level != "always", 
                                             number_of_teachers_per_class/number_of_children,NULL,NULL),
   I.SCHOOL.ratio_schools=	case_when(I.SCHOOL.ratio_teacher_students < 1.35 ~ "low", 
                                     I.SCHOOL.ratio_teacher_students > 1.34 & I.SCHOOL.ratio_teacher_students < 1.41   ~ "ideal", 
                                     I.SCHOOL.ratio_teacher_students > 1.4 ~ "high", 
                                     T ~ NA_character_),
   
   I.SCHOOL.attrition_rate = 	average_attrition/number_of_teachers,
   #burmese_selection_criteria_rowsum = rowSums(clean_df_yes_consent[,burmese_teachers_recruitment_cols],na.rm = T),
   #I.SCHOOL.burmese_selection_criteria = burmese_teachers_recruitment  only for  burmese teachers (I.TEACHER.burmese_teacher.INDV)
   I.HB_LEARNING.ratio_teacher_students =	if_else(teaching_at_home_level == "always",
                                                  number_of_teachers_per_class/number_of_children,NULL,NULL),
   I.HB_LEARNING.ratio_schools=	case_when(I.HB_LEARNING.ratio_teacher_students < 2.10 ~ "low", 
                                          I.HB_LEARNING.ratio_teacher_students > 2.09 & I.HB_LEARNING.ratio_teacher_students < 2.16   ~ "ideal", 
                                          I.HB_LEARNING.ratio_teacher_students > 2.15 ~ "high", 
                                          T ~ NA_character_),
  
   I.SCHOOL.school_disabled_students= if_else(disabilities_by_type_of_disabilities.none == 0 ,"yes","no",NULL),
   I.SCHOOL.gender_segregated_classroom =if_else(seperating_girls_and_boys == "always","yes","no",NULL)
)


# burmese language --------------------------------------------------------

 burmese_teachers_recruitment_cols <- clean_df_yes_consent %>% dplyr::select(starts_with("burmese_teachers_recruitment.")) %>% names()
 
 for (i in burmese_teachers_recruitment_cols) {
    col_name = paste0("I.",i)
    composite_indicator_df[[col_name]] <- if_else(composite_indicator_df$I.TEACHER.type_teacher == "burmese_teacher",
                                               composite_indicator_df[[i]],
                                                NULL,NULL)
 }


# teacher lang numeric ----------------------------------------------------

teacher_lang_cols <- clean_df_yes_consent %>% dplyr::select(starts_with("teacher_lang.")) %>% names()
 for (i in teacher_lang_cols) {
    col_name_lang = i
    composite_indicator_df[[col_name_lang]] <- composite_indicator_df[[col_name_lang]] %>% as.numeric()
 }

# basic analysis ----------------------------------------------------------
 composite_indicator_df$upazila <- composite_indicator_df$upazila %>% tolower()

 dfsvy <- as_survey(composite_indicator_df)

cols_not_to_analze <-  c("survey_date", "camps","survey_start", "deviceid", "end_survey", "instance_name", 
                         "audit", "enumerator_id", "ki_code", "ki_code_confirm", "introduction", 
                         "informed_consent","end_note", "start", "end", "X_id", "X_uuid", "X_submission_time", 
                         "X_validation_status", "X_index","I.HB_LEARNING.ratio_teacher_students",
                         "I.SCHOOL.ratio_teacher_students")

is_not_empty<-function(x){ all(is.na(x))==FALSE}

cols_to_analyze <- composite_indicator_df %>% select(c(-ends_with("_other"),
                                                      -cols_not_to_analze)) %>% select_if(.,is_not_empty) %>% names()
 
#dfsvy$variables$teacher_lang.other<- forcats::fct_expand(dfsvy$variables$teacher_lang.other,c( 0, 1))
dfsvy$variables$I.SCHOOL.ratio_schools<- forcats::fct_expand(dfsvy$variables$I.SCHOOL.ratio_schools,c( "low","ideal", "high"))
dfsvy$variables$I.HB_LEARNING.ratio_schools<- forcats::fct_expand(dfsvy$variables$I.HB_LEARNING.ratio_schools,c( "low","ideal", "high"))


overall_analysis <- mean_prop_working(design = dfsvy,list_of_variables = cols_to_analyze)
analysis_by_upazila <- mean_prop_working(design = dfsvy,list_of_variables = cols_to_analyze,
                                                aggregation_level =  "upazila")
analysis_by_resp_gender <- mean_prop_working(design = dfsvy,list_of_variables = cols_to_analyze,
                                              aggregation_level =  "resp_gender")
analysis_by_teacher_groups <- mean_prop_working(design = dfsvy,list_of_variables = cols_to_analyze,
                                                aggregation_level =  "teacher_groups")
analysis_by_type_teacher <-mean_prop_working(design = dfsvy,list_of_variables = cols_to_analyze,
                                              aggregation_level =  "I.TEACHER.type_teacher")
 
if (write_output == "yes" ) {
   
 basic_analysis_write_list <- list(
                       "cleaned_data" = cleaned_data,
                       "cln_df_wth_cmpst_idctrs_ys_cnsnt"=composite_indicator_df,
                       "consent_frequency" = consent_table,
                       "overall_analysis"= overall_analysis,
                       "analysis_by_upazila"=analysis_by_upazila,
                       "analysis_by_resp_gender"=analysis_by_resp_gender,
                       "analysis_by_teacher_groups"=analysis_by_teacher_groups,
                       "analysis_by_type_teacher"=analysis_by_type_teacher)
 
 write.xlsx(basic_analysis_write_list, file = paste0("outputs/basic_analysis/teacher_basic_analysis/",str_replace_all(Sys.Date(),"-",""),"_",population,"_","basic_analysis.xlsx"))
 }