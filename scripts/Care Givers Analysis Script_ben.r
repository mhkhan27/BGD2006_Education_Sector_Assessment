
#---------------------------------
# Education Sector - Care Givers
# Analysis Script
#---------------------------------
# Ben.Smith@reach.initiative.org
# 26/11/2020
#-----------


# Preamble ----------------------------------------------------------------

# For manipulating inputs
  library(dplyr)
  library(openxlsx)
  library(stringr)
  library(openxlsx)
  library(butteR)

# For analysis
  library(srvyr)
  library(survey)

# Functions:
  
  # Split a vector of strings by "AND" and "&" and trim white space:
  kat_split_by_conjunction = function(string_vector){
    spit_string_vector = gsub(pattern = c("AND"), replacement = ",", x = string_vector)
    spit_string_vector = gsub(pattern = c("&"), replacement = ",", x = spit_string_vector)
    spit_string_vector = gsub(pattern = c(" "), replacement = "", x = spit_string_vector)
    spit_string_vector = unique(strsplit(x = spit_string_vector, split = ","))
    return(spit_string_vector)
  }

# Choose whether you would like to write outputs:
write_output <-c("yes","no")[1]

# Set the directory:
setwd("C:/Users/Ben SMITH/Documents/Bangladesh/Projects/Education Sector Assessment/")

# Read in the raw data ----------------------------------------------------

# Household Response Data:
hh <- read.csv("Inputs/Household Survey Responses - BGD2006_Education_Caregivers_Final - latest_version - False - 2020-11-29-03-47-08.csv", 
               check.names =FALSE,
               na.strings = c(""," ","NA","N/A"), 
               stringsAsFactors = F) %>% rename("X_uuid" = "_uuid")

# Individual Response Data:
indv <- read.csv("Inputs/Individual Survey Responses - BGD2006_Education_Caregivers_Final - latest_version - False - 2020-11-29-03-47-08.csv", 
                 check.names =FALSE,
                 na.strings = c("", " ", "NA", "N/A"), 
                 stringsAsFactors = F) %>% rename("X_uuid" = "_submission__uuid")

# Data Upazila Weightings:
  # Households in host community:
  weighting_hc <- read.xlsx(xlsxFile = "Inputs/Weightings - number of households.xlsx",
                               sheet = "HC", na.strings = c(""," ","NA","N/A"))[,c(1,3)]
  # Households in camps:
  weighting_camp <- read.xlsx(xlsxFile = "Inputs/Weightings - number of households.xlsx",
                            sheet = "Camp", na.strings = c(""," ","NA","N/A"))[,c(1,3)]

# The Data Analysis Plan:
DAP <- read.xlsx(xlsxFile = "Other/CAREGIVER-SURVEY_DA instructions_v2.xlsx",
                 sheet = "Basic statistics", na.strings = c(""," ","NA","N/A"),
                 colNames = TRUE, rowNames = FALSE,
                 skipEmptyRows = TRUE, skipEmptyCols = TRUE, 
                 startRow = 3, cols = c(2,5,6))


# The Cleaning log:
cleaning_log <- read.xlsx("Inputs/20201130_Data_cleaning_logbook_v2.xlsx",
                          na.strings = c(""," ","NA","N/A"))


# Data cleaning ------------------------------------------------------------

# Clean the Household responses:
  # Filter the cleaning data to only household:
  cleaning_log_hh <- filter(cleaning_log, dataset_loop== "household" & change_type != "no_action")
  
  # Check the cleaning log for potential issues:
  View(check_cleaning_log(df = hh, 
                     df_uuid = "X_uuid", 
                     cl = cleaning_log_hh, 
                     cl_change_type_col ="change_type", 
                     cl_change_col ="question", 
                     cl_uuid = "uuid",
                     cl_new_val = "new_value"))
  
  # Change values according to the cleaning log:
  hh_clean_data <- implement_cleaning_log(df = hh,
                                          df_uuid = "X_uuid",
                                          cl = cleaning_log_hh,
                                          cl_change_type_col ="change_type",
                                          cl_change_col ="question",
                                          cl_uuid = "uuid",
                                          cl_new_val = "new_value")


# Clean the individual responses:
  # Filter the data to only individual responses:
  cleaning_log_indv  <- filter(cleaning_log, dataset_loop== "individual" & change_type != "no_action")
  
  # Check the cleaning log for potential issues:
  View(check_cleaning_log(df = indv, 
                          df_uuid = "X_uuid", 
                          cl = cleaning_log_indv, 
                          cl_change_type_col ="change_type", 
                          cl_change_col ="question", 
                          cl_uuid = "uuid",
                          cl_new_val = "new_value"))
  
  # Change values according to the cleaning log:
  indv_clean_data <- implement_cleaning_log(df = indv,
                                          df_uuid = "X_uuid",
                                          cl = cleaning_log_indv,
                                          cl_change_type_col ="change_type",
                                          cl_change_col ="question",
                                          cl_uuid = "uuid",
                                          cl_new_val = "new_value")
  
# Write cleaned data:
  if(write_output == "yes"){

    write.xlsx(x = list("hh_loop" = hh_clean_data,
                        "individual_loop" = indv_clean_data),
               
               file = paste0("outputs/02_clean_data/",
                             str_replace_all(Sys.Date(),"-","_"),
                             "_cleaned_data.xlsx"))
  }
  
# Remove all responses that do not have consent:
  hh_clean_consent <- hh_clean_data %>% dplyr::filter(informed_consent == "yes")
  

# Create Composite Variables (individual data only) ------------------------

  # Set up to add columns:
  indv_clean_with_composite_idicators <- indv_clean_data %>% dplyr::mutate(
    
    # Add column of age group:
    age_group = case_when(age_of_child %in% 3:10 ~ "age_group_3_10",
                          age_of_child %in% 11:14 ~ "age_group_11_14",
                          age_of_child %in% 15:18 ~ "age_group_15_18",
                          age_of_child %in% 19:24 ~ "age_group_19_24"),
    
    # Add an age & gender column:
    gender_age_group = case_when(girl_3_10 >0 ~ "girl_3_10",
                                     girl_11_14 >0 ~ "girl_11_14",
                                     girl_15_18 >0 ~ "girl_15_18",
                                     boy_3_10 >0 ~ "boy_3_10",
                                     boy_11_14 >0 ~ "boy_11_14",
                                     boy_15_18 >0 ~ "boy_15_18",
                                     girl_19_24 >0 ~ "girl_19_24",
                                     boy_19_24 >0 ~ "boy_19_24"),
    
    # Add a disabled seeing column:
    disabled_seeing = if_else(
      difficulties_with_seeing %in% c("lot_of_difficulty", "cannot_do_at_all"),
      true = "yes", false = "no", missing = NULL),
    
    # Add a disabled hearing column:
    disabled_hearing = if_else(
      difficulties_with_hearing %in% c("lot_of_difficulty", "cannot_do_at_all"),
      true = "yes", false = "no", missing = NULL),
    
    # Add a disabled climbing column:
    disabled_climbing = if_else(
      difficulties_with_climbing %in% c("lot_of_difficulty", "cannot_do_at_all"),
      true = "yes", false = "no", missing = NULL),
    
    # Add a disabled remembering column:
    disabled_remembering = if_else(
      difficulity_remembering %in% c("lot_of_difficulty", "cannot_do_at_all"),
      true = "yes", false = "no", missing = NULL),
    
    # Add a disabled self care column:
    disabled_self_care = if_else(
      difficulty_with_self_care %in% c("lot_of_difficulty", "cannot_do_at_all"),
      true = "yes", false = "no", missing = NULL),
    
    # Add a disabled communication care column:
    disabled_communication = if_else(
      difficulity_communicating %in% c("lot_of_difficulty", "cannot_do_at_all"),
      true = "yes", false = "no", missing = NULL),
    
    # Add a disabled self care column:
    level_of_education_camp = if_else(
      difficulty_with_self_care %in% c("lot_of_difficulty", "cannot_do_at_all"),
      true = "yes", false = "no", missing = NULL)
    )
  
  # Add on the Upazila column from the hh data using UUID as a match:
  indv_clean_with_composite_idicators = 
    left_join(x = indv_clean_with_composite_idicators, 
              y = hh_clean_consent[,c("X_uuid", "upazila", "group_of_caregivers")],
              by = "X_uuid")
  
# Write Composite Indicators ----------------------------------------------

  if(write_output == "yes"){
    
    write.xlsx(x = indv_clean_with_composite_idicators,
                file = paste0("Outputs/03_composite_indicators_with_data/",
                              "Individual data - composite indicators with cleaned data - ",
                              str_replace_all(Sys.Date(),"-","_"), ".xlsx"))
  }


# Analysis ----------------------------------------------------------------
  
# Tidy the workspace:
  hh = hh_clean_consent
  indv = indv_clean_with_composite_idicators
  # rm(list=ls()[! ls() %in% c("DAP", "hh", "indv", "kat_split_by_conjunction")])
  
# Create survey objects (this is to help with NA values):
  
  # Setup the weightings:
  # Weighting = sample upazila population / total population / upazila samples / total samples
  #   >> (sample upazila population / total population) x (total samples / upazila samples)
  #     >> (Total samples/total population) x (sample population / no. samples)
  #       >> constant * (sample population / no. samples)
  
  # Create a dataframe to hold some summary stats for the weightings:
  weight_tble = data.frame("camp_Ukhia" = sum(weighting_camp$Total_HH[weighting_camp$Upazila == "Ukhia"]),
                           "camp_Teknaf" = sum(weighting_camp$Total_HH[weighting_camp$Upazila == "Teknaf"]),
                           "hc_Ukhia" =  sum(weighting_hc$number_hh[weighting_hc$Upazila == "Ukhia"]),
                           "hc_Teknaf" = sum(weighting_hc$number_hh[weighting_hc$Upazila == "Teknaf"]))
  
  weight_tble$camp_total_count = weight_tble$camp_Ukhia+weight_tble$camp_Teknaf
  weight_tble$hc_total_count = weight_tble$hc_Ukhia+weight_tble$hc_Teknaf
  
  weight_tble$camp_Teknaf_sample_count = nrow(hh %>% filter(upazila == "teknaf" & group_of_caregivers == "camps"))
  weight_tble$camp_Ukhia_sample_count = nrow(hh %>% filter(upazila == "ukhiya" & group_of_caregivers == "camps"))
  
  weight_tble$hc_Teknaf_sample_count = nrow(hh %>% filter(upazila == "teknaf" & group_of_caregivers == "host_communities"))
  weight_tble$hc_Ukhia_sample_count = nrow(hh %>% filter(upazila == "ukhiya" & group_of_caregivers == "host_communities"))
  
  
  # Create a function to calculate the weightings:
  calc_weight = function(division_count, total_count, division_sample_count, total_sample_count, constant){

    x = ifelse(test = exists("constant"),
               yes = constant * division_count/division_sample_count,
               no = total_count/total_sample_count * division_count/division_sample_count)
    
    return(x)
  }
  
  # Calculate the constants for the calc_weight function:
  c_camp = nrow(hh %>% filter(group_of_caregivers == "camps")) / weight_tble$camp_total_count
  c_hc = nrow(hh %>% filter(group_of_caregivers == "host_communities")) / weight_tble$hc_total_count
  
  # Add a weightings column to the household data:
  hh = hh %>% dplyr::mutate(
    weight = case_when(
      group_of_caregivers == "camps" & upazila == "ukhiya" ~ calc_weight(constant = c_camp,
                                                                        division_count = weight_tble$camp_Ukhia,
                                                                        division_sample_count = weight_tble$camp_Ukhia_sample_count),
      group_of_caregivers == "camps" & upazila == "teknaf" ~ calc_weight(constant = c_camp,
                                                                         division_count = weight_tble$camp_Teknaf,
                                                                         division_sample_count = weight_tble$camp_Teknaf_sample_count),
      group_of_caregivers == "host_communities" & upazila == "ukhiya" ~ calc_weight(constant = c_hc,
                                                                                    division_count = weight_tble$hc_Ukhia,
                                                                                    division_sample_count = weight_tble$hc_Ukhia_sample_count),
      group_of_caregivers == "host_communities" & upazila == "teknaf" ~ calc_weight(constant = c_hc,
                                                                                    division_count = weight_tble$hc_Teknaf,
                                                                                    division_sample_count = weight_tble$hc_Teknaf_sample_count)))
  
  # Add a weighting column to the individual data:
  indv = indv %>% dplyr::mutate(
    weight = case_when(
      group_of_caregivers == "camps" & upazila == "ukhiya" ~ calc_weight(constant = c_camp,
                                                                         division_count = weight_tble$camp_Ukhia,
                                                                         division_sample_count = weight_tble$camp_Ukhia_sample_count),
      group_of_caregivers == "camps" & upazila == "teknaf" ~ calc_weight(constant = c_camp,
                                                                         division_count = weight_tble$camp_Teknaf,
                                                                         division_sample_count = weight_tble$camp_Teknaf_sample_count),
      group_of_caregivers == "host_communities" & upazila == "ukhiya" ~ calc_weight(constant = c_hc,
                                                                                    division_count = weight_tble$hc_Ukhia,
                                                                                    division_sample_count = weight_tble$hc_Ukhia_sample_count),
      group_of_caregivers == "host_communities" & upazila == "teknaf" ~ calc_weight(constant = c_hc,
                                                                                    division_count = weight_tble$hc_Teknaf,
                                                                                    division_sample_count = weight_tble$hc_Teknaf_sample_count)))
      
  # Create survey objects:
  hh_srv = as_survey(hh, weights = weight)
  hh_unweighted_srv = as_survey(hh)
  indv_srv = as_survey(indv, weights = weight)
  indv_unweighted_srv = as_survey(indv)

# The following lines of the DAP refer to the household and individual analyses:
  hh_DAP_rows = c(1:10,114:146)
  indv_DAP_rows = c(11:35, 37:66, 68:95, 98:113)
  # >> 96:97 are excluded as Ben thinks they are incorrect.
  # >> 36 and 
  
# Create empty lists to take the analysis results:
  hh_results = list()
  hh_unweighted_results = list()
  indv_results = list()
  indv_unweighted_results = list()
  

# Household Assessment ----------------------------------------------------
  
  # Find the groups to analyse with:
  group_names = unique(trimws(unlist(strsplit(x = DAP$Analysis.Level[hh_DAP_rows], split = ","))))
  
  # Convert analysis levels that contain "AND"s and "&"s into a list of vectors: 
  groups = kat_split_by_conjunction(group_names)

  # Set up a loop to run through the data and fill the list:
  for(i in 1:length(group_names)){
    
    print(paste0(i, ". -------- ", group_names[i], " --------"))
    
    # Find which columns (variables) are in the group:
    cols_to_analyze = DAP$Main.variable.of.interest[grep(pattern = group_names[i], x = DAP$Analysis.Level)]
    
    # Clip these columns to only those that are in the household analysis:
    cols_to_analyze = cols_to_analyze[cols_to_analyze %in% DAP$Main.variable.of.interest[hh_DAP_rows]]
    
    if(all(groups[[i]] == "overall")){x = mean_prop_working(design = hh_srv,
                                                     list_of_variables = cols_to_analyze)
    
    } else {x = mean_prop_working(design = hh_srv,
                                  list_of_variables = cols_to_analyze,
                                  aggregation_level = groups[[i]])}
    
    hh_results[[substr(x = group_names[i], start=1, stop = 31)]] =  x
    
    # Paste the group names into the sheet:
    if(i == length(group_names)){hh_results[["groupings"]] =  group_names}
  
  }
  

  # Household Unweighted Assessment ----------------------------------------
  
  # Set up a loop to run through the data and fill the list:
  for(i in 1:length(group_names)){
    
    print(paste0(i, ". -------- ", group_names[i], " --------"))
    
    # Find which columns (variables) are in the group:
    cols_to_analyze = DAP$Main.variable.of.interest[grep(pattern = group_names[i], x = DAP$Analysis.Level)]
    
    # Clip these columns to only those that are in the household analysis:
    cols_to_analyze = cols_to_analyze[cols_to_analyze %in% DAP$Main.variable.of.interest[hh_DAP_rows]]
    
    if(all(groups[[i]] == "overall")){x = mean_prop_working(design = hh_unweighted_srv,
                                                            list_of_variables = cols_to_analyze)
    
    } else {x = mean_prop_working(design = hh_unweighted_srv,
                                  list_of_variables = cols_to_analyze,
                                  aggregation_level = groups[[i]])}
    
    hh_unweighted_results[[substr(x = group_names[i], start=1, stop = 31)]] =  x
    
    # Paste the group names into the sheet:
    if(i == length(group_names)){hh_unweighted_results[["groupings"]] =  group_names}
    
  }
  

# Individual Assessment ---------------------------------------------------
  
  # Find the groups to analyse with:
  group_names = unique(trimws(unlist(strsplit(x = DAP$Analysis.Level[indv_DAP_rows], split = ","))))
  
  # Remove host_attending_education AND level_of_education_camp as Ben thinks its broken:
  group_names = group_names[!group_names =="host_attending_education AND level_of_education_camp"]
  
  # Convert analysis levels that contain "AND"s and "&"s into a list of vectors: 
  groups = kat_split_by_conjunction(group_names)
  
  
  # Set up a loop to run through the data and fill the list:
  for(i in 1:length(group_names)){
    
    print(paste0(i, ". -------- ", group_names[i], " --------"))
    
    # Find which columns (variables) are in the group:
    cols_to_analyze = DAP$Main.variable.of.interest[grep(pattern = group_names[i], x = DAP$Analysis.Level)]
    
    # Clip these columns to only those that are in the household analysis:
    cols_to_analyze = cols_to_analyze[cols_to_analyze %in% DAP$Main.variable.of.interest[indv_DAP_rows]]
    
    if(all(groups[[i]] == "overall")){x = mean_prop_working(design = indv_srv,
                                                     list_of_variables = cols_to_analyze)
    
    } else {x = mean_prop_working(design = indv_srv,
                                  list_of_variables = cols_to_analyze,
                                  aggregation_level = groups[[i]])}
    
    indv_results[[substr(x = group_names[i], start=1, stop = 31)]] =  x
    
    # Paste the group names into the sheet:
    if(i == length(group_names)){indv_results[["groupings"]] =  group_names}
  }
  
  
# Individual Assessment Unweighted -----------------------------------------
  
  # Set up a loop to run through the data and fill the list:
  for(i in 1:length(group_names)){
    
    print(paste0(i, ". -------- ", group_names[i], " --------"))
    
    # Find which columns (variables) are in the group:
    cols_to_analyze = DAP$Main.variable.of.interest[grep(pattern = group_names[i], x = DAP$Analysis.Level)]
    
    # Clip these columns to only those that are in the household analysis:
    cols_to_analyze = cols_to_analyze[cols_to_analyze %in% DAP$Main.variable.of.interest[indv_DAP_rows]]
    
    if(all(groups[[i]] == "overall")){x = mean_prop_working(design = indv_unweighted_srv,
                                                     list_of_variables = cols_to_analyze)
    
    } else {x = mean_prop_working(design = indv_srv,
                                  list_of_variables = cols_to_analyze,
                                  aggregation_level = groups[[i]])}
    
    indv_unweighted_results[[substr(x = group_names[i], start=1, stop = 31)]] =  x
    
    # Paste the group names into the sheet:
    if(i == length(group_names)){indv_unweighted_results[["groupings"]] =  group_names}
  }


# Write data --------------------------------------------------------------

  
write.xlsx(hh_results, paste0("Outputs/04_basic_analysis/", str_replace_all(Sys.Date(),"-","_"), "_hh_analysis_2.xlsx"))
write.xlsx(hh_unweighted_results, paste0("Outputs/04_basic_analysis/", str_replace_all(Sys.Date(),"-","_"), "_hh_unweighted_analysis_1.xlsx"))

write.xlsx(indv_results, paste0("Outputs/04_basic_analysis/", str_replace_all(Sys.Date(),"-","_"), "_indv_analysis_2.xlsx"))
write.xlsx(indv_unweighted_results, paste0("Outputs/04_basic_analysis/", str_replace_all(Sys.Date(),"-","_"), "_indv_unweighted_analysis_1.xlsx"))

