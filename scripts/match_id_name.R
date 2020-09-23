rm(list = ls())
library(dplyr)
library(haven)
library(snakecase)
library(tidyr)
library(stringr)
library(qdapRegex)
source("scripts/population_data_cleaning.R")

# read_csv ----------------------------------------------------------------
df_2019 <- read.csv("inputs/compiled.list_2019.csv",na.strings = c(""," ","N/A"),stringsAsFactors = F)
df_2018 <-haven::read_sav("inputs/Final dataset-ASER plus - 19082019.sav") %>% filter(!is.na(address)) %>% filter (address!="")
name_fix <- read.csv("DAP/fix_name.csv",na.strings = c(""," "),stringsAsFactors = F)
#  data_cleaning_2018 data -----------------------------------------------------------------

df_2018 <- df_2018 %>%
  mutate(camp_loc= snakecase::to_snake_case(address),
         address_raw =address,
         name_orientation= case_when(str_detect(camp_loc, "^c")~"camp_fist",
                                     str_detect(camp_loc,"^b")~ "block_first",
                                     TRUE~ "unknown")) %>% as.data.frame() %>% mutate(
    block_first_last_c_length = unlist(lapply(gregexpr(pattern ='c',camp_loc),max)),
    camp_first_last_b_length = unlist(lapply(gregexpr(pattern ='b',camp_loc),min)),
    camp_name = if_else(name_orientation == "block_first", substr(camp_loc, block_first_last_c_length, length(camp_loc)),
                        if_else(name_orientation == "camp_fist", substr(camp_loc, 0,camp_first_last_b_length-1),NULL, NULL))
)

df_2018<-df_2018 %>% separate(address_raw,c("address_raw_1", "address_raw_2","address_raw_3"),sep = c(","))


make_blank <- "uu_15_|_c_zone_c_17|_d_5|rr_zone_|_zone_a|_c_zone_c_3|_c_zon_c_18|	leda_|_b_c_10|
                  _c_zon_c_15|_d_4|jamtoli_|_mudasora|_modassara|
                  c_1_|_a_13|_a_14|_a_15|_a_16|_a_21|_a_36|a_65_|
                  _h_06|_h_21|_i_10|_i_11|_i_13|_i_18|_i_20|
                  _c_zone_c_5|_czone_c_16|d_1_|d_3_|d_4_|d_4_2_|d_5_|
                  _i_15|_a_29|_i_2|_d_4|_e_3|_zone_a|leda_|_c_zon_c_15|
              _h_06|_h_09|_h_29|_13_a|_i_15|_jc_2|_su|_c_zon_c_15|_c_zone_c_5|
              _leda_zone|_zone_a|_zone_c|_pp_7|_g_4|_zone_pp|_ai|_h_06|
              _h_06|d_2_3_|dd_11_|g_2_|leda_|pp_zone_|_b_c_11|_b_c_12|_b_c_14|_b_c_15|_b_c_16|_b_c_41|_b_c_42|_b_c_43|_b_c_44|_b_c_17|_b_c_18|_b_c_19|_b_c_20|_b_c_21|_b_c_22|_b_c_45|_b_c_23|_b_c_24|_b_c_25|_b_c_26|_b_c_27|_b_c_28|_b_c_13|_b_c_29|_b_c_30|_b_c_31|_b_c_32|_b_c_33|_b_c_34|_b_c_35|_b_c_36|_b_c_37|_b_c_38|_b_c_39|_b_c_40"


make_blank <- make_blank %>% str_replace_all("\n","") %>% trimws()
make_blank2<- "_a_66|_si|_zone"

df_2018<-df_2018 %>% mutate(
  camp_name = if_else(camp_name == ""|camp_name == " ",NA_character_,camp_name,NULL),
  camp_name_separate = if_else(grepl("Camp",address_raw_1),address_raw_1,
                      if_else(grepl("Camp",address_raw_2),address_raw_2,
                              if_else(grepl("Camp",address_raw_3),address_raw_3,address))) %>% trimws(),
  camp_name_new= if_else(is.na(camp_name),camp_name_separate,camp_name,NULL) %>% snakecase::to_snake_case(),
  camp_name_new = str_replace_all(camp_name_new,pattern = make_blank,replacement = "") %>% trimws(),
  camp_name_new = str_replace_all(camp_name_new,pattern = c("comp|csmp|ca_mp|camo"),replacement = "camp") %>% trimws(),
  camp_name_new = str_replace_all(camp_name_new,pattern = make_blank2,replacement = "") %>% trimws(),
  
  
)

df_2018$camp_name_new <- df_2018$camp_name_new %>% str_replace_all("c_","camp_")

df_2018$camp_name_new <- df_2018$camp_name_new %>% str_replace_all(c("2_camp_15"="camp_15",
                                                                     "8_w_camp"="camp_8_w",
                                                                     "cam_18"="camp_18",
                                                                     "camp_1_camp_15"="camp_15",
                                                                     "camp_1_east|east_camp_1" ="camp_1_e",
                                                                     "camp_2_e_2"="camp_2_e",
                                                                     "camp_20_exts_1"="camp_20_ext",
                                                                     "camp_081"="camp_8",
                                                                     "camp_2_ext" = "camp_2"
                                                                     ))
remove_zero <- c("camp_01", "camp_01_e", "camp_01_w", "camp_03", "camp_04", "camp_05", 
                 "camp_08_w", "camp_09","camp_02_e","camp_02_w","camp_08_e","camp_07","camp_06")

df_2018$camp_name_new <- if_else(df_2018$camp_name_new %in% remove_zero,
                                 str_remove_all(df_2018$camp_name_new, "0"),df_2018$camp_name_new,NULL)

df_2018$camp_name_new <- if_else(df_2018$address =="Lada,Camp,24,C-10",
                                 "camp_24",df_2018$camp_name_new,NULL)
                                                              

to_remove <- c("b", "balukhali_rr_zone", "c", "ca", "camp", "camp_0",
               "camp_1", "camp_2_ext","balukhali_rr","camp_2",
                "camp_8","camp_h_ext", "cb", "ck", "ck_40", "ck_8", "ck_a",
               "ck_b", "ck_d", "ck_d_3", "ck_e", "ck_e_1", "ck_f_4", "ck_g",
               "ck_h", "ck_h_13", "ck_pp_12")
# df_2019_join <- df_2019 %>% select(camp_name_new,address) %>% unique() %>% filter(camp_name_new %in% to_remove)

df_2018$camp_name_new <- if_else(df_2018$camp_name_new %in% to_remove,df_2018$address,df_2018$camp_name_new)

remove_from_final_dataset <- c("camp_loc", 
                               "address_raw_1", "address_raw_2", "address_raw_3", "name_orientation", 
                               "block_first_last_c_length", "camp_first_last_b_length", "camp_name", 
                               "camp_name_separate")


dataset_2018_cleaned <- df_2018 %>%  left_join(name_fix,by =c("camp_name_new"="old_name")) %>% 
  select(-remove_from_final_dataset) %>% filter(fix_name !="need_to_remove") 

dataset_2018_cleaned$Camp <- dataset_2018_cleaned$fix_name 

dataset_2018_cleaned <- dataset_2018_cleaned %>% select(-c("fix_name","camp_name_new"))



# data_cleaning_2019 ------------------------------------------------------

df_2019$Camp <-snakecase::to_snake_case(df_2019$Camp) 

df_2019$Camp <- if_else(df_2019$Camp %in% remove_zero,
                        str_remove_all(df_2019$Camp, "0"),df_2019$Camp,NULL)

dataset_2019_cleaned<- df_2019 %>%  filter(Level.of.the.student !="ECD",!is.na(Level.of.the.student))

# matching -----------------------------------------------------------------
dataset_2018_cleaned$childname <-snakecase::to_snake_case(dataset_2018_cleaned$childname)
dataset_2019_cleaned$Name.of.the.student <-snakecase::to_snake_case(dataset_2019_cleaned$Name.of.the.student)

match.df_2018_cleaned <- dataset_2018_cleaned %>% mutate(
  student_name_and_facility_id = paste0(childname,"_",centerid),
  level_of_the_student_2018 = aggregated,
  camp_2018 =Camp
)  %>% select(student_name_and_facility_id,level_of_the_student_2018)

match.df_2019_cleaned <- dataset_2019_cleaned %>% mutate(
  student_name_and_facility_id = paste0(Name.of.the.student,"_",Facility.ID),
  level_of_the_student_2019 = if_else(Level.of.the.student == "level1",1,
                                      if_else(Level.of.the.student == "level2",2,
                                              if_else(Level.of.the.student == "level3",3,
                                                      if_else(Level.of.the.student == "level4",4,999999,NULL)))),
  
  camp_2019 =Camp
) %>% select(student_name_and_facility_id,camp_2019,level_of_the_student_2019)

match.df_2018_cleaned$student_name_and_facility_id %in% match.df_2019_cleaned$student_name_and_facility_id %>% AMR::freq()
match.df_2019_cleaned$student_name_and_facility_id %in% match.df_2018_cleaned$student_name_and_facility_id %>% AMR::freq()

matched_df_full <- match.df_2018_cleaned %>% inner_join(match.df_2019_cleaned) %>% mutate(
  change_value = level_of_the_student_2019-level_of_the_student_2018,
  change_lable = if_else(change_value >0 ,"increased",
                         if_else(change_value <0 ,"decreased",
                                 if_else(change_value == 0 ,"stayed_same","error",NULL)))
)

matched_df_full$change_lable %>% AMR::freq()

percentage_change_per_camp <- matched_df_full %>% dplyr::group_by(camp_2019) %>% dplyr::summarise(
  total = n(),
  increased = sum(change_value >0,na.rm=T),
  decreased = sum(change_value <0,na.rm=T),
  stayed_same = sum(change_value ==0,na.rm=T),
  percent_increased = increased/total*100,
  percent_decreased = decreased/total*100,
  percent_stayed_same = stayed_same/total*100,
  change_val = increased-decreased,
  change_lable = if_else(change_val >0 ,"increased",
                         if_else(change_val <0 ,"decreased",
                                 if_else(change_val == 0 ,"stayed_same","error",NULL)))
)

aaa <- matched_df_full %>% filter(change_value!=0,camp_2019 == "camp_10")
