rm(list = ls())

library(dplyr)
library(srvyr)
library(survey)
library(readr)
library(stringr)
library(jtools)
library(tidyverse)
library(tidymodels)


population<- c("host","refugee")[2]

dap<-read_csv("DAP/Significance_testing_DAP_TR_YPA_revisions -new.csv",col_types = cols())

dap<-dap %>% 
  janitor::clean_names() %>% 
  mutate(population=str_replace_all(population, c("Ref"="refugee","HC"="host"))) %>%
  rename(analysis_pop="population")


dap <- dap  %>% filter(analysis_pop %in% c("Both",population) )
dap_hh<- dap %>% filter(analysis_level=="HH") %>% filter(remove==0)



if (population=="refugee"){
  hh<-read.csv("inputs/refugee/significance_testing/2020_hh_refugee_with_composites.csv",
               stringsAsFactors = F,na.strings=c(""," ","NA",NA))
  
  hh<-hh %>% 
    mutate(
      I.HH_CHAR.education_level_similar_as_2019.HH= na_if(I.HH_CHAR.education_level_similar_as_2019.HH,"dont_know")
    )
  
  
  ind<-read.csv("inputs/refugee/significance_testing/2020_indiv_refugee_with_composites.csv",
                stringsAsFactors = F,na.strings=c(""," ","NA",NA))
}

if (population=="host"){
  hh<-read.csv("inputs/host/significance_testing/2020_hh_host_with_composites.csv",
               stringsAsFactors = F,na.strings=c(""," ","NA",NA))
  
  weights <- read.csv("outputs/host/weights/host_weights.csv",stringsAsFactors = F,na.strings=c(""," ","NA",NA))
  
  ind<-read.csv("inputs/refugee/significance_testing/2020_indiv_refugee_with_composites.csv",
                stringsAsFactors = F,na.strings=c(""," ","NA",NA))
}

#list that are not in dataset
dap_hh$new_dependent_variable_name[!dap_hh$new_dependent_variable_name %in% colnames(hh)]

#check
dap_hh$new_dependent_variable_name %in% colnames(hh)

dap_hh<-dap_hh %>% 
  filter(new_dependent_variable_name %in% colnames(hh),
         independent_variable_name %in% colnames(hh))



hh<-hh %>% 
  mutate_if(is.character,as.factor)

if(population=="refugee"){
  hhsvy<-as_survey(hh, strata=camp_name, weights=survey_weight)
}
if(population=="host"){
  hhsvy<-as_survey(hh, strata=upazilla_name, weights=survey_weight)
}

mod_res<-list()
for(i in 1:nrow(dap_hh)){
  ind_temp<-dap_hh$independent_variable_name[i]
  dep_temp<-dap_hh$new_dependent_variable_name[i]
  
  
  #overall chisq
  chisq_res<-svychisq(formula = formula(paste0("~",dep_temp,"+",ind_temp)),design=hhsvy) 
  # just messing with some tidy models functions
  chisq_res_tidy<-chisq_res%>%
    tidy() %>% 
    mutate(ind_var_name=ind_temp,
           dep_var_name=dep_temp) %>% 
    janitor::clean_names()
  
  mean_ci = svyby(formula(paste0("~",dep_temp)), formula(paste0("~",ind_temp)), hhsvy, svymean, vartype="ci", na.rm = T)
  unwt_n = svyby(formula(paste0("~",dep_temp)), formula(paste0("~",ind_temp)), hhsvy, unwtd.count, keep.var=F)
  
  row.names(mean_ci) <- NULL
  row.names(unwt_n) <- NULL
  
  # mean_ci %>%  pivot_longer(cols=c("shelter_paid.payment_of_cash", "ci_l", "ci_u"),
  #                         names_to = "name",values_to = "value")
  
 mean_ci <- mean_ci %>% mutate(ind_var_name=ind_temp,
                               dep_var_name =dep_temp) %>%  rename("ind_var_name_value"=ind_temp,
                                                      "mean"=dep_temp)
 
 unwt_n <-unwt_n %>% mutate(ind_var_name=ind_temp,
                            dep_var_name =dep_temp) %>%  rename("ind_var_name_value"=ind_temp,
                                                       "n_unweighted"="statistic")
 mean_and_weights <- mean_ci %>% left_join(unwt_n,by = c("ind_var_name_value", "ind_var_name", "dep_var_name"))
  
 chisq_res_tidy_with_mean_weights <- mean_and_weights %>% left_join(chisq_res_tidy,by = c("ind_var_name", "dep_var_name"))
  mod_res[[i]]<-chisq_res_tidy_with_mean_weights
}

all_res_data <- do.call("bind_rows",mod_res)
