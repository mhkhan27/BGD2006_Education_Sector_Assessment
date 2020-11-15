rm(list = ls())

library(butteR)
library(survey)
library(srvyr)
library(jtools)
#library(purrr)
library(tidyr)
library(tidyverse)
library(tidymodels)

source("functions/reach_style.R")
source("functions/mean_prop2020.R")
source("functions/svychisq_posthoc_test.R")

dap_secondary_significant <- read.csv("DAP/significant_test/secondary_data/Significance_testing.csv",
                                      na.strings = c("", " ","N/A","NA"),stringsAsFactors = F)

data <-read.csv("outputs/significant_testing/secondary/data_for_significance_testing/cleaned_data_for_significance_testing.csv",
                na.strings = c("", " ","N/A","NA"),stringsAsFactors = F) %>% mutate(
                  i.age_grp_gender = paste0(age_group,"_",sex_2018),
                  i.childage_5_13 = as.factor(if_else(childage == "14", NA_character_,
                                                      as.character(childage),NULL)),
                  i.teacher_edu_level_mymnr_ex_primary = as.factor(if_else(teacher_edu_level_mymnr == "primary", NA_character_,
                                                                 as.character(teacher_edu_level_mymnr),NULL)))

dap_hh <- dap_secondary_significant #%>% 
  #filter(Test == "Chi square with post hoc") %>% filter(adjustment_to_data == "no")

data<-data %>% 
  mutate_if(is.character,as.factor)
dep_vars_with_over_one_val<-data %>% select(unique(dap_hh$new_dependent_variable)) %>% map(~length(levels(.))) %>% keep(.>1) %>% names()
dep_vars_with_only_one_level<-data %>% select(unique(dap_hh$new_dependent_variable)) %>% select_if(is.factor) %>%  map(~length(levels(.))) %>% keep(.<=1) %>% names()

dap_hh<-dap_hh %>% 
  dplyr::filter(new_dependent_variable %in% dep_vars_with_over_one_val)


dap_hh$new_dependent_variable %in% names(data)

df_svy <- as_survey(data)



# explot ------------------------------------------------------------------

# data<- data %>% dplyr::filter(!is.na(change_out_of_level_1)) %>% filter(!is.na(Implementing.partner))
# 
# pt <- PivotTable$new()
# pt$addData(data)
# pt$addColumnDataGroups(independent_var)
# pt$addRowDataGroups(dependent_var)
# pt$defineCalculation(calculationName="independent_var", summariseExpression="n()")
# pt$renderPivot()
# df_mat<-pt$asDataMatrix()
# 
# ##df_mat<- df_mat %>% as.matrix()
# a <- chisq.posthoc.test::chisq.posthoc.test(x =df_mat,method = "bonferroni")

################################################
mod_res<-list()
res2<-list()
plot_list<-list()
result_df_list<-list()
posthoc_list<-list()
corplot_list<-list()
for(i in 1:nrow(dap_hh)){
  #ind_var_label<-dap_hh$household_characteristic[i]
  #dep_var_label<-dap_hh$indicator_of_interest[i]
  ind_temp<-dap_hh$new_indpendent_variable_name[i]
  dep_temp<-dap_hh$new_dependent_variable[i]
  # print(paste0(ind_temp," by ", dep_temp))
  
  
  # new function i adapted, from non weighted version. Function allows for pvalue adjustment, in theory we should
  # probably apply bonferonni adjustment, but I am considering this more of an exploratory step just to help understand 
  # what is driving the significance of the overall chisq tests.
  ph_df<-svychisq_posthoc_test(design = df_svy,
                               independent_var = ind_temp,
                               dependent_var = dep_temp,
                               method = "none") %>%
    #filter(value=="p_value") %>%
    mutate_all(~as.character(.)) %>% 
    pivot_longer(
      cols = starts_with(dep_temp),
      names_to = "temporary_column",
      values_to = "rank",
      values_drop_na = F
    ) %>% 
    separate(col=temporary_column,sep="\\*(?!.*\\*)",into = c("dep_var","indep_var_choice")) %>% 
    select(ind_var_name,indep_var_choice,dep_var,everything())
  # print("posthoc done") # debugging step
  
  #overall chisq
  chisq_res<-svychisq(formula = formula(paste0("~",dep_temp,"+",ind_temp)),design=df_svy) 
  # just messing with some tidy models functions
  chisq_res_tidy<-chisq_res%>%
    tidy() %>% 
    mutate(ind_var=ind_temp,
           dep_var=dep_temp) %>% 
    janitor::clean_names()
  mod_res[[i]]<-chisq_res_tidy
  
  # extract p value- will use this to plot significant results later
  pval<- chisq_res_tidy$p_value
  
  # these plots could be cool down the road, they help show what is driving the differences of the chisq test as well
  # contrib<-round(100*chisq_res$residuals^2/chisq_res$statistic,3)
  # if( !is.nan(sum(contrib))){
  # corplot_list[[paste0(ind_temp,"_",dep_temp)]]<-corrplot(contrib, is.cor = FALSE,title = paste0(dep_temp," by ",ind_temp))}
  # print("corplotdone")
  ci_df<-mean_prop2020(df = df_svy,aggregate_by = ind_temp, 
                       variables_to_analyze = dep_temp) %>% bind_rows() #%>% 
  
  #tidy up data and join dissagregated percent means with chisq overall results
  ci_df<-ci_df %>%  mutate(
    ind_var_name= colnames(.)[1],
    
  ) %>%
    rename(ind_var_name_value=ind_temp) %>%
    select(ind_var_name,everything()) %>% 
    left_join(chisq_res_tidy, by=c("ind_var_name"="ind_var", "indicator"="dep_var"))# %>% 
    #mutate(ind_var_label=ind_var_label,
      #     dep_var_label=dep_var_label)
  
  ci_df$ind_var_name_value <- ci_df$ind_var_name_value %>% as.factor()
    ph_df$indep_var_choice <-  ph_df$indep_var_choice %>% as.factor()
  #join posthoc tests with the rest of the results
  ci_df<-ci_df %>% left_join(ph_df,
                             by=c("ind_var_name"="ind_var_name",
                                  "ind_var_name_value"="indep_var_choice",
                                  "indicator"="dep_var",
                                  "option"="dep_var_option"))
  
  # save posthoc  tes results independently (not necessary)
  posthoc_list[[i]]<-ph_df
  # put temporary result df into a list
  result_df_list[[i]]<- ci_df
  
  # ill print these out later  
  # plot_list<-ci_df %>% 
  #   ggplot(aes(x=option,y=mean, color=ind_var_name_value))+
  #   geom_point()+scale_y_continuous(labels=scales::percent)+
  #   geom_errorbar(aes(ymin=mean_low,ymax=mean_upp))+
  #   ggtitle(paste0(dep_temp, " by ", ind_temp," p value= ", pval))
  # 
  
  
}
all_res_data<-bind_rows(result_df_list)

all_res_data_only_all2 <- all_res_data %>% filter(!is.na(value))

all_res_data_only_all_pivot_wider <- all_res_data %>%  
  pivot_wider(names_from = "value",values_from = "rank", names_prefix = "rank_") %>% dplyr::select(-rank_NA)

write.csv(all_res_data_only_all_pivot_wider,"outputs/significant_testing/secondary/test_result/significance_result_testing.csv")

# # add some signicance symbols based on overall chisq and post-hoc p values
# chi_test_result_with_symbl<-all_res_data_only_all_pivot_wider %>%
#   mutate(sig_tf= ifelse(p_value<=0.05,T,F),
#          sig_symbol=case_when(p_value<=0.0001~ "****",
#                               p_value<=0.001~ "***",
#                               p_value<=0.01~ "**",
#                               p_value<=0.05~ "*",
#                               TRUE~ NA_character_),
#          pval_pairwise= parse_number(rank),
#          pairwise_sig = case_when(pval_pairwise<=0.0001~ "****",
#                                   pval_pairwise<=0.001~ "***",
#                                   pval_pairwise<=0.01~ "**",
#                                   pval_pairwise<=0.05~ "*",
#                                   TRUE~ NA_character_)) 
# 
