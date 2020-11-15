# Working Analyze Numerics ------------------------------------------------

analyze_numerics<-function(df,aggregate_by=NULL,variables_to_analyze){
  
  df<-as_survey(df)
  if(!is.null((aggregate_by))){
    group_by_vars<-syms(aggregate_by)
    numerics_ready<-df %>% 
      filter(!is.na(group_by_vars)) %>% 
      group_by(!!!group_by_vars,.drop=F)}
  else{
    numerics_ready<-df
  }
  numerics_wide<-numerics_ready %>%
    summarise_at(
      .vars = variables_to_analyze,.funs = list(mean=~survey_mean(.x,na.rm=TRUE,vartype="ci"),n_unweighted=~unweighted(n()))
    )
    
  # numerics_wide<-numerics_ready %>%
  #   summarise_at(
  #     .vars = variables_to_analyze,.funs = ~survey_mean(.,na.rm=TRUE,vartype="ci"))
    
    
  # numerics_wide<-numerics_ready %>%
  #   summarise(
  #     across(variables_to_analyze,~survey_mean(.x,na.rm=TRUE,vartype="ci"))
  #   )
  # numerics_wide<-numerics_ready %>% select(variables_to_analyze) %>% 
  #   summarise(
  #     across(everything(),~survey_mean(.x,na.rm=TRUE,vartype="ci"))
  #   )
  if(!is.null(aggregate_by)){
      numerics_long<-numerics_wide %>% 
    pivot_longer(
      -aggregate_by,
      names_to = "indicator"
    )
  }else{
    numerics_long<-numerics_wide %>% 
      pivot_longer(
        cols=everything(),
        names_to = "indicator"
      )
  }
  numerics_long%>% 
    mutate(stat=case_when(str_detect(indicator,"_low$")~"mean_low",
                          str_detect(indicator,"_upp$")~"mean_upp",
                          str_detect(indicator,"n_unweighted$")~"n_unweighted",
                          TRUE~"mean"),
           indicator=str_replace_all(indicator,"_mean$|_mean_low$|_mean_upp$|_n_unweighted$","")) %>% 
    pivot_wider(names_from = stat ,values_from = value) %>% 
    mutate(option="1")
  
}



# Working Analyze Factors -------------------------------------------------

analyze_factors<-function(df,aggregate_by=NULL,variables_to_analyze){
  df<-as_survey(df)
  res<-list()
  if(is.null(aggregate_by)){
    variables_to_analyze %>% 
      map_dfr(~df %>% 
                filter(!is.na(!!sym(.x))) %>%
                group_by(!!sym(.x),.drop=F) %>% 
                summarise(
                  mean=survey_mean(na.rm=T,vartype="ci"),
                  n_unweighted= unweighted(n())
                ) %>% 
                mutate(indicator=.x) %>% 
                dplyr::select(indicator,option=.x,everything())
      )
  } else{
    aggregate_by<-syms(aggregate_by)
    
    variables_to_analyze %>% 
      map_dfr(~df %>% 
                filter(!is.na(!!sym(.x))) %>% #!is.na(aggregate_by)) %>%
                group_by(!!!(aggregate_by),!!sym(.x), .drop = F) %>% 
                summarise(
                  mean=survey_mean(na.rm=T,vartype="ci"),
                  n_unweighted= unweighted(n())
                ) %>% 
                mutate(indicator=.x) %>% 
                dplyr::select(!!!aggregate_by,indicator,option=.x,everything())
      )
  }
}
# analyze_factors(df = hhsvy,aggregate_by = "I.HH_CHAR.adlt_male_in_hh.INDVHH",variables_to_analyze = "I.HH_CHAR.atlst_one_birth_plc_home.INDVHH")

encode_sm_in_list_to_data_format<- function(df, variables_to_analyze, select_multiple_in_data){
  select_multiples_in_list_of_variables<-variables_to_analyze[which(variables_to_analyze%in%select_multiple_in_data)]
  list_of_variables_no_concatenated_select_multiple<-variables_to_analyze[which(variables_to_analyze%in%select_multiple_in_data==FALSE)]
  if(length(select_multiples_in_list_of_variables)>0){
    select_multiples_in_data_with_dot<-paste0(select_multiple_in_data,".")
    select_multiples_in_given_list_with_dot<-paste0(select_multiples_in_list_of_variables, ".")
    vars_selection_helper <- paste0("^(", paste(select_multiples_in_given_list_with_dot, collapse="|"), ")")
    
    select_multiple_logical_names<-select(df$variables, matches(vars_selection_helper)) %>%
      select(-ends_with("_other")) %>% colnames()
    
    list_of_variables<-unique(c(list_of_variables_no_concatenated_select_multiple,select_multiple_logical_names))
  }else{
    list_of_variables<-list_of_variables_no_concatenated_select_multiple
  }
}

mean_prop2020<- function(df, aggregate_by, variables_to_analyze){
  df<-as_survey(df)
  res<-list()
  
  select_multiple_in_data<-auto_detect_select_multiple(df$variables)
  list_of_variables<-encode_sm_in_list_to_data_format(df = df,variables_to_analyze = variables_to_analyze,select_multiple_in_data = select_multiple_in_data)
  
  all_vars_data<-df %>% 
    select(list_of_variables)
  numeric_vars<-all_vars_data %>% select_if(is.numeric) %>% colnames()
  factor_vars<-all_vars_data %>% select_if(~is.character(.)|is.factor(.)) %>% colnames()
  
  
  if(length(numeric_vars)>0){
    numerics_analyzed<-analyze_numerics(df, aggregate_by, numeric_vars)
    res$numerics<- numerics_analyzed}
  if(length(factor_vars)>0){
    
    factors_analyzed<-analyze_factors(df, aggregate_by, factor_vars)
    res$factors<- factors_analyzed
  }
  return(res)
  
}


