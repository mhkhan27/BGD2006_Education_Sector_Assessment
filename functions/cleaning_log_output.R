cleaning_log_output <- function(spotted_by,...){
  issue_dfs_all <- list(...)
  cleaning_log <- tribble(
    ~X_uuid,~indicator ,~reported_data, ~enumerator_id,~change_type,~value_in_raw_df,~new_value,~issue
  )

  return(bind_rows(cleaning_log,issue_dfs_all) %>% mutate(
    spotted_by =spotted_by
  ))
}
