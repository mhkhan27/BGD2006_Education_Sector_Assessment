library(dplyr)
library(tidyr)
library(stringr)

cleaning_log_output <- function(spotted_by ="BGD_Data_Unit",...){
  issue_dfs_all <- list(...)
  return(bind_rows(issue_dfs_all) %>% mutate(
    spotted_by =spotted_by,
    new_value = NA_real_,
    change_type = NA_real_
    ))
}


a<- data.frame(id= c("a2","b1"),
               questions= c("q1","q2"),
               value = c(1,2)
               )
b<- data.frame(id= c("aa","bb"),
               questions= c("qq","qq2"),
               value = c(11,21)
)
c<- data.frame(id= c("aa2","bb1"),
               value = c(10,22)
)

cleaning_log <- cleaning_log_output()
