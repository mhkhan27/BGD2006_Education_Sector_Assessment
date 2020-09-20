rm(list = ls())
library(dplyr)
library(readxl)
library(purrr)
library(stringr)
library(openxlsx)
library(data.table)
require(haven)
# path_lication -----------------------------------------------------------

path_file_full = list.files("inputs/raw_data/ASER_DATA_excel/",full.names = T)
a<- read_sav("inputs/Final dataset-ASER plus - 19082019.sav")

# path --------------------------------------------------------------------
problematric_sheet <- list()
combine_data <- list()
combine_all_col_not_same <- list()
for (x in path_file_full) {
path <- x

# sheet_name --------------------------------------------------------------

sheet_name <- getSheetNames(path) 
sheet_name <- sheet_name[!sheet_name %in%c("Compilation ","Compilation")]

standard_excle <- read_xlsx(path,sheet =sheet_name[1],col_names = F)
strandard_col_length <- names(standard_excle) %>% length()
standard_row_number <- which(grepl("SL", standard_excle$...1))
colnm<-paste0(standard_excle[standard_row_number,] ,"_",standard_excle[standard_row_number+1,],"_",standard_excle[standard_row_number+2,]) %>% str_replace_all("_NA_","")  %>% 
  str_replace_all("NA_","") %>% str_replace_all("_NA","") %>% str_replace_all("NA","") %>% str_replace_all(" ","_")

all_excel_2 <- read_xlsx(path,sheet =sheet_name[1],col_names = colnm)

standard_colname <- c(names(all_excel_2),"sheet_name")

sheet_not_same_length_as_standard <- list()
for(z in sheet_name) {
  rw_excel_for_sheet <- read_xlsx(path,sheet =z,col_names = F)
  sheet_not_same_length_as_standard[[z]] <- data.frame(name = paste0(z),length =length(names(rw_excel_for_sheet)))
}

sheet_not_same_length_as_standard2 <- do.call("rbind",sheet_not_same_length_as_standard)

problematric_sheet[[x]] <- sheet_not_same_length_as_standard2 %>% filter(length != strandard_col_length) %>% mutate(
  file_name = x
)
valid_sheet<- sheet_not_same_length_as_standard2 %>% filter(length == strandard_col_length)
sheet_name_vaid <- valid_sheet$name %>% dput()

df <- list()
col_not_same <- list()
for (i in sheet_name_vaid){
  rw_excel <- read_xlsx(path,sheet =i,col_names = F)
  row_number <- which(grepl("SL", rw_excel$...1)) %>% max()
  colnm1<-paste0(rw_excel[row_number,] ,"_",rw_excel[row_number+1,],"_",rw_excel[row_number+2,]) %>% str_replace_all("_NA_","")  %>% 
    str_replace_all("NA_","") %>% str_replace_all("_NA","") %>% str_replace_all("NA","") %>% str_replace_all(" ","_")


df_1<- read_xlsx(path,sheet = i,col_names =colnm1) %>% mutate(
  sheet_name = paste0(i)) %>% filter(!is.na(Name_of_center)) %>% mutate_all(as.character) %>% filter(!is.na(SL),SL != "SL")


all_in_standard_sheet <- if_else(all(names(df_1) %in% standard_colname),T,F)


if(all_in_standard_sheet == T) {
df[[i]] <- df_1[standard_colname]
}

if(all_in_standard_sheet == F) {
  col_not_same[[i]]  <- data.frame(
    file_name = x,
    sheet_name = i
  )
}

}
combine_all_col_not_same[[x]] <- do.call("rbind",col_not_same)
combine_data [[x]] <- do.call("rbind",df) %>% mutate(
  file_location = x
)
}#1st for
problematric_sheet_col_length <- do.call("rbind",problematric_sheet)
problematric_sheet_col_name <- do.call("rbind",combine_all_col_not_same)

# names(combine_data) <- rep_len("x",length(combine_data))

combine_data_all <- do.call("bind_rows",combine_data) %>% mutate(
  Name_of_the_Child=  if_else(is.na(Name_of_the_Child),Name_of_the_child,Name_of_the_Child)
) %>% select(-Name_of_the_child)

write.csv(combine_data_all,"outputs/merge_data_all_similar.csv")
write.csv(problematric_sheet_col_name,"outputs/problem_in_col_name.csv")
write.csv(problematric_sheet_col_length,"outputs/problem_in_col_length.csv")

# a <- combine_data_all  %>% filter(SL =="1") %>% select(SL,sheet_name)
# 
# aa <- combine_data_all %>% group_by(combine_data_all$sheet_name,combine_data_all$file_location) %>% summarise(
#   n =n()
# )
# aaaa <- data.frame(
# shhet_name = aa$`combine_data_all$sheet_name`,
# tf = aa$`combine_data_all$sheet_name` %in% a$sheet_name ) %>% filter(tf ==F)
# 
# combine_data_all2 <-  combine_data_all %>% filter(sheet_name =="BLC-410")
