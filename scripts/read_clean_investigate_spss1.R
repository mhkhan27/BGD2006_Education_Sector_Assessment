library(tidyverse)
library(lubridate)
df<-haven::read_sav("inputs/Final dataset-ASER plus - 19082019.sav")
df2<-df %>% 
  mutate(Date= as_date(Date))

df2 %>% 
  filter(is.na(Date)) %>% nrow()
# these seem like more realistic dates
df2 %>% 
  filter(!is.na(Date)) %>% 
  filter(Date>as_date("2016-01-01"),Date<Sys.Date()) %>% 
  pull(Date) %>% range()

df2 %>% 
  filter(!is.na(Date)) %>% 
  filter(Date>as_date("2016-01-01"),Date<Sys.Date()) %>% 
  filter(Date>as_date("2019-01-01")) %>% 
  select(Date) %>% print(n=nrow(.))

df2 %>% 
  filter(!is.na(Date)) %>% 
  filter(Date>as_date("2016-01-01"),Date<Sys.Date()) %>% 
  # filter(Date>as_date("2019-01-01")) %>% 
  group_by(centername) %>% 
  select(Date,teacherhost_1) 

df3<-df2 %>% mutate_all(~trimws(.)) %>% 
  mutate_if(is.character, funs(na_if(., ""))) 

skimr::skim(df3)
