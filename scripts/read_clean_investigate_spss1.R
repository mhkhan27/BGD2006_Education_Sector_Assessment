library(tidyverse)
library(lubridate)
df<-haven::read_sav("inputs/Final dataset-ASER plus - 19082019.sav")
df2<-df %>% 
  mutate(Date= as_date(Date))

df2 %>% nrow()

# how many rows with NA date values
df2 %>% 
  filter(is.na(Date)) %>% nrow()

# how many rows with dates
df2 %>% 
  filter(!is.na(Date)) %>% nrow()

#how many unique centers
df2$centerid %>% unique() %>% length()

# UNICEF - Education notes
# aggregated score 2018... required entire test
# how do you have the levels without 

#apparently they want us to compare these aggregated scores to 2019 agg scores
df2 %>% 
  ggplot(aes(x=aggregated))+geom_histogram()



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




df3$teacherhost_1 %>% unique()
df3 %>% colnames()
df4<-df3 %>% group_by(centerid) %>% 
  mutate(teach_host= teacherhost_1[!is.na(teacherhost_1)][1L],
         teach_myan=teachermyan_1[!is.na(teachermyan_1)][1L]) %>% 
  select(SL:shift, teach_host, teacherhost_1:teacherhost_4, teach_myan,everything())


df4 %>% head() %>% View()
