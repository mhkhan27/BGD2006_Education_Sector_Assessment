
library(dplyr)
raw_population <- read.csv("DAP/july2019_unhcr_pop_numbers.csv",na.strings = c(""," "),stringsAsFactors = F,skip = 1)[,1:16]

pop_2019 <- raw_population %>% dplyr::filter(grepl("Total",Camp)) %>% 
  dplyr::filter(!grepl("camp Total",Camp))

pop_2019$Camp <- str_replace_all(pop_2019$Camp,"Total","") %>% trimws()

pop_2019 <- pop_2019 %>% dplyr::filter(Camp != "")

names(pop_2019) <- str_replace_all(names(pop_2019),"X","age_")

age_1_to_17 <- c ("age_1.4.Children.Female", "age_1.4.Children.Male", "age_5.11.Children.Female", 
                  "age_5.11.Children.Male", "age_12.17.Children.Female", "age_12.17.Children.Male")


  pop_2019$age_1.4.Children.Female <- pop_2019$age_1.4.Children.Female %>% as.numeric()
  pop_2019$age_1.4.Children.Male <- pop_2019$age_1.4.Children.Male %>% as.numeric()
  pop_2019$age_5.11.Children.Female <- pop_2019$age_5.11.Children.Female %>% as.numeric()
  pop_2019$age_5.11.Children.Male <- pop_2019$age_5.11.Children.Male %>% as.numeric()
  pop_2019$age_12.17.Children.Female <- pop_2019$age_12.17.Children.Female %>% as.numeric()
  pop_2019$age_12.17.Children.Male <- pop_2019$age_12.17.Children.Male %>% as.numeric()
  


population_2019 <- pop_2019 %>% mutate(
  age_1_to_17_total =rowSums(pop_2019[age_1_to_17],na.rm = T)
) %>% dplyr::select(Camp,age_1_to_17_total,Total.Individuals,Total.Families)

population_2019$Camp <- snakecase::to_snake_case(population_2019$Camp)
population_2019$Camp<- str_replace_all(population_2019$Camp,"extension","ext")
remove_zero <- c("camp_01", "camp_01_e", "camp_01_w", "camp_03", "camp_04", "camp_05", 
                 "camp_08_w", "camp_09","camp_02_e","camp_02_w","camp_08_e","camp_07","camp_06")

population_2019$Camp <- if_else(population_2019$Camp %in% remove_zero,
                                 str_remove_all(population_2019$Camp, "0"),population_2019$Camp,NULL)



rm(list=c("pop_2019","raw_population","age_1_to_17"))
