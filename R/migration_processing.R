setwd("~/R_project/smoking prevalence projection/R")

library(tidyverse)
library(writexl)



############################################
##internal in migration 
Internal_in_females <- read_csv("~/R_project/smoking prevalence projection/2022 SNPP Internal in females.csv")
Internal_in_males <- read_csv("~/R_project/smoking prevalence projection/2022 SNPP Internal in males.csv")

#male
Internal_in_males = Internal_in_males %>% 
  filter(AREA_NAME == "Birmingham") %>% 
  filter(AGE_GROUP != "All ages") %>% 
  filter(AGE_GROUP != "90 and over") %>% 
  rbind(Internal_in_males %>% 
          filter(AREA_NAME == "Birmingham") %>% 
  filter(AGE_GROUP == "90 and over") %>% 
  slice(rep(1, 11)) %>%                      # repeat the row 11 times
  mutate(AGE_GROUP = as.character(90:100)) %>% 
  mutate(across(`2023`:`2047`, ~ .x / 11))) %>%   # divide each year's value equally
  pivot_longer(cols = c(-AREA_CODE, -AREA_NAME,-COMPONENT,-SEX,-AGE_GROUP),
               names_to = "year",
               values_to = "in_") %>% 
  mutate(SEX = "Men")
  


#female

Internal_in_females = Internal_in_females %>% 
  filter(AREA_NAME == "Birmingham") %>% 
  filter(AGE_GROUP != "All ages") %>% 
  filter(AGE_GROUP != "90 and over") %>% 
  rbind(Internal_in_females %>% 
          filter(AREA_NAME == "Birmingham") %>% 
          filter(AGE_GROUP == "90 and over") %>% 
          slice(rep(1, 11)) %>%                      # repeat the row 11 times
          mutate(AGE_GROUP = as.character(90:100)) %>% 
          mutate(across(`2023`:`2047`, ~ .x / 11))) %>%   # divide each year's value equally
  pivot_longer(cols = c(-AREA_CODE, -AREA_NAME,-COMPONENT,-SEX,-AGE_GROUP),
               names_to = "year",
               values_to = "in_") %>% 
  mutate(SEX = "Women")



##internal out migration 


Internal_out_females <- read_csv("~/R_project/smoking prevalence projection/2022 SNPP Internal out females.csv")
Internal_out_males <- read_csv("~/R_project/smoking prevalence projection/2022 SNPP Internal out males.csv")



Internal_out_males = Internal_out_males %>% 
  filter(AREA_NAME == "Birmingham") %>% 
  filter(AGE_GROUP != "All ages") %>% 
  filter(AGE_GROUP != "90 and over") %>% 
  rbind(Internal_out_males %>% 
          filter(AREA_NAME == "Birmingham") %>% 
          filter(AGE_GROUP == "90 and over") %>% 
          slice(rep(1, 11)) %>%                      # repeat the row 11 times
          mutate(AGE_GROUP = as.character(90:100)) %>% 
          mutate(across(`2023`:`2047`, ~ .x / 11))) %>%   # divide each year's value equally
  pivot_longer(cols = c(-AREA_CODE, -AREA_NAME,-COMPONENT,-SEX,-AGE_GROUP),
               names_to = "year",
               values_to = "out") %>% 
  mutate(SEX = "Men")







#female
Internal_out_females = Internal_out_females %>% 
  filter(AREA_NAME == "Birmingham") %>% 
  filter(AGE_GROUP != "All ages") %>% 
  filter(AGE_GROUP != "90 and over") %>% 
  rbind(Internal_out_females %>% 
          filter(AREA_NAME == "Birmingham") %>% 
          filter(AGE_GROUP == "90 and over") %>% 
          slice(rep(1, 11)) %>%                      # repeat the row 11 times
          mutate(AGE_GROUP = as.character(90:100)) %>% 
          mutate(across(`2023`:`2047`, ~ .x / 11))) %>%   # divide each year's value equally
  pivot_longer(cols = c(-AREA_CODE, -AREA_NAME,-COMPONENT,-SEX,-AGE_GROUP),
               names_to = "year",
               values_to = "out") %>% 
  mutate(SEX = "Women")



######################################
#calculate net internal migration 

net_internal_migration_female = Internal_out_females %>%
  select(-COMPONENT) %>%
  left_join(
    Internal_in_females %>% select(-COMPONENT),
    by = c("AREA_CODE","AREA_NAME","SEX","AGE_GROUP","year")
  ) %>%
  rename(out = out, in_ = in_) %>%
  mutate(net_internal = in_ - out)



net_internal_migration_male = Internal_out_males %>%
  select(-COMPONENT) %>%
  left_join(
    Internal_in_males %>% select(-COMPONENT),
    by = c("AREA_CODE","AREA_NAME","SEX","AGE_GROUP","year")
  ) %>%
  rename(out = out, in_ = in_) %>%
  mutate(net_internal = in_ - out)

############################################################################################
############################################################################################
############################################################################################
#cross boarder migration 
Cross_border_in_females <- read_csv("~/R_project/smoking prevalence projection/2022 SNPP Cross border in females.csv")

Cross_border_in_males <- read_csv("~/R_project/smoking prevalence projection/2022 SNPP Cross border in males.csv")



#male
Cross_border_in_males = Cross_border_in_males %>% 
  filter(AREA_NAME == "Birmingham") %>% 
  filter(AGE_GROUP != "All ages") %>% 
  filter(AGE_GROUP != "90 and over") %>% 
  rbind(Cross_border_in_males %>% 
          filter(AREA_NAME == "Birmingham") %>% 
          filter(AGE_GROUP == "90 and over") %>% 
          slice(rep(1, 11)) %>%                      # repeat the row 11 times
          mutate(AGE_GROUP = as.character(90:100)) %>% 
          mutate(across(`2023`:`2047`, ~ .x / 11))) %>%   # divide each year's value equally
  pivot_longer(cols = c(-AREA_CODE, -AREA_NAME,-COMPONENT,-SEX,-AGE_GROUP),
               names_to = "year",
               values_to = "in_") %>% 
  mutate(SEX = "Men")




#female

Cross_border_in_females = Cross_border_in_females %>% 
  filter(AREA_NAME == "Birmingham") %>% 
  filter(AGE_GROUP != "All ages") %>% 
  filter(AGE_GROUP != "90 and over") %>% 
  rbind(Cross_border_in_females %>% 
          filter(AREA_NAME == "Birmingham") %>% 
          filter(AGE_GROUP == "90 and over") %>% 
          slice(rep(1, 11)) %>%                      # repeat the row 11 times
          mutate(AGE_GROUP = as.character(90:100)) %>% 
          mutate(across(`2023`:`2047`, ~ .x / 11))) %>%   # divide each year's value equally
  pivot_longer(cols = c(-AREA_CODE, -AREA_NAME,-COMPONENT,-SEX,-AGE_GROUP),
               names_to = "year",
               values_to = "in_") %>% 
  mutate(SEX = "Women")




##cross boarder out migration 

Cross_border_out_females <- read_csv("~/R_project/smoking prevalence projection/2022 SNPP Cross border out females.csv")
Cross_border_out_males <- read_csv("~/R_project/smoking prevalence projection/2022 SNPP Cross border out males.csv")



#male
Cross_border_out_males = Cross_border_out_males %>% 
  filter(AREA_NAME == "Birmingham") %>% 
  filter(AGE_GROUP != "All ages") %>% 
  filter(AGE_GROUP != "90 and over") %>% 
  rbind(Cross_border_out_males %>% 
          filter(AREA_NAME == "Birmingham") %>% 
          filter(AGE_GROUP == "90 and over") %>% 
          slice(rep(1, 11)) %>%                      # repeat the row 11 times
          mutate(AGE_GROUP = as.character(90:100)) %>% 
          mutate(across(`2023`:`2047`, ~ .x / 11))) %>%   # divide each year's value equally
  pivot_longer(cols = c(-AREA_CODE, -AREA_NAME,-COMPONENT,-SEX,-AGE_GROUP),
               names_to = "year",
               values_to = "out") %>% 
  mutate(SEX = "Men")







#female
Cross_border_out_females  = Cross_border_out_females  %>% 
  filter(AREA_NAME == "Birmingham") %>% 
  filter(AGE_GROUP != "All ages") %>% 
  filter(AGE_GROUP != "90 and over") %>% 
  rbind(Cross_border_out_females  %>% 
          filter(AREA_NAME == "Birmingham") %>% 
          filter(AGE_GROUP == "90 and over") %>% 
          slice(rep(1, 11)) %>%                      # repeat the row 11 times
          mutate(AGE_GROUP = as.character(90:100)) %>% 
          mutate(across(`2023`:`2047`, ~ .x / 11))) %>%   # divide each year's value equally
  pivot_longer(cols = c(-AREA_CODE, -AREA_NAME,-COMPONENT,-SEX,-AGE_GROUP),
               names_to = "year",
               values_to = "out") %>% 
  mutate(SEX = "Women")


######################################
#calculate net cross boarder migration 

net_crossborder_migration_female = Cross_border_out_females %>%
  select(-COMPONENT) %>%
  left_join(
    Cross_border_in_females %>% select(-COMPONENT),
    by = c("AREA_CODE","AREA_NAME","SEX","AGE_GROUP","year")
  ) %>%
  rename(out = out, in_ = in_) %>%
  mutate(net_crossborder = in_ - out)



net_crossborder_migration_male = Cross_border_out_males %>%
  select(-COMPONENT) %>%
  left_join(
    Cross_border_in_males %>% select(-COMPONENT),
    by = c("AREA_CODE","AREA_NAME","SEX","AGE_GROUP","year")
  ) %>%
  rename(out = out, in_ = in_) %>%
  mutate(net_crossborder = in_ - out)

############################################################################################
############################################################################################
############################################################################################
#international migration 


International_in_males <- read_csv("~/R_project/smoking prevalence projection/2022 SNPP International in males.csv")
International_in_females <- read_csv("~/R_project/smoking prevalence projection/2022 SNPP International in females.csv")




#male
International_in_males = International_in_males %>% 
  filter(AREA_NAME == "Birmingham") %>% 
  filter(AGE_GROUP != "All ages") %>% 
  filter(AGE_GROUP != "90 and over") %>% 
  rbind(International_in_males %>% 
          filter(AREA_NAME == "Birmingham") %>% 
          filter(AGE_GROUP == "90 and over") %>% 
          slice(rep(1, 11)) %>%                      # repeat the row 11 times
          mutate(AGE_GROUP = as.character(90:100)) %>% 
          mutate(across(`2023`:`2047`, ~ .x / 11))) %>%   # divide each year's value equally
  pivot_longer(cols = c(-AREA_CODE, -AREA_NAME,-COMPONENT,-SEX,-AGE_GROUP),
               names_to = "year",
               values_to = "in_") %>% 
  mutate(SEX = "Men")





#female

International_in_females = International_in_females %>% 
  filter(AREA_NAME == "Birmingham") %>% 
  filter(AGE_GROUP != "All ages") %>% 
  filter(AGE_GROUP != "90 and over") %>% 
  rbind(International_in_females %>% 
          filter(AREA_NAME == "Birmingham") %>% 
          filter(AGE_GROUP == "90 and over") %>% 
          slice(rep(1, 11)) %>%                      # repeat the row 11 times
          mutate(AGE_GROUP = as.character(90:100)) %>% 
          mutate(across(`2023`:`2047`, ~ .x / 11))) %>%   # divide each year's value equally
  pivot_longer(cols = c(-AREA_CODE, -AREA_NAME,-COMPONENT,-SEX,-AGE_GROUP),
               names_to = "year",
               values_to = "in_") %>% 
  mutate(SEX = "Women")



##############################
International_out_females <- read_csv("~/R_project/smoking prevalence projection/2022 SNPP International out females.csv")
International_out_males <- read_csv("~/R_project/smoking prevalence projection/2022 SNPP International out males.csv")


#male
International_out_males  = International_out_males  %>% 
  filter(AREA_NAME == "Birmingham") %>% 
  filter(AGE_GROUP != "All ages") %>% 
  filter(AGE_GROUP != "90 and over") %>% 
  rbind(International_out_males  %>% 
          filter(AREA_NAME == "Birmingham") %>% 
          filter(AGE_GROUP == "90 and over") %>% 
          slice(rep(1, 11)) %>%                      # repeat the row 11 times
          mutate(AGE_GROUP = as.character(90:100)) %>% 
          mutate(across(`2023`:`2047`, ~ .x / 11))) %>%   # divide each year's value equally
  pivot_longer(cols = c(-AREA_CODE, -AREA_NAME,-COMPONENT,-SEX,-AGE_GROUP),
               names_to = "year",
               values_to = "out") %>% 
  mutate(SEX = "Men")





#female
International_out_females  = International_out_females  %>% 
  filter(AREA_NAME == "Birmingham") %>% 
  filter(AGE_GROUP != "All ages") %>% 
  filter(AGE_GROUP != "90 and over") %>% 
  rbind(International_out_females  %>% 
          filter(AREA_NAME == "Birmingham") %>% 
          filter(AGE_GROUP == "90 and over") %>% 
          slice(rep(1, 11)) %>%                      # repeat the row 11 times
          mutate(AGE_GROUP = as.character(90:100)) %>% 
          mutate(across(`2023`:`2047`, ~ .x / 11))) %>%   # divide each year's value equally
  pivot_longer(cols = c(-AREA_CODE, -AREA_NAME,-COMPONENT,-SEX,-AGE_GROUP),
               names_to = "year",
               values_to = "out") %>% 
  mutate(SEX = "Women")



######################################
#calculate net international migration 

net_interantional_migration_female = International_out_females %>%
  select(-COMPONENT) %>%
  left_join(
    International_in_females %>% select(-COMPONENT),
    by = c("AREA_CODE","AREA_NAME","SEX","AGE_GROUP","year")
  ) %>%
  rename(out = out, in_ = in_) %>%
  mutate(net_international = in_ - out)



net_international_migration_male = International_out_males %>%
  select(-COMPONENT) %>%
  left_join(
    International_in_males %>% select(-COMPONENT),
    by = c("AREA_CODE","AREA_NAME","SEX","AGE_GROUP","year")
  ) %>%
  rename(out = out, in_ = in_) %>%
  mutate(net_international = in_ - out)

############################################################################################
############################################################################################
############################################################################################
#calculate net migration

net_migration_male = net_crossborder_migration_male %>% 
  left_join(net_internal_migration_male, by =  c("AREA_CODE","AREA_NAME","SEX","AGE_GROUP","year")) %>% 
  left_join(net_international_migration_male, by=c("AREA_CODE","AREA_NAME","SEX","AGE_GROUP","year")) %>% 
  mutate(net_migration = net_international+net_internal+net_crossborder)







net_migration_female = net_crossborder_migration_female %>% 
  left_join(net_internal_migration_female, by =  c("AREA_CODE","AREA_NAME","SEX","AGE_GROUP","year")) %>% 
  left_join(net_interantional_migration_female, by=c("AREA_CODE","AREA_NAME","SEX","AGE_GROUP","year")) %>% 
  mutate(net_migration = net_international+net_internal+net_crossborder)




####################################################################################
####################################################################################
#use the prevalence data show we know the proportion of smoker, nonsmoker and exsmoker entering birmingham
#for simplicity, we assume migration will just follow the birmingham age specific prevalence 

smokingprev_allage = read_excel("~/R_project/smoking prevalence projection/processed data/smokingprev_allage.xlsx")


final_net_migration = rbind(net_migration_female,net_migration_male) %>% 
  rename(Sex = SEX,Age = AGE_GROUP) %>% 
  select(Sex, Age,year,net_migration) %>% 
  mutate(Age = as.numeric(Age)) %>% 
  filter(Age>=13) %>% 
  left_join(smokingprev_allage , by=c("Age","Sex"),relationship = "many-to-many") %>% 
  mutate(net_migration_adjusted = net_migration*Percentage,
         net_migration_adjusted = ifelse(Age == 13, 0,net_migration_adjusted)) #see next line explanation
#since we will inject new population from already calcualted projection from ONS, we dont need that 


write_xlsx(final_net_migration, "final_net_migration_by_states.xlsx")







