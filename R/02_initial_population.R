library(tidyverse)
library(writexl)
library(data.table)



Population_females <- read_csv("data/2022 SNPP Population females.csv")

Population_males <- read_csv("data/2022 SNPP Population males.csv")

#update on 13/10/2026
year_filter  = 2025
######################################################################################################
#since i am injecting new population only at age 13
#i will only need data from 202to 2047 fro age 13
#and 2022 base estimate for every other age 

##########################################################################
#base intiatl states data for 2024
#male
Population_males_2024 = Population_males %>% 
  filter(AREA_NAME == "Birmingham") %>% 
  filter(AGE_GROUP != "All ages") %>% 
  filter(AGE_GROUP != "90 and over") %>% 
  rbind(Population_males %>% 
          filter(AREA_NAME == "Birmingham") %>% 
          filter(AGE_GROUP == "90 and over") %>% 
          slice(rep(1, 11)) %>%                      # repeat the row 11 times
          mutate(AGE_GROUP = as.character(90:100)) %>% 
          mutate(across(`2025`:`2047`, ~ .x / 11))) %>%   # divide each year's value equally
  pivot_longer(cols = c(-AREA_CODE, -AREA_NAME,-COMPONENT,-SEX,-AGE_GROUP),
               names_to = "year",
               values_to = "count") %>% 
  mutate(SEX = "Men") %>% 
  filter(year == year_filter)




#male
Population_females_2024 = Population_females %>% 
  filter(AREA_NAME == "Birmingham") %>% 
  filter(AGE_GROUP != "All ages") %>% 
  filter(AGE_GROUP != "90 and over") %>% 
  rbind(Population_males %>% 
          filter(AREA_NAME == "Birmingham") %>% 
          filter(AGE_GROUP == "90 and over") %>% 
          slice(rep(1, 11)) %>%                      # repeat the row 11 times
          mutate(AGE_GROUP = as.character(90:100)) %>% 
          mutate(across(`2025`:`2047`, ~ .x / 11))) %>%   # divide each year's value equally
  pivot_longer(cols = c(-AREA_CODE, -AREA_NAME,-COMPONENT,-SEX,-AGE_GROUP),
               names_to = "year",
               values_to = "count") %>% 
  mutate(SEX = "Women") %>% 
  filter(year ==year_filter)


##########################################################################
#base intiatl states for 13 yr old as injection to the model, data for 2023-2047

Population_males_2447 = Population_males %>% 
  filter(AREA_NAME == "Birmingham") %>% 
  filter(AGE_GROUP != "All ages") %>% 
  filter(AGE_GROUP != "90 and over") %>% 
  rbind(Population_males %>% 
          filter(AREA_NAME == "Birmingham") %>% 
          filter(AGE_GROUP == "90 and over") %>% 
          slice(rep(1, 11)) %>%                      # repeat the row 11 times
          mutate(AGE_GROUP = as.character(90:100)) %>% 
          mutate(across(`2025`:`2047`, ~ .x / 11))) %>%   # divide each year's value equally
  pivot_longer(cols = c(-AREA_CODE, -AREA_NAME,-COMPONENT,-SEX,-AGE_GROUP),
               names_to = "year",
               values_to = "count") %>% 
  mutate(SEX = "Men") %>% 
  filter(year !=year_filter) %>% 
  filter(AGE_GROUP == 13)




#male
Population_females_2447 = Population_females %>% 
  filter(AREA_NAME == "Birmingham") %>% 
  filter(AGE_GROUP != "All ages") %>% 
  filter(AGE_GROUP != "90 and over") %>% 
  rbind(Population_males %>% 
          filter(AREA_NAME == "Birmingham") %>% 
          filter(AGE_GROUP == "90 and over") %>% 
          slice(rep(1, 11)) %>%                      # repeat the row 11 times
          mutate(AGE_GROUP = as.character(90:100)) %>% 
          mutate(across(`2025`:`2047`, ~ .x / 11))) %>%   # divide each year's value equally
  pivot_longer(cols = c(-AREA_CODE, -AREA_NAME,-COMPONENT,-SEX,-AGE_GROUP),
               names_to = "year",
               values_to = "count") %>% 
  mutate(SEX = "Women") %>% 
  filter(year !=year_filter)%>% 
  filter(AGE_GROUP == 13)


######################################################################################
######################################################################################
#use the prevalence data show we know the proportion of smoker, nonsmoker and exsmoker in birmingham for initial states
#bind all the data above
#update on 13/10/2026 smokingprev_allage2025
smokingprev_allage = read_excel("processed data/smokingprev_allage2025.xlsx")


population_intial_states = rbindlist(list(Population_males_2024,Population_females_2024,Population_males_2447,Population_females_2447)) %>% 
  rename(Sex = SEX,Age = AGE_GROUP) %>% 
  select(Sex, Age,year,count)%>% 
  mutate(Age = as.numeric(Age)) %>% 
  left_join(smokingprev_allage , by=c("Age","Sex"),relationship = "many-to-many") %>% 
  filter(Age>=13) %>% 
  mutate(count_adjusted = count*Percentage)


#update on 13/10/2026 population_intial_states2025

write_xlsx(population_intial_states, "processed data/population_intial_states2025.xlsx")


