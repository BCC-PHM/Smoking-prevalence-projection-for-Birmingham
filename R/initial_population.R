setwd("C:/Users/TMPACGAG/OneDrive - Birmingham City Council/Documents/R projects/addiction team/smoking prevalence projection/R")
library(tidyverse)
library(writexl)
library(data.table)



Population_females <- read_csv("C:/Users/TMPACGAG/OneDrive - Birmingham City Council/Documents/R projects/addiction team/smoking prevalence projection/2022 SNPP Population females.csv")

Population_males <- read_csv("C:/Users/TMPACGAG/OneDrive - Birmingham City Council/Documents/R projects/addiction team/smoking prevalence projection/2022 SNPP Population males.csv")
######################################################################################################
#since i am injecting new population only at age 13
#i will only need data from 202to 2047 fro age 13
#and 2022 base estimate for every other age 

##########################################################################
#base intiatl states data for 2022
#male
Population_males_2022 = Population_males %>% 
  filter(AREA_NAME == "Birmingham") %>% 
  filter(AGE_GROUP != "All ages") %>% 
  filter(AGE_GROUP != "90 and over") %>% 
  rbind(Population_males %>% 
          filter(AREA_NAME == "Birmingham") %>% 
          filter(AGE_GROUP == "90 and over") %>% 
          slice(rep(1, 11)) %>%                      # repeat the row 11 times
          mutate(AGE_GROUP = as.character(90:100)) %>% 
          mutate(across(`2022`:`2047`, ~ .x / 11))) %>%   # divide each year's value equally
  pivot_longer(cols = c(-AREA_CODE, -AREA_NAME,-COMPONENT,-SEX,-AGE_GROUP),
               names_to = "year",
               values_to = "count") %>% 
  mutate(SEX = "Men") %>% 
  filter(year ==2022)




#male
Population_females_2022 = Population_females %>% 
  filter(AREA_NAME == "Birmingham") %>% 
  filter(AGE_GROUP != "All ages") %>% 
  filter(AGE_GROUP != "90 and over") %>% 
  rbind(Population_males %>% 
          filter(AREA_NAME == "Birmingham") %>% 
          filter(AGE_GROUP == "90 and over") %>% 
          slice(rep(1, 11)) %>%                      # repeat the row 11 times
          mutate(AGE_GROUP = as.character(90:100)) %>% 
          mutate(across(`2022`:`2047`, ~ .x / 11))) %>%   # divide each year's value equally
  pivot_longer(cols = c(-AREA_CODE, -AREA_NAME,-COMPONENT,-SEX,-AGE_GROUP),
               names_to = "year",
               values_to = "count") %>% 
  mutate(SEX = "Women") %>% 
  filter(year ==2022)


##########################################################################
#base intiatl states for 13 yr old as injection to the model, data for 2023-2047

Population_males_2347 = Population_males %>% 
  filter(AREA_NAME == "Birmingham") %>% 
  filter(AGE_GROUP != "All ages") %>% 
  filter(AGE_GROUP != "90 and over") %>% 
  rbind(Population_males %>% 
          filter(AREA_NAME == "Birmingham") %>% 
          filter(AGE_GROUP == "90 and over") %>% 
          slice(rep(1, 11)) %>%                      # repeat the row 11 times
          mutate(AGE_GROUP = as.character(90:100)) %>% 
          mutate(across(`2022`:`2047`, ~ .x / 11))) %>%   # divide each year's value equally
  pivot_longer(cols = c(-AREA_CODE, -AREA_NAME,-COMPONENT,-SEX,-AGE_GROUP),
               names_to = "year",
               values_to = "count") %>% 
  mutate(SEX = "Men") %>% 
  filter(year !=2022) %>% 
  filter(AGE_GROUP == 13)




#male
Population_females_2347 = Population_females %>% 
  filter(AREA_NAME == "Birmingham") %>% 
  filter(AGE_GROUP != "All ages") %>% 
  filter(AGE_GROUP != "90 and over") %>% 
  rbind(Population_males %>% 
          filter(AREA_NAME == "Birmingham") %>% 
          filter(AGE_GROUP == "90 and over") %>% 
          slice(rep(1, 11)) %>%                      # repeat the row 11 times
          mutate(AGE_GROUP = as.character(90:100)) %>% 
          mutate(across(`2022`:`2047`, ~ .x / 11))) %>%   # divide each year's value equally
  pivot_longer(cols = c(-AREA_CODE, -AREA_NAME,-COMPONENT,-SEX,-AGE_GROUP),
               names_to = "year",
               values_to = "count") %>% 
  mutate(SEX = "Women") %>% 
  filter(year !=2022)%>% 
  filter(AGE_GROUP == 13)


######################################################################################
######################################################################################
#use the prevalence data show we know the proportion of smoker, nonsmoker and exsmoker in birmingham for initial states
#bind all the data above

smokingprev_allage = read_excel("C:/Users/TMPACGAG/OneDrive - Birmingham City Council/Documents/R projects/addiction team/smoking prevalence projection/processed data/smokingprev_allage.xlsx")


population_intial_states = rbindlist(list(Population_males_2022,Population_females_2022,Population_males_2347,Population_females_2347)) %>% 
  rename(Sex = SEX,Age = AGE_GROUP) %>% 
  select(Sex, Age,year,count)%>% 
  mutate(Age = as.numeric(Age)) %>% 
  left_join(smokingprev_allage , by=c("Age","Sex"),relationship = "many-to-many") %>% 
  filter(Age>=13) %>% 
  mutate(count_adjusted = count*Percentage)




write_xlsx(population_intial_states, "population_intial_states.xlsx")


