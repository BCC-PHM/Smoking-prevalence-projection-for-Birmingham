setwd("C:/Users/TMPACGAG/OneDrive - Birmingham City Council/Documents/R projects/addiction team/smoking prevalence projection/R")

library(tidyverse)
library(readxl)
library(data.table)
library(writexl)

# https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/healthandlifeexpectancies/bulletins/adultsmokinghabitsingreatbritain/2021/relateddata



smoking_prev_adult =  read_excel("~/R projects/addiction team/smoking prevalence projection/smokinghabitsintheukanditsconstituentcountries.xlsx", 
                                 sheet = "Table_1", skip = 8)


colnames(smoking_prev_adult) =gsub(" ", "", colnames(smoking_prev_adult))

colnames(smoking_prev_adult) =gsub("\r\n", "_", colnames(smoking_prev_adult))

colnames(smoking_prev_adult) = paste0("_", colnames(smoking_prev_adult))

smoking_prev_adult = smoking_prev_adult %>% 
  filter(`_Country`== "England" & `_Sex` %in% c("Men", "Women")) %>% 
  select( Sex = `_Sex`,
          AgeGroup = `_Agegroup`,
         Currentsmoker = `_2023_Currentsmokers_%`,
         Currentsmoker_LCL=`_2023_Currentsmokers_LCL`,
         Currentsmoker_UCL = `_2023_Currentsmokers_UCL`,
         EXsmoker = `_2023_Ex-smokers_%`,
         EXsmoker_LCL = `_2023_Ex-smokers_LCL`,
         EXsmoker_UCL = `_2023_Ex-smokers_UCL`,
         Neversmoker = `_2023_Neversmoked_%`,
         Neversmoker_LCL = `_2023_Neversmoked_LCL`,
         Neversmoker_UCL = `_2023_Neversmoked_UCL`) %>% 
  pivot_longer(
    cols = -c(Sex, AgeGroup),
    names_to = c("Initial", "Measure"),
    names_pattern = "^(Currentsmoker|EXsmoker|Neversmoker)(?:_(LCL|UCL))?$"
  ) %>%
  mutate(
    Measure = case_when(
      Measure == "" ~ "Percentage",
      TRUE ~ Measure
    )
  ) %>%
  pivot_wider(
    names_from = Measure,
    values_from = value
  ) %>%
  mutate(
    Initial = recode(Initial,
                     "Currentsmoker" = "Current smoker",
                     "EXsmoker" = "Ex-smoker",
                     "Neversmoker" = "Non smoker"
    )
  )

###
#https://digital.nhs.uk/data-and-information/publications/statistical/smoking-drinking-and-drug-use-among-young-people-in-england/2023
#prcoess data for younger teenager 
#will just assume male and female the same because the differences are tiny 

smoking_teen_11_15 = data.frame(
  Sex = rep(c("Men", "Women"), each =3),
  AgeGroup = "11-15",
  Initial = rep(c("Current smoker", "Ex-smoker", "Non smoker"), times = 2),
  Percentage =c(3, 1, 96,   
                3, 1, 96),  
  LCL = 0,
  UCL = 0
  

)

#to determine 16-17 smoking prevalence from ash 
#https://ash.org.uk/uploads/Profile-of-16-and-17-year-old-smokers.pdf

#https://smokinginengland.info/graphs/top-line-findings

#will use ash data instead because they have larger sample size 

smoking_teen_16_17 = data.frame(
  Sex = rep(c("Men", "Women"), each =3),
  AgeGroup = "16-17",
  Initial = rep(c("Current smoker", "Ex-smoker", "Non smoker"), times = 2),
  Percentage =c(7.8, 1, 100-1-7.8,   
                7.8, 1, 100-1-7.8),  
  LCL = 0,
  UCL = 0
  
  
  
)

#create an age to age group transformation 
age_to_agegroup = data.frame(
  Age = 13:100
)

age_to_agegroup = age_to_agegroup %>% 
  mutate(AgeGroup = case_when(
    Age <=15 ~　"11-15",
    Age >=16 & Age <=17 ~ "16-17",
    Age >=18 & Age <=24 ~ "18-24",
    Age >=25 & Age <=34 ~ "25-34",
    Age >=35 & Age <=44 ~ "35-44",
    Age >=45 & Age <=54 ~ "45-54",
    Age >= 55 & Age <= 64 ~ "55-64",
    Age >=65 ~ "65+",
    TRUE ~ NA_character_
  ))





smokingprev_allage = rbindlist(list(smoking_prev_adult,smoking_teen_11_15,smoking_teen_16_17)) %>% 
  right_join(age_to_agegroup,
        by = "AgeGroup",
        relationship = "many-to-many") %>% 
  mutate(Percentage = Percentage/100,
         UCL = UCL/100,
         LCL = LCL/100)


#set all 13 as non smoker
smokingprev_allage =smokingprev_allage %>% 
  mutate(Percentage = ifelse(Age == 13 & Initial == "Non smoker",1,Percentage),
         Percentage = ifelse(Age == 13 & Initial == "Ex-smoker",0,Percentage),
         Percentage = ifelse(Age == 13 & Initial == "Current smoker",0,Percentage))




write_xlsx(smokingprev_allage, "smokingprev_allage.xlsx")





#############################################################
#Birmingham rate 



LA_prev_adults = read_excel("~/R_project/smoking prevalence projection/smokinghabitsintheukanditsconstituentcountries.xlsx", 
                            sheet = "Table_4", skip = 9)


colnames(LA_prev_adults) =gsub(" ", "", colnames(LA_prev_adults))

colnames(LA_prev_adults) =gsub("\r\n", "_", colnames(LA_prev_adults))

colnames(LA_prev_adults) = paste0("_", colnames(LA_prev_adults))

LA_prev_adults$`_LocalAuthorityName_[note6][note20][note21]`

LA_prev_adults  %>% 
  filter(`_LocalAuthorityName_[note6][note20][note21]`== "Birmingham" & `_Sex` %in% c("Men", "Women")) %>% 
  select( Sex = `_Sex`,
          Currentsmoker = `_2023_Currentsmokers_%`,
          Currentsmoker_LCL=`_2023_Currentsmokers_LCL`,
          Currentsmoker_UCL = `_2023_Currentsmokers_UCL`,
          EXsmoker = `_2023_Ex-smokers_%`,
          EXsmoker_LCL = `_2023_Ex-smokers_LCL`,
          EXsmoker_UCL = `_2023_Ex-smokers_UCL`,
          Neversmoker = `_2023_Neversmoked_%`,
          Neversmoker_LCL = `_2023_Neversmoked_LCL`,
          Neversmoker_UCL = `_2023_Neversmoked_UCL`,
          Sample = `_2023_Samplesize_[note3]`)
  





















