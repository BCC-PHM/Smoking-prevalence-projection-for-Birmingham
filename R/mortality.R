setwd("~/R_project/smoking prevalence projection/R")

library(tidyverse)
library(readxl)
library(data.table)
linrary(writexl)

national_lifetables <- read_excel("~/R_project/smoking prevalence projection/national_lifetables.xlsx", 
                                  sheet = "2021-2023", skip = 5)

#mortality for male

male_qx = national_lifetables[,1:7] %>% 
  select(Age = age...1,
         mortality = qx...3) %>% 
  mutate(Sex = "Men",
         AgeGroup = case_when(
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




#mortyality for female

female_qx =national_lifetables[,8:13] %>% 
  select(Age = age...8,
         mortality = qx...10) %>% 
  mutate(Sex = "Women",
         AgeGroup = case_when(
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





############################################################
#make a relative risk table from https://www.sciencedirect.com/science/article/pii/S0749379722005712#tb2fn2
#for simplicity , just use long term quit rate for every former quitter 

male_qx_smoker =male_qx %>% 
  mutate(RR = case_when(
    Age >=30 & Age<=44 ~ 2.14,
    Age >=45 & Age<=54 ~2.66,
    Age >=55 & Age<=64 ~3.38,
    Age >=65 & Age<=74 ~3.35,
    Age >=75 & Age<=84 ~2.36,
    Age >=85 ~ 1.36, # from older data might not be accurate
    TRUE ~ 1
  ),
     RR_LCL = case_when(
       Age >=30 & Age<=44 ~ 1.86,
       Age >=45 & Age<=54 ~2.37,
       Age >=55 & Age<=64 ~3.09,
       Age >=65 & Age<=74 ~3.12,
       Age >=75 & Age<=84 ~2.20,
       Age >=85 ~ 1.01, # from older data might not be accurate
       TRUE ~ 1
     ),
  RR_UCL = case_when(
    Age >=30 & Age<=44 ~ 2.46,
    Age >=45 & Age<=54 ~2.97,
    Age >=55 & Age<=64 ~3.69,
    Age >=65 & Age<=74 ~3.60,
    Age >=75 & Age<=84 ~2.53,
    Age >=85 ~ 1.84, # from older data might not be accurate
    TRUE ~ 1
  ),
  Initial = "Current smoker"
  
  
  )



male_qx_exsmoker =male_qx %>% 
  mutate(RR = case_when(
    Age >=30 & Age<=44 ~ 1.16,
    Age >=45 & Age<=54 ~1.19,
    Age >=55 & Age<=64 ~1.41,
    Age >=65 & Age<=74 ~1.51,
    Age >=75 & Age<=84 ~1.40,
    Age >=85 ~ 1.35, # from older data might not be accurate
    TRUE ~ 1
  ),
  RR_LCL = case_when(
    Age >=30 & Age<=44 ~ 0.92,
    Age >=45 & Age<=54 ~1.02,
    Age >=55 & Age<=64 ~1.28,
    Age >=65 & Age<=74 ~1.41,
    Age >=75 & Age<=84 ~1.32,
    Age >=85 ~ 1.15, # from older data might not be accurate
    TRUE ~ 1
  ),
  RR_UCL = case_when(
    Age >=30 & Age<=44 ~ 1.44,
    Age >=45 & Age<=54 ~1.37,
    Age >=55 & Age<=64 ~1.56,
    Age >=65 & Age<=74 ~1.62,
    Age >=75 & Age<=84 ~1.47,
    Age >=85 ~ 1.58, # from older data might not be accurate
    TRUE ~ 1
  ),
  Initial = "Ex-smoker")


#####################################

female_qx_smoker = female_qx %>% 
mutate(RR = case_when(
  Age >=30 & Age<=44 ~ 2.28,
  Age >=45 & Age<=54 ~2.60,
  Age >=55 & Age<=64 ~3.20,
  Age >=65 & Age<=74 ~2.96,
  Age >=75 & Age<=84 ~2.72,
  Age >=85 ~ 1.56, # from older data might not be accurate
  TRUE ~ 1
),
RR_LCL = case_when(
  Age >=30 & Age<=44 ~ 1.93,
  Age >=45 & Age<=54 ~2.31,
  Age >=55 & Age<=64 ~2.91,
  Age >=65 & Age<=74 ~3.77,
  Age >=75 & Age<=84 ~2.57,
  Age >=85 ~ 1.28, # from older data might not be accurate
  TRUE ~ 1
),
RR_UCL = case_when(
  Age >=30 & Age<=44 ~ 2.70,
  Age >=45 & Age<=54 ~2.93,
  Age >=55 & Age<=64 ~3.51,
  Age >=65 & Age<=74 ~3.17,
  Age >=75 & Age<=84 ~2.88,
  Age >=85 ~ 1.90, # from older data might not be accurate
  TRUE ~ 1
),
Initial = "Current smoker"


)






female_qx_exsmoker=  female_qx %>% 
  mutate(RR = case_when(
    Age >=30 & Age<=44 ~ 1.09,
    Age >=45 & Age<=54 ~1.16,
    Age >=55 & Age<=64 ~1.44,
    Age >=65 & Age<=74 ~1.55,
    Age >=75 & Age<=84 ~1.47,
    Age >=85 ~ 1.54, # from older data might not be accurate
    TRUE ~ 1
  ),
  RR_LCL = case_when(
    Age >=30 & Age<=44 ~ 0.81,
    Age >=45 & Age<=54 ~0.98,
    Age >=55 & Age<=64 ~1.29,
    Age >=65 & Age<=74 ~1.45,
    Age >=75 & Age<=84 ~1.40,
    Age >=85 ~ 1.35, # from older data might not be accurate
    TRUE ~ 1
  ),
  RR_UCL = case_when(
    Age >=30 & Age<=44 ~ 1.45,
    Age >=45 & Age<=54 ~1.37,
    Age >=55 & Age<=64 ~1.61,
    Age >=65 & Age<=74 ~1.67,
    Age >=75 & Age<=84 ~1.55,
    Age >=85 ~ 1.75, # from older data might not be accurate
    TRUE ~ 1
  ),
  Initial = "Ex-smoker"
  
  
  )






#create RRtable

male_qx_never = male_qx %>%
  mutate(RR=1, RR_LCL=1, RR_UCL=1, Initial="Non smoker")

male_rr <- bind_rows(male_qx_smoker, male_qx_exsmoker, male_qx_never) %>%
  mutate(Sex = "Men")

# Do the same for females
female_qx_never = female_qx %>%
  mutate(RR=1, RR_LCL=1, RR_UCL=1, Initial="Non smoker")


female_rr <- bind_rows(female_qx_smoker, female_qx_exsmoker, female_qx_never) %>%
  mutate(Sex = "Women")

rr_all <- bind_rows(male_rr, female_rr)

#join with prevalence
smokingprev_allage <- read_excel("~/R_project/smoking prevalence projection/processed data/smokingprev_allage.xlsx")


df <- smokingprev_allage %>%
  left_join(rr_all, by = c("Sex","Age","Initial"))

#calculate averaged non smoker death risk from all cause mortality 
#following https://tobaccocontrol.bmj.com/content/30/4/380
#appendix 1 will have the equation that used below to caculated the mortality transP

mort_split <- df %>%
  group_by(Sex, Age) %>%
  mutate(
    Percentage = Percentage / sum(Percentage),  # make sure it sums to 1
    denom      = sum(Percentage * RR),
    dN         = unique(mortality) / denom,
    p_mort    = RR * dN
  )


write_xlsx(mort_split, "mortality_processed.xlsx")


