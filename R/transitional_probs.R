setwd("C:/Users/TMPACGAG/OneDrive - Birmingham City Council/Documents/R projects/addiction team/smoking prevalence projection")


library(tidyverse)
library(readxl)
library(data.table)
library(writexl)


#the transP from non smoker to current smoker from published litrature has broken down by imd quntile
#need to pull a weighted average transP
#by using proportion of Birmingham population in each decile 



###########################
deprivation_2019_all_indicies_birmingham_postcodes <- read_csv("deprivation-2019-all-indicies-birmingham-postcodes.csv")


deprivation_2019_all_indicies_birmingham_postcodes = deprivation_2019_all_indicies_birmingham_postcodes %>% 
  filter(`Postcode Status` == "Live") %>% 
  group_by(`LSOA 2011 Code`) %>% 
  summarise(`Index of Multiple Deprivation Decile` = mean(`Index of Multiple Deprivation Decile`))



###########################

lsoapopulation2021 <- read_excel("lsoapopulation2021.xlsx", 
                                 sheet = "Mid-2022 LSOA 2021", skip = 3)


deprivation_prop = lsoapopulation2021 %>% 
  filter(`LAD 2021 Name` == "Birmingham") %>% 
  pivot_longer(cols = where(is.numeric),
               names_to = "Age",
               values_to = "count") %>% 
  filter(Age == "Total") %>% 
  left_join(deprivation_2019_all_indicies_birmingham_postcodes, by = c("LSOA 2021 Code" = "LSOA 2011 Code")) %>% 
  group_by(`Index of Multiple Deprivation Decile`) %>% 
  summarise(population = sum(count)) %>% 
  drop_na() %>%   #for simplicity i will just frop them without transforming
  mutate(prop = population/sum(population))



###########################
#https://fingertips.phe.org.uk/profile/tobacco-control/data#page/7/gid/1938132885/pat/159/par/K02000001/ati/15/are/E92000001/iid/92443/age/168/sex/4/cat/-1/ctp/-1/yrr/1/cid/4/tbm/1/page-options/ine-yo-1:2022:-1:-1_ine-pt-0_ine-ct-146
#fingtips smoking prevalence distribution to imd decile 

smoking_prev_by_imd_uk = data.frame(
  IMD_decile = 1:10,
  percentage = c(16.4,14.6,12.1,14.9,12.7,12.4,10.7,10.6,9.4,10.3)/100
)



##########################
#averaging of deciles with weighting using deprivation_prop (Birmingham specific)

smoking_prev_by_imd_birm = smoking_prev_by_imd_uk %>% 
  left_join(deprivation_prop, by =c( "IMD_decile"= "Index of Multiple Deprivation Decile")) %>% 
  mutate(IMD_quintile = case_when(
    IMD_decile == 1 | IMD_decile == 2 ~ 5,
    IMD_decile == 3 | IMD_decile == 4 ~ 4,
    IMD_decile == 5 | IMD_decile == 6 ~ 3,
    IMD_decile == 7 | IMD_decile == 8 ~ 2,
    IMD_decile == 9 | IMD_decile == 10 ~ 1,
  )) %>% 
  group_by(IMD_quintile) %>% 
  summarise(weighted_avg = sum(percentage * prop) / sum(prop)) %>% 
  mutate(IMD_quintile = as.character(IMD_quintile))


############################################################################################################################################################
#transP from non smoker/never to current smoker 
TransP_N_2_C <- read_excel("smoking_state_transition_probabilities_England.xlsx", 
                                                             sheet = "Initiation", skip = 1)


#use 2022 value 

TransP_N_2_C  = TransP_N_2_C %>% 
  filter(year == "2022") %>% 
  mutate(sex = ifelse(sex == "Female",
                      "Women",
                      "Men"),
         imd_quintile = str_extract(imd_quintile, "^[1-5]")) %>% 
  left_join(smoking_prev_by_imd_birm , by = c("imd_quintile" = "IMD_quintile")) %>% 
  group_by(year,age,sex) %>% 
  summarise(tp_N_2_C =  sum(p_start * weighted_avg) / sum(weighted_avg))

# make grid of missing ages 31–100 x sex
ages_fill <- expand_grid(
  year = 2022,
  age  = 31:100,
  sex  = c("Men", "Women")
) %>%
  mutate(tp_N_2_C = 0)

TransP_N_2_C = bind_rows(TransP_N_2_C,ages_fill ) %>% 
  rename(Age =age,
         Sex =sex)




###########################################################
#transP from current smoker to ex smoker

TransP_C_2_Ex <- read_excel("smoking_state_transition_probabilities_England.xlsx", 
                           sheet = "Quit", skip = 1)




TransP_C_2_Ex = TransP_C_2_Ex %>% 
  filter(year == "2022") %>% 
  mutate(sex = ifelse(sex == "Female",
                      "Women",
                      "Men"),
         imd_quintile = str_extract(imd_quintile, "^[1-5]")) %>% 
  left_join(smoking_prev_by_imd_birm , by = c("imd_quintile" = "IMD_quintile")) %>% 
  group_by(year,age,sex) %>% 
  summarise(tp_C_2_Ex =  sum(p_quit * weighted_avg) / sum(weighted_avg))

#Fill 90–100 with relapse = 0
ages_fill_quit<- expand_grid(
  year = 2022,
  age  = 89:100,
  sex  = c("Men", "Women")
) %>%
  mutate(tp_C_2_Ex = 0)


TransP_C_2_Ex = bind_rows(TransP_C_2_Ex, ages_fill_quit)%>% 
  rename(Age =age,
         Sex =sex)

  

###########################################################
#transP_EX_2_C

TransP_EX_2_C = read_excel("smoking_state_transition_probabilities_England.xlsx", 
           sheet = "Relapse", skip = 1)


TransP_EX_2_C = TransP_EX_2_C  %>% 
  filter(year == "2022")%>% 
  mutate(sex = ifelse(sex == "Female",
                      "Women",
                      "Men"),
         imd_quintile = str_extract(imd_quintile, "^[1-5]"))%>% 
  left_join(smoking_prev_by_imd_birm , by = c("imd_quintile" = "IMD_quintile")) %>% 
  group_by(year,age,sex,time_since_quit) %>% 
  summarise(tp_EX_2_C =  sum(p_relapse * weighted_avg) / sum(weighted_avg))



#Fill 90–100 with relapse = 0
ages_fill_relapse<- expand_grid(
  year = 2022,
  age  = 90:100,
  sex  = c("Men", "Women"),
  time_since_quit = 1:10   # all quit states
) %>%
  mutate(tp_EX_2_C = 0)


TransP_EX_2_C = bind_rows(TransP_EX_2_C, ages_fill_relapse) %>% 
  rename(Age =age,
         Sex =sex)

###############################################################


write_xlsx(
  list(
    Initiation = TransP_N_2_C, 
    Quit = TransP_C_2_Ex,
    Relapse = TransP_EX_2_C
  ),
  "transition_probabilities_birmingham.xlsx"
)



  
