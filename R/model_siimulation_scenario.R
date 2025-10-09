setwd("C:/Users/TMPACGAG/OneDrive - Birmingham City Council/Documents/R projects/addiction team/smoking prevalence projection")

library(tidyverse)
library(readr)
library(readxl)
library(writexl)

##########################################################################
#load all the processed data i need

#population data for intials 
population = read_excel("processed data/population_intial_states.xlsx")

#migration data
migration = read_excel("processed data/final_net_migration_by_states.xlsx")

#transP
Initiation = read_excel("processed data/transition_probabilities_birmingham.xlsx", 
                        sheet = "Initiation")

Quit =  read_excel("processed data/transition_probabilities_birmingham.xlsx", 
                   sheet = "Quit")

Relapse =  read_excel("processed data/transition_probabilities_birmingham.xlsx", 
                      sheet = "Relapse")

#mortality
mortality = read_excel("processed data/mortality_processed.xlsx")
View(mortality_processed)

###################################################################################
# cascade helper to apply net migration to ex-smokers without going negative

cascade_ex = function(ex_vec, net){
  v = ex_vec
  if (net == 0) return(v)
  
  if (net > 0) {
    # add new ex-smokers into Ex1
    v[1] = v[1] + net
    return(v)
  }
  
  # net < 0 → remove from Ex1 upwards
  rem = -net
  for (k in seq_along(v)) {
    if (rem <= 0) break
    take = min(v[k], rem)
    v[k] = v[k] - take
    rem  = rem - take
  }
  v
}


###################################################################################
#to model scenario 
scenario_multiplier =function(p_init, year, age, scenario =1){
  multiplier <- case_when(
    scenario == 1 ~ 0.90,  # 10% reduction
    scenario == 2 ~ 0.70,  # 30% reduction
    scenario == 3 ~ 0.40,  # 60% reduction
    scenario == 4 ~ 0.10,  # 90% reduction
    TRUE ~ 1.0
  )
  
  if(year>=2027){
    legal_age = year - 2009   # those cohorts born after 2009 are banned
    if (age <= legal_age) {
      return(p_init * multiplier^(year+1-2027))
    }
  }
  
  return(p_init)  # no change otherwise
}


ages  = 13:100
years = 2022:2047
selected_gender = "Women"   #change to Women to re run
longtermquit = 0.0087   # only apply to Ex10

# --- fix inconsistent labels in population ---
population_fix = population %>%
  mutate(
    year = as.numeric(year), 
    Initial = recode(Initial,
                     "Never smoker" = "Non smoker",
                     "never smoker" = "Non smoker",
                     "Non-smoker"   = "Non smoker")
  )



# helper: initialise baseline vector for a given state
get_init_vec = function(pop, sex, state){
  v = pop %>% 
    filter(year == 2022,
           Sex == sex,
           Initial == state,
           Age %in% ages) %>%
    group_by(Age) %>%
    summarise(count = sum(count_adjusted, na.rm = TRUE), .groups="drop")
  out = setNames(numeric(length(ages)), as.character(ages))
  out[as.character(v$Age)] = v$count
  out
}

###################################################################
library(dplyr)
library(tidyr)



simulate_model <- function(scenario = 0, selected_gender = "Women", verbose =TRUE) {
  
  
  if (verbose) {
    message("▶ Starting simulation | Scenario: ", scenario, " | Sex: ", selected_gender)
  }
  
  
  # --- initialise matrices ---
  N   <- matrix(0, nrow=length(ages), ncol=length(years),
                dimnames=list(as.character(ages), as.character(years)))
  C   <- N; Ex1 <- N; Ex2 <- N; Ex3 <- N; Ex4 <- N; Ex5 <- N; 
  Ex6 <- N; Ex7 <- N; Ex8 <- N; Ex9 <- N; Ex10 <- N; D <- N
  
  # --- baseline year 2022 ---
  N[, "2022"]   <- get_init_vec(population_fix, selected_gender, "Non smoker")
  C[, "2022"]   <- get_init_vec(population_fix, selected_gender, "Current smoker")
  Ex1[, "2022"] <- get_init_vec(population_fix, selected_gender, "Ex-smoker")
  
  # ---- MAIN LOOP ----
  for (t in 1:(length(years)-1)) {
    this_year <- years[t]
    next_year <- years[t+1]
    
    # Progress messages every 5 years if verbose
    if (verbose) {
      message("   ... running year ", this_year, " → ", next_year)
    }
    
    for (a in ages) {
      if (a < max(ages)) {
        next_age <- a+1
        
        # --- mortality ---
        qN  <- mortality %>% filter(Age==next_age, Sex==selected_gender, Initial=="Non smoker") %>% pull(p_mort)
        qC  <- mortality %>% filter(Age==next_age, Sex==selected_gender, Initial=="Current smoker") %>% pull(p_mort)
        qEx <- mortality %>% filter(Age==next_age, Sex==selected_gender, Initial=="Ex-smoker") %>% pull(p_mort)
        
        # --- initiation (apply scenario multiplier here) ---
        p_init_raw <- Initiation %>% filter(Age==next_age, Sex==selected_gender) %>% pull(tp_N_2_C)
        if(length(p_init_raw)==0) p_init_raw <- 0
        p_init <- if (scenario == 0) p_init_raw else scenario_multiplier(p_init_raw, next_year, next_age, scenario)
        
        # --- quitting ---
        tp_C_2_Ex <- Quit %>% filter(Age==next_age, Sex==selected_gender) %>% pull(tp_C_2_Ex)
        if(length(tp_C_2_Ex)==0) tp_C_2_Ex <- 0
        
        # --- relapse ---
        r1 <- Relapse %>% filter(Age==next_age, Sex==selected_gender, time_since_quit==1) %>% pull(tp_EX_2_C)
        r2 <- Relapse %>% filter(Age==next_age, Sex==selected_gender, time_since_quit==2) %>% pull(tp_EX_2_C)
        r3 <- Relapse %>% filter(Age==next_age, Sex==selected_gender, time_since_quit==3) %>% pull(tp_EX_2_C)
        r4 <- Relapse %>% filter(Age==next_age, Sex==selected_gender, time_since_quit==4) %>% pull(tp_EX_2_C)
        r5 <- Relapse %>% filter(Age==next_age, Sex==selected_gender, time_since_quit==5) %>% pull(tp_EX_2_C)
        r6 <- Relapse %>% filter(Age==next_age, Sex==selected_gender, time_since_quit==6) %>% pull(tp_EX_2_C)
        r7 <- Relapse %>% filter(Age==next_age, Sex==selected_gender, time_since_quit==7) %>% pull(tp_EX_2_C)
        r8 <- Relapse %>% filter(Age==next_age, Sex==selected_gender, time_since_quit==8) %>% pull(tp_EX_2_C)
        r9 <- Relapse %>% filter(Age==next_age, Sex==selected_gender, time_since_quit==9) %>% pull(tp_EX_2_C)
        
        # --- migration ---
        mig_N   = migration %>% filter(year==next_year, Age==next_age, Sex==selected_gender, Initial=="Non smoker") %>% pull(net_migration_adjusted) %>% {if(length(.)==0) 0 else .}
        mig_C   = migration %>% filter(year==next_year, Age==next_age, Sex==selected_gender, Initial=="Current smoker") %>% pull(net_migration_adjusted) %>% {if(length(.)==0) 0 else .}
        mig_Ex1 = migration %>% filter(year==next_year, Age==next_age, Sex==selected_gender, Initial=="Ex-smoker") %>% pull(net_migration_adjusted) %>% {if(length(.)==0) 0 else .}
        
        # --- Never smokers ---
        N[as.character(next_age), as.character(next_year)] = 
          N[as.character(a), as.character(this_year)] * (1 - p_init - qN) +
          Ex10[as.character(a), as.character(this_year)]*longtermquit +
          mig_N
        
        # --- Current smokers ---
        C[as.character(next_age), as.character(next_year)] = 
          C[as.character(a), as.character(this_year)] * (1 - tp_C_2_Ex - qC) +
          N[as.character(a), as.character(this_year)] * p_init +
          mig_C +
          (Ex1[as.character(a),as.character(this_year)]*r1 +
             Ex2[as.character(a),as.character(this_year)]*r2 +
             Ex3[as.character(a),as.character(this_year)]*r3 +
             Ex4[as.character(a),as.character(this_year)]*r4 +
             Ex5[as.character(a),as.character(this_year)]*r5 +
             Ex6[as.character(a),as.character(this_year)]*r6 +
             Ex7[as.character(a),as.character(this_year)]*r7 +
             Ex8[as.character(a),as.character(this_year)]*r8 +
             Ex9[as.character(a),as.character(this_year)]*r9)
        
        # --- Ex-smokers (before migration) ---
        ex_vec = numeric(10)
        ex_vec[1]  = C[as.character(a),as.character(this_year)] * tp_C_2_Ex
        ex_vec[2]  = Ex1[as.character(a),as.character(this_year)]*(1 - r1 - qEx)
        ex_vec[3]  = Ex2[as.character(a),as.character(this_year)]*(1 - r2 - qEx)
        ex_vec[4]  = Ex3[as.character(a),as.character(this_year)]*(1 - r3 - qEx)
        ex_vec[5]  = Ex4[as.character(a),as.character(this_year)]*(1 - r4 - qEx)
        ex_vec[6]  = Ex5[as.character(a),as.character(this_year)]*(1 - r5 - qEx)
        ex_vec[7]  = Ex6[as.character(a),as.character(this_year)]*(1 - r6 - qEx)
        ex_vec[8]  = Ex7[as.character(a),as.character(this_year)]*(1 - r7 - qEx)
        ex_vec[9]  = Ex8[as.character(a),as.character(this_year)]*(1 - r8 - qEx)
        ex_vec[10] = Ex9[as.character(a),as.character(this_year)]*(1 - r9 - qEx) +
          Ex10[as.character(a),as.character(this_year)]*(1 - qEx - longtermquit)
        
        # --- apply migration cascade ---
        ex_vec = cascade_ex(ex_vec, mig_Ex1)
        
        # --- write back into matrices ---
        Ex1 [as.character(next_age),as.character(next_year)] = ex_vec[1]
        Ex2 [as.character(next_age),as.character(next_year)] = ex_vec[2]
        Ex3 [as.character(next_age),as.character(next_year)] = ex_vec[3]
        Ex4 [as.character(next_age),as.character(next_year)] = ex_vec[4]
        Ex5 [as.character(next_age),as.character(next_year)] = ex_vec[5]
        Ex6 [as.character(next_age),as.character(next_year)] = ex_vec[6]
        Ex7 [as.character(next_age),as.character(next_year)] = ex_vec[7]
        Ex8 [as.character(next_age),as.character(next_year)] = ex_vec[8]
        Ex9 [as.character(next_age),as.character(next_year)] = ex_vec[9]
        Ex10[as.character(next_age),as.character(next_year)] = ex_vec[10]
        
        
        # --- Deaths ---
        D[as.character(next_age),as.character(next_year)] = 
          D[as.character(a),as.character(this_year)] +
          N[as.character(a),as.character(this_year)]*qN +
          C[as.character(a),as.character(this_year)]*qC +
          Ex1[as.character(a),as.character(this_year)]*qEx +
          Ex2[as.character(a),as.character(this_year)]*qEx +
          Ex3[as.character(a),as.character(this_year)]*qEx +
          Ex4[as.character(a),as.character(this_year)]*qEx +
          Ex5[as.character(a),as.character(this_year)]*qEx +
          Ex6[as.character(a),as.character(this_year)]*qEx +
          Ex7[as.character(a),as.character(this_year)]*qEx +
          Ex8[as.character(a),as.character(this_year)]*qEx +
          Ex9[as.character(a),as.character(this_year)]*qEx +
          Ex10[as.character(a),as.character(this_year)]*qEx
        
        # Ex-smokers (cascade as before) ...
        # [keep your cascade_ex logic here]
      }
    }
    
    # entrants (same as before)
    N["13", as.character(next_year)]   <- population %>% filter(year==next_year, Sex==selected_gender, Initial=="Non smoker") %>% pull(count_adjusted) %>% {if(length(.)==0) 0 else .}
    C["13", as.character(next_year)]   <- population %>% filter(year==next_year, Sex==selected_gender, Initial=="Current smoker") %>% pull(count_adjusted) %>% {if(length(.)==0) 0 else .}
    Ex1["13", as.character(next_year)] <- population %>% filter(year==next_year, Sex==selected_gender, Initial=="Ex-smoker") %>% pull(count_adjusted) %>% {if(length(.)==0) 0 else .}
  }
  
  # ---- Collect results ----
  results <- bind_rows(
    as.data.frame(as.table(N))   %>% mutate(State="Never smoker"),
    as.data.frame(as.table(C))   %>% mutate(State="Current smoker"),
    as.data.frame(as.table(Ex1)) %>% mutate(State="Ex1"),
    as.data.frame(as.table(Ex2)) %>% mutate(State="Ex2"),
    as.data.frame(as.table(Ex3)) %>% mutate(State="Ex3"),
    as.data.frame(as.table(Ex4)) %>% mutate(State="Ex4"),
    as.data.frame(as.table(Ex5)) %>% mutate(State="Ex5"),
    as.data.frame(as.table(Ex6)) %>% mutate(State="Ex6"),
    as.data.frame(as.table(Ex7)) %>% mutate(State="Ex7"),
    as.data.frame(as.table(Ex8)) %>% mutate(State="Ex8"),
    as.data.frame(as.table(Ex9)) %>% mutate(State="Ex9"),
    as.data.frame(as.table(Ex10))%>% mutate(State="Ex10"),
    as.data.frame(as.table(D))   %>% mutate(State="Deaths")
  ) %>%
    rename(Age=Var1, Year=Var2, Count=Freq) %>%
    mutate(Scenario = scenario,
           Sex = selected_gender)
  
  if (verbose) {
    message("✔ Finished simulation | Scenario: ", scenario, " | Sex: ", selected_gender)
  }
  
  return(results)
}

# ---- Run all scenarios ----

# --- wrapper to run all scenarios and both sexes ---
all_scenarios <- expand.grid(
  scenario = 0:4,                      # 0 = baseline, 1–4 = policy scenarios
  sex = c("Men", "Women"), 
  stringsAsFactors = FALSE
) %>%
  purrr::pmap_dfr(function(scenario, sex) {
    simulate_model(scenario = scenario, selected_gender = sex)
  })


 write_xlsx(all_scenarios, "all_scenario.xlsx")

all_scenario <- read_excel("result/all_scenario.xlsx")


######################################################################

plot1 = all_scenario%>%
  mutate(State_new = State,
         State_new = ifelse(grepl("Ex", State),"Ex-smoker", State_new)) %>% 
  filter(State_new!= "Deaths") %>% 
  group_by(Year, State_new,Scenario) %>%
  summarise(Total = sum(Count, na.rm=TRUE), .groups="drop") %>% 
  mutate(newgroup = ifelse(State_new == "Current smoker",
                           "Current smoker",
                           "Former and non")) %>% 
  group_by(Year,newgroup ,Scenario) %>% 
  summarise(numerator = sum(Total)) %>% 
  ungroup() %>% 
  group_by(Year,Scenario) %>% 
  mutate(denominator =sum(numerator)) %>% 
  ungroup() %>% 
  filter(newgroup == "Current smoker") %>% 
  mutate(smoking_prev = numerator/denominator,Year = as.numeric(as.character(Year))) %>% 
  filter(Scenario == 0, Year>=2023) %>% 
  mutate(tooltip = paste0("Year: ", Year,"\n",
                          "Smoking Prevalence: ", round(smoking_prev*100,2), "%", "\n")) %>% 
  ggplot(aes(x = Year, y = smoking_prev, group = factor(Scenario), colour = factor(Scenario))) +
  geom_line_interactive(size=1, aes(data_id = Year), colour = "#3c3c3b")+
  geom_point_interactive(aes(tooltip = tooltip, data_id = Year),colour="#3c3c3b")+
  theme_minimal(base_size = 14)+
  scale_y_continuous(limits = c(0,0.15), labels = scales::percent)+
  scale_x_continuous(breaks = c(2023:2047))+
  labs(title = "Modelled baseline smoking prevalence in Birmingham >= 13 year olds",
       y="Prevalence") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle=45),
        legend.position = "bottom")


#custom css
tooltip_css = "
  border-radius: 12px;
  color: #333;
  background-color: white;
  padding: 10px;
  font-size: 14px;
  transition: all 0.5s ease-out;
"
girafe( ggobj = plot1, 
        options = list( opts_hover(css=" "), 
                        opts_hover_inv(css = girafe_css( css = "opacity:0.4;",
                                                         line = "opacity:0.075;", 
                                                         point = "opacity:0.075;" )), 
                        opts_tooltip(css = tooltip_css) ))























########################################################################

plot2 = all_scenario%>%
  mutate(State_new = State,
         State_new = ifelse(grepl("Ex", State),"Ex-smoker", State_new)) %>% 
  filter(State_new!= "Deaths") %>% 
  group_by(Year, State_new,Scenario) %>%
  summarise(Total = sum(Count, na.rm=TRUE), .groups="drop") %>% 
  mutate(newgroup = ifelse(State_new == "Current smoker",
                           "Current smoker",
                           "Former and non")) %>% 
  group_by(Year,newgroup ,Scenario) %>% 
  summarise(numerator = sum(Total)) %>% 
  ungroup() %>% 
  group_by(Year,Scenario) %>% 
  mutate(denominator =sum(numerator)) %>% 
  ungroup() %>% 
  filter(newgroup == "Current smoker") %>% 
  mutate(smoking_prev = numerator/denominator,Year = as.numeric(as.character(Year))) %>% 
  filter(Year >=2026) %>% 
  mutate(tooltip = paste0("Year: ", Year,"\n",
                          "Scenario: ", factor(Scenario),"\n",
                          "Smoking Prevalence: ", round(smoking_prev*100,2), "%", "\n")) %>% 
  ggplot(aes(x = Year, y = smoking_prev, group = factor(Scenario), colour = factor(Scenario))) +
  geom_line_interactive(size=1, aes(data_id = paste(Scenario)))+
  geom_line_interactive(
    data = all_scenario %>%
      mutate(State_new = State,
             State_new = ifelse(grepl("Ex", State),"Ex-smoker", State_new)) %>% 
      filter(State_new != "Deaths") %>% 
      group_by(Year, State_new, Scenario) %>%
      summarise(Total = sum(Count, na.rm = TRUE), .groups="drop") %>% 
      mutate(newgroup = ifelse(State_new == "Current smoker", "Current smoker", "Former and non")) %>% 
      group_by(Year, newgroup, Scenario) %>% 
      summarise(numerator = sum(Total)) %>% 
      ungroup() %>% 
      group_by(Year, Scenario) %>% 
      mutate(denominator = sum(numerator)) %>% 
      ungroup() %>% 
      filter(newgroup == "Current smoker") %>% 
      mutate(smoking_prev = numerator / denominator,
             Year = as.numeric(as.character(Year))) %>% 
      filter(Year >= 2023 & Year <= 2026, Scenario == 0) %>% 
      mutate(tooltip = paste0("Year: ", Year, "\n",
                              "Scenario: ", factor(Scenario), "\n",
                              "Smoking Prevalence: ", round(smoking_prev*100,2), "%", "\n")),
    size = 1, colour = "#3c3c3b", linetype = 2,aes(data_id = paste(Scenario))
  )+
  geom_point_interactive(data = all_scenario%>%
                           mutate(State_new = State,
                                  State_new = ifelse(grepl("Ex", State),"Ex-smoker", State_new)) %>% 
                           filter(State_new!= "Deaths") %>% 
                           group_by(Year, State_new,Scenario) %>%
                           summarise(Total = sum(Count, na.rm=TRUE), .groups="drop") %>% 
                           mutate(newgroup = ifelse(State_new == "Current smoker",
                                                    "Current smoker",
                                                    "Former and non")) %>% 
                           group_by(Year,newgroup ,Scenario) %>% 
                           summarise(numerator = sum(Total)) %>% 
                           ungroup() %>% 
                           group_by(Year,Scenario) %>% 
                           mutate(denominator =sum(numerator)) %>% 
                           ungroup() %>% 
                           filter(newgroup == "Current smoker") %>% 
                           mutate(smoking_prev = numerator/denominator,Year = as.numeric(as.character(Year))) %>% 
                           filter(Year >=2026) %>% 
                           mutate(tooltip = paste0("Year: ", Year,"\n",
                                                   "Scenario: ", factor(Scenario),"\n",
                                                   "Smoking Prevalence: ", round(smoking_prev*100,2), "%", "\n")),aes(tooltip = tooltip, data_id = paste(Scenario)))+
  geom_point_interactive(data = all_scenario%>%
                           mutate(State_new = State,
                                  State_new = ifelse(grepl("Ex", State),"Ex-smoker", State_new)) %>% 
                           filter(State_new!= "Deaths") %>% 
                           group_by(Year, State_new,Scenario) %>%
                           summarise(Total = sum(Count, na.rm=TRUE), .groups="drop") %>% 
                           mutate(newgroup = ifelse(State_new == "Current smoker",
                                                    "Current smoker",
                                                    "Former and non")) %>% 
                           group_by(Year,newgroup ,Scenario) %>% 
                           summarise(numerator = sum(Total)) %>% 
                           ungroup() %>% 
                           group_by(Year,Scenario) %>% 
                           mutate(denominator =sum(numerator)) %>% 
                           ungroup() %>% 
                           filter(newgroup == "Current smoker") %>% 
                           mutate(smoking_prev = numerator/denominator,Year = as.numeric(as.character(Year))) %>% 
                           filter(Year >= 2023 & Year <= 2026, Scenario == 0) %>% 
                           mutate(tooltip = paste0("Year: ", Year,"\n",
                                                   "Scenario: ", factor(Scenario),"\n",
                                                   "Smoking Prevalence: ", round(smoking_prev*100,2), "%", "\n")),aes(tooltip = tooltip, data_id = paste(Scenario)), colour = "#3c3c3b")+
  theme_minimal(base_size = 14)+
  scale_y_continuous(limits = c(0,0.15), labels = scales::percent)+
  scale_x_continuous(breaks = c(2023:2047))+
  labs(title = "Modelled smoking prevalence in Birmingham >= 13 year olds for each scenario \n assuming the Tobacco and Vape Bill comes in effect in 2027",
       y="Prevalence",
       colour = "Scenario") +
  scale_color_manual(values = c(
    "0" = "#3c3c3b",
    "1" = "#D00070",
    "2" = "#FFAD00",
    "3" = "#75BC22",
    "4" = "#84329B"
  ),
  labels = c("baseline",
             "-10% in instigation",
             "-30% in institgation",
             "-60% in instigation",
             "-90% in instigation"))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle=45),
        legend.position = "bottom")


#custom css
tooltip_css = "
  border-radius: 12px;
  color: #333;
  background-color: white;
  padding: 10px;
  font-size: 14px;
  transition: all 0.5s ease-out;
"
girafe( ggobj = plot2, 
        options = list( opts_hover(css=" "), 
                        opts_hover_inv(css = girafe_css( css = "opacity:0.4;",
                                                         line = "opacity:0.075;", 
                                                         point = "opacity:0.075;" )), 
                        opts_tooltip(css = tooltip_css) ))



