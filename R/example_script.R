library(tidyverse)

ages <- 18:20    # keep it small for example
years <- 2022:2024
sexes <- c("male","female")

# Fake prevalence lookup (normally from Table 1w/2w or surveys)
prevalence <- expand.grid(age=ages, sex=sexes, smoking_status=c("never","current","former")) %>%
  mutate(prevalence = case_when(
    smoking_status=="never"   ~ 0.7,
    smoking_status=="current" ~ 0.2,
    smoking_status=="former"  ~ 0.1
  ))

# Fake mortality risks (normally from ONS life tables + RR)
mortality <- expand.grid(age=ages, sex=sexes, smoking_status=c("never","current","former")) %>%
  mutate(mortality_risk = case_when(
    smoking_status=="never"   ~ 0.005,
    smoking_status=="current" ~ 0.01,
    smoking_status=="former"  ~ 0.007
  ))

# Fake ONS population projections
ons_pop <- expand.grid(age=ages, sex=sexes, year=years) %>%
  mutate(population = 1000)  # flat for simplicity

# Initialise state arrays
states <- expand.grid(age=ages, sex=sexes, smoking_status=c("never","current","former"), year=years) %>%
  mutate(count=0)

# Base year (2022) initial split
states <- states %>%
  left_join(ons_pop %>% filter(year==2022), by=c("age","sex","year")) %>%
  left_join(prevalence, by=c("age","sex","smoking_status")) %>%
  mutate(count = ifelse(!is.na(population), population*prevalence, count)) %>%
  select(age,sex,smoking_status,year,count)

# Loop through years
for (t in 1:(length(years)-1)) {
  
  yr <- years[t]
  nextyr <- years[t+1]
  
  for (a in ages) {
    for (s in sexes) {
      for (st in c("never","current","former")) {
        
        # current count at age, sex, state, year
        n <- states$count[states$age==a & states$sex==s & states$smoking_status==st & states$year==yr]
        
        # mortality risk for that group
        d <- mortality$mortality_risk[mortality$age==a & mortality$sex==s & mortality$smoking_status==st]
        
        # survivors (simplified: no quit/relapse/uptake yet)
        surv <- n * (1-d)
        
        # age them up one year into next cycle
        if ((a+1) %in% ages) {
          states$count[states$age==(a+1) & states$sex==s & states$smoking_status==st & states$year==nextyr] <- 
            states$count[states$age==(a+1) & states$sex==s & states$smoking_status==st & states$year==nextyr] + surv
        }
      }
    }
  }
  
  # Inject new 18-year-olds in next year
  entrants <- ons_pop %>% filter(age==18, year==nextyr) %>%
    left_join(prevalence, by=c("age","sex")) %>%
    mutate(count = population*prevalence)
  
  for (s in sexes) {
    for (st in c("never","current","former")) {
      add <- entrants$count[entrants$sex==s & entrants$smoking_status==st]
      states$count[states$age==18 & states$sex==s & states$smoking_status==st & states$year==nextyr] <- 
        states$count[states$age==18 & states$sex==s & states$smoking_status==st & states$year==nextyr] + add
    }
  }
}

# Inspect results
states %>% filter(year==2023)
