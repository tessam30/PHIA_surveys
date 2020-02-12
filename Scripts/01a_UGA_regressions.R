#**********************************************************
# Purpose: Generate Uganda Estimate for HIV Prevalence
# Author: Tim Essam, USAID OHA/SIEI/SI
# Date: 2020_02_11
# Notes: For UGA COP planning
#**********************************************************

# Regression munging per request ------------------------------------------------------
source(file.path(scriptspath, "01_UGA_hiv_prevalence.R"))

# Risk factors for logistic regression
# •	having an HIV-positive family member in the household
# •	being a single or double orphan
# X	being a head of household
# X	having first sexual experience before age 15
# X	number of sexual partners in the last 12 months (having two or more partners as a risk factor)
# •	ever attended school or missed school days (or being out of school, for those aged 9-17)
# X	level of education
# •	knowledge about HIV prevention
# X	experience of intimate partner violence or other types of violence (sexual, physical, emotional)
# •	feel pressured by friends to have sex
# X	condom use at last sex with a non-marital, non-cohabitating partner
# X	number of sexual partners in the last 12 months (having two or more partners as a risk factor)
# •	engaging in transactional sex
# X alcohol or drug use
# •	experienced pregnancy in adolescence (aged 9-19)
# •	presence of an STI
# X	marital status
# X	residence (rural, urban)
# •	unmarried AGYW who had sex



# Exctracting key hh variables to control for hh characteristics a --------

hhchar_reg <- 
  uphia$hhchar %>% 
  select(wealth_tile = wealthquintile,
    wealth = wealthscorecont,
    hoh_gender = householdheadgender, 
    householdid) %>% 
  mutate(hoh_gender = two_to_zero(hoh_gender))


# Extracting adult characteristics associated with risk --------------
adultchar_reg <-
  uphia$adultind %>%
  mutate(
    hoh = ifelse(relattohh == 1, 1, 0),
    educ = ifelse(educationuganda == 99, NA, educationuganda),
    married = ifelse(hhrmarital == 1, 1, 0),
    urban2 = ifelse(urban == 1, 1, 0),
    sex_ever = case_when(
      sexever == 2 ~ 0,
      sexever == 99 ~ NA_real_,
      TRUE ~ sexever
    ),
    first_sx_under_15 = ifelse(firstsxage < 15, 1, 0),
    sx_partners_last12 = ifelse(part12monum == -7, NA, part12monum),
    sx_partners_lifetime = ifelse(lifetimesex == -7, NA, lifetimesex),
    cdm_lst_sex = case_when(
      condomlastsex12months == 1 ~ "used",
      condomlastsex12months == 2 ~ "nope",
      condomlastsex12months == 3 ~ "no sex",
      TRUE ~ NA_character_
    ),
    no_condom_use = ifelse(cdm_lst_sex == "nope", 1, 0),
    first_vag_sex = firstsxage, 
    away_12months = case_when(
      away12mo == 2 ~ 0,
      away12mo %in% c(-9, -8) ~ NA_real_,
      TRUE ~ away12mo
    ),
    attended_school = case_when(
      schlat == 2 ~ 0,
      schlat == -9 ~ NA_real_,
      TRUE ~ (schlat)
    ),
    religion2 = factor(case_when(
      religion == 1 ~ "1_Catholic",
      religion == 2 ~ "2_Protestant",
      religion == 5 ~ "3_Pentacostal",
      religion == 7 ~ "4_Muslim",
      TRUE ~ "5_Other"
    )),
    hiv_pos_partner = case_when(
      parthivsat1 %in% c(1, 2, 3) ~ "positive",
      parthivsat1 %in% c(4, 5, 6) ~ "negative",
      parthivsat1 %in% c(7, -9) ~ "don't know"
    ),
    partner_hivpos = ifelse(hiv_pos_partner == "positive", 1, 0),
    alcohol_b4_sex = ifelse(partlastetoh1 %in% c(1, 2, 3), 1, 0),
  ) %>% 
  select(householdid, personid, region, hoh:alcohol_b4_sex)



# Bring in the kids module and narrow covariate set -----------------------
# Kids module requires a bit more attention because of how sampling was done
# and because of how children are weighted. Omitting for now.
kids <- 
  uphia$childbio %>% 
  select(householdid:varunit) %>% 
  left_join(., uphia$childind) 

# Narrow down covariates from adult biomarker data for regressions --------

adultbio_reg <- 
  adultbio_svy %>% 
  select(gender_age:age, activesyphilis_recode, hivstatusfinal_recode,
    btwt0, varstrat, varunit, agecat, hiv_miss_flag, age_15_24, region_labs) %>% 
  mutate(age_15_24_flag = ifelse(age_15_24 == "[15,24]" & gender == 2, 1, 0))

tabyl(adultbio_reg$age_15_24_flag)  

agyw_df <- 
  adultbio_reg %>% 
  filter(age_15_24_flag ==1 & hiv_miss_flag == 0) %>%  
  left_join(., adultchar_reg, by = c("personid", "householdid")) %>% 
  left_join(., hhchar_reg, by = c("householdid"))

skimr::skim(agyw_df)


# Check benchmark regressions in Stata - to be reproduced in R.
write_dta(agyw_df, file.path(dataout, "uga_agyw.dta"))
    
   