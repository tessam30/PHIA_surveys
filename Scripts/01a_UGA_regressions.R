#**********************************************************
# Purpose: Generate Uganda Estimate for HIV Prevalence
# Author: Tim Essam, USAID OHA/SIEI/SI
# Date: 2020_02_11
# Notes: For UGA COP planning
#**********************************************************

# Regression munging per request ------------------------------------------------------

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
# •	alcohol or drug use
# •	experienced pregnancy in adolescence (aged 9-19)
# •	presence of an STI
# X	marital status
# X	residence (rural, urban)
# •	unmarried AGYW who had sex



# Exctracting key hh variables to control for hh characteristics a --------

hhchar_reg <- 
  hhchar %>% 
  select(wealth_tile = wealthquintile,
    wealth = wealthscorecont,
    hoh_gender = householdheadgender) %>% 
  mutate(hoh_gender = two_to_zero(hoh_gender))


# Extracting adult characteristics associated with risk --------------
adultchar_reg <-
  adultchar %>%
  mutate(
    hoh = ifelse(relattohh == 1, 1, 0),
    educ = educationuganda,
    married = ifelse(hhrmarital == 1, 1, 0),
    urban2 = ifelse(urban == 1, 1, 0),
    first_sx_under_15 = ifelse(firstsxage < 15, 1, 0),
    sx_partners_last12 = ifelse(part12monum == -7, NA, part12monum),
    sx_partners_lifetime = ifelse(lifetimesex == -7, NA, lifetimesex),
    cdm_lst_sex = case_when(
      condomlastsex12months == 1 ~ "used",
      condomlastsex12months == 2 ~ "nope",
      condomlastsex12months == 3 ~ "no sex",
      TRUE ~ NA_character_
    ),
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
    religion2 = case_when(
      religion == 1 ~ "1_Catholic",
      religion == 2 ~ "2_Protestant",
      religion == 5 ~ "3_Pentacostal",
      religion == 7 ~ "4_Muslim",
      TRUE ~ "5_Other"
    ),
    hiv_pos_partner = case_when(
      parthivsat1 %in% c(1, 2, 3) ~ "positive",
      parthivsat1 %in% c(4, 5, 6) ~ "negative",
      parthivsat1 %in% c(7, -9) ~ "don't know"
    )
  ) %>% 
  select(householdid, personid, region, gender, age, hoh:hiv_pos_partner)
    
    
   