############################################################
# Purpose: Generate Uganda Estimate for HIV Incidence
# Author: Tim Essam, USAID OHA/SIEI/SI
# Date: 2020_01_29
# Notes: For UGA COP planning
############################################################


# Package load and path finding -------------------------------------------

pacman::p_load("survey", "tidyverse", "haven", "foreign", "srvyr", "tidylog", "broom", "labelled",
               "scales")

# Path where data live for now -- not synched for obvious reasons
PHIApath <- '~/Documents/Github/PHIA_data/UPHIA data/UPHIA 2016-2017 Dissemination Package v1.0 20190605/'

# What are we looking at here? Showing Stata files for comparison
list.files(file.path(PHIApath), pattern = ".dta")

hhinfo <- read_dta(file.path(PHIApath, "Uphia2016hh.dta")) %>% 
  select(householdid, region)

# load adult bio data for producing prevalance estimates & join in HH info needed for analysis
adultbio <- read_dta(file.path(PHIApath, "Uphia2016adultbio.dta")) %>% 
  left_join(., hhinfo, by = "householdid")

adultchar <- read_dta(file.path(PHIApath, "Uphia2016adultind.dta"))


# Function setup ----------------------------------------------------------
# Convert 2s to 0 and 99s to NAs for summary statistics and regressions
two_to_zero <- function(x) {
  ifelse(x == 2, 0, x) 
}

nn_to_nas <- function(x) {
  ifelse(x == 99, NA, x)
} 

# labels age / gender breakdown
age_gend_labels <- c(
  "15-19 M",
  "20-24 M",
  "25-29 M",
  "30-34 M",
  "35-39 M",
  "40-49 M",
  "50+ M",
  "15-19 F",
  "20-24 F",
  "25-29 F",
  "30-34 F",
  "35-39 F",
  "40-49 F",
  "50+ F"
  )

male="#1FC3AA"
male2 = "#6CA18F"
female="#8624F5"
female2 = "#D9812C"

source = c("Source: 2016/17 Uganda Population-Based HIV Impact Survey (UPHIA).")

# Explore data ------------------------------------------------------------

adultbio %>% select(-contains("btwt")) %>% names()

# Check the cross-tabulations of the hivstatus variable to figure out filters (99s) needed for stats
adultbio %>% 
  select(hivstatusfinal) %>% 
  mutate(hivstatusfinal = two_to_zero(hivstatusfinal)) %>% 
  table()

# Look at the key indicators full information
adultbio %>% 
  select_at(vars(hivstatusfinal, aware, art, vls)) %>% 
  Hmisc::describe()

# What do tabulations look like after modifications to account for 99s
adultbio %>%
  select_at(vars(hivstatusfinal, aware, art, vls)) %>% 
  mutate_all(., ~two_to_zero(.)) %>%
  mutate_all(., ~nn_to_nas(.)) %>% 
  purrr::map(~tidy(summary(.x))) %>% 
  do.call(rbind, .) 




# Fix 2s to zero for major variables of interest
adultbio_svy <- 
  adultbio %>% 
  mutate_at(vars(hivstatusfinal, aware, art, vls), list(recode = two_to_zero)) %>%
  mutate_at(vars(hivstatusfinal, aware, art, vls), list(nn_to_nas)) %>% 
  # generate age categories as you need
  mutate(agecat = case_when(
    age <= 19 ~ "15-19",
    age >=20 & age <= 24 ~ "20-24",
    age >=25 & age <= 29 ~ "25-29",
    age >=30 & age <= 34 ~ "30-34",
    age >=35 & age <= 39 ~ "35-39",
    age >=40 & age <= 49 ~ "40-49",
    TRUE ~ "50 plus"
  ),
  # Flagging all values that are 99 for hiv status
  hiv_miss_flag = ifelse(hivstatusfinal == 99, 1, 0),
  age_group5 = cut(age, c(15, 19, 24, 29, 34, 39, 49, 99), include.lowest = TRUE),
  age_15_24 = cut(age, c(15, 24, 99), include.lowest = TRUE),
  region_labs = case_when(
    region == 1 ~ "Central1",
    region == 2 ~ "Central2",
    region == 3 ~ "Kampala",
    region == 4 ~ "East Central",
    region == 5 ~ "Mid~East",
    region == 6 ~ "North East",
    region == 7 ~ "West Nile",
    region == 8 ~ "Mid~North",
    region == 9 ~ "Mid~West",
    region == 10 ~ "South West"
  )) %>%  
  
  #Create age / sex groups based on index values (1 - 7 men + age, 8 -14 female + age)
  group_by(gender, agecat) %>% 
  mutate(gender_age = group_indices()) %>% 
  ungroup() %>% 
  select(gender_age, age_group5, everything()) 








# Old way of doing things with the survey package -------------------------

# Declare data to be survey set
# Check if any observations are missing weights
adultbio_svy %>% filter(is.na(varunit)) %>% count() # 570 missing weights -- not good


phia_svyset <-svydesign(id = ~varunit, strata = ~varstrat,  weights = ~btwt0, 
                            data = adultbio_svy %>% filter(!is.na(varunit) & hiv_miss_flag == 0) , 
                            nest = TRUE)
summary(phia_svyset)

# Estimate hiv status
(res <- svyciprop(~hivstatusfinal_recode,
                 design = phia_svyset,
                 method="mean", level=0.95))

(c(res[[1]],attr(res,"ci"))) # display results
table(phia_svyset$variables$hivstatusfinal_recode)

# What is the status across all adults by region?
svyby(~hivstatusfinal_recode, ~region, phia_svyset, svymean)


# New way with the srvyr package that fits within tidyverse ---------------

# See here for the update on srvyr
# https://cran.r-project.org/web/packages/srvyr/vignettes/srvyr-vs-survey.html

uga_adults <- adultbio_svy %>% 
  filter(!is.na(varunit) & hiv_miss_flag == 0) %>% #exclude missing weights and flagged hiv
  as_survey_design(id = varunit, strata = varstrat,  weights = btwt0, nest = TRUE) #declare data svyset

natl_ave <- uga_adults %>% 
  summarise(hvi_prev = survey_mean(hivstatusfinal_recode, vartype = "ci")) %>% pull()

uga_adults %>% 
  group_by(region) %>% #stratifying summary stats on region
  filter(gender == 2 & age>= 15 & age <= 49) %>%   
  summarise(hvi_prev = survey_mean(hivstatusfinal_recode, vartype = "ci"),
            n = unweighted(n()))



# Look at the prevalence by male / female across large age swaths
hiv_prev <- uga_adults %>% 
  group_by(agecat, gender, region_labs) %>% 
  summarise(hvi_prev = survey_mean(hivstatusfinal_recode, vartype = "ci"),
            n = unweighted(n()))
  
  # Plot the results in a dot plot with ci's
hiv_prev %>% 
  ggplot(., aes(x = hvi_prev, y = agecat, color = factor(gender), fill = factor(gender))) +
  geom_vline(xintercept = natl_ave, colour = llamar::grey10K, size = 2) +
  geom_segment(aes(x = hvi_prev_low, xend = hvi_prev_upp, y = agecat, yend = agecat)) +
  geom_point(size = 3, shape = 21, colour = "white", stroke = 1) +
    ggrepel::geom_text_repel(aes(label = n, colour = factor(gender)), size = 2, 
                             segment.color = NA, vjust = 0, force = 5 ) +
  scale_colour_manual(values = c("1" = male2, "2" = female2), name = '',
                      labels = c("male", "female")) +
  scale_fill_manual(values = c("1" = male2, "2" = female2), name = '',
                    labels = c("male", "female")) +
  scale_x_continuous(labels = percent) +
  facet_wrap(~region_labs, nrow = 2) +
  theme_minimal() +
  theme(legend.position = "top",
        legend.justification='left',
        panel.spacing = unit(2, "lines"),
        strip.text = element_text(hjust = 0)) + 
  labs(x = "", y = "", 
       title = "20-30 year old men and women have large differences in HIV prevalence",
       subtitle = "Gray line represents national average ~ 6.7%. Numbers below estimate indicate sample size.",
       source = source)



# Regression munging ------------------------------------------------------

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


adultchar_reg <- 
  adultchar %>% 
  mutate(hoh = ifelse(relattohh == 1, 1, 0),
         educ = educationuganda,
         married = ifelse(hhrmarital == 1, 1, 0),
         urban = ifelse(urban == 1, 1, 0), 
         firstsx_und15 = ifelse(firstsxage < 15, 1, 0),
         sx_partners_last12 = part12monum,
         cdm_lst_sex = condomlastnonmaritalsex12months,
         sx_violencepart_12mo = sexualviolencepart12mo,
         phys_violencepat_12mo = physicalviolencepart12mo)
  

