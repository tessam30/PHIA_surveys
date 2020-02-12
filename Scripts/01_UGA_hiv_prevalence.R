# ******************************************************
# Purpose: Generate Uganda Estimate for HIV Incidence
# Author: Tim Essam, USAID OHA/SIEI/SI
# Date: 2020_01_29
# Notes: For UGA COP planning
# ******************************************************


# Package load and path finding for analysis -------------------------------------------

pacman::p_load(
  "survey", "tidyverse", "haven", "foreign", "srvyr", "tidylog", "broom", "labelled",
  "scales", "RColorBrewer", "janitor"
)

# Path where data live for now -- not synched for obvious reasons
PHIApath <- "~/Documents/Github/PHIA_data/UPHIA data/UPHIA 2016-2017 Dissemination Package v1.0 20190605/"

# What are we looking at here? Showing Stata files for comparison
uphia_dtas <- list.files(file.path(PHIApath), pattern = ".dta") %>% as.list() 
  

# Creating a list of all datasets for use
uphia <- purrr::map(uphia_dtas, ~read_dta(file.path(PHIApath, .)))
uphia <- uphia[-c(6, 7)] # kick out intermediary sampling weights

# Providing names so we can track which dataset is which
names(uphia) <- as.list(c("adultbio", "adultind", "childbio", "childind", "hhchar")) %>% set_names()

names(uphia)



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

# Colors for male/female plots -- avoiding pink/blues per guidelines
male <- "#1FC3AA"
male2 <- "#6CA18F"
female <- "#8624F5"
female2 <- "#D9812C"

sex_clrs = c("1" = "#6CA18F", "2" = "#D9812C")
sex_labels = c("male", "female")

scales::show_col(cbind(male, male2, female, female2), borders = NA)

datasource <- c("Source: 2016/17 Uganda Population-Based HIV Impact Survey (UPHIA).")

# Explore data ------------------------------------------------------------

uphia$adultbio %>%
  select(-contains("btwt")) %>%
  names()

# Check the cross-tabulations of the hivstatus variable to figure out filters (99s) needed for stats
uphia$adultbio %>%
  select(hivstatusfinal) %>%
  mutate(hivstatusfinal = two_to_zero(hivstatusfinal)) %>%
  table()

# Look at the key indicators full information
uphia$adultbio %>%
  select_at(vars(hivstatusfinal, aware, art, vls)) %>%
  Hmisc::describe()

# What do tabulations look like after modifications to account for 99s
uphia$adultbio %>%
  select_at(vars(hivstatusfinal, aware, art, vls, activesyphilis)) %>%
  mutate_all(., ~ two_to_zero(.)) %>%
  mutate_all(., ~ nn_to_nas(.)) %>%
  purrr::map(~ tidy(summary(.x))) %>%
  do.call(rbind, .)


# Fix 2s to zero for major variables of interest
adultbio_svy <-
  uphia$adultbio %>%
  left_join(., uphia$hhchar %>% select(householdid, region), by = c("householdid")) %>% 
  mutate_at(vars(hivstatusfinal, aware, art, vls, activesyphilis), list(recode = two_to_zero)) %>%
  mutate_at(vars(hivstatusfinal, aware, art, vls, activesyphilis), list(nn_to_nas)) %>%
  # generate age categories as you need
  mutate(
    agecat = case_when(
      age <= 19 ~ "15-19",
      age >= 20 & age <= 24 ~ "20-24",
      age >= 25 & age <= 29 ~ "25-29",
      age >= 30 & age <= 34 ~ "30-34",
      age >= 35 & age <= 39 ~ "35-39",
      age >= 40 & age <= 49 ~ "40-49",
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
    )
  ) %>%

  # Create age / sex groups based on index values (1 - 7 men + age, 8 -14 female + age)
  group_by(gender, agecat) %>%
  mutate(gender_age = group_indices()) %>%
  ungroup() %>%
  select(gender_age, age_group5, everything())


# Old way of doing things with the survey package -------------------------

# Declare data to be survey set
# Check if any observations are missing weights
# See page 26 of the PHIA Data use manual on which weight to use.
# Here we use blood weights as we are dealing with HIV prevalence
adultbio_svy %>%
  filter(is.na(varunit)) %>%
  count() # 570 missing weights -- not good

phia_svyset <- svydesign(
  id = ~varunit, strata = ~varstrat, weights = ~btwt0,
  data = adultbio_svy %>% filter(!is.na(varunit) & hiv_miss_flag == 0),
  nest = TRUE
)
summary(phia_svyset)

# Demonstrate old way for estimating stats
(res <- svyciprop(~hivstatusfinal_recode,
  design = phia_svyset,
  method = "mean", level = 0.95
))

(c(res[[1]], attr(res, "ci"))) # display results
table(phia_svyset$variables$hivstatusfinal_recode)

# What is the status across all adults by region? This was the question asked.
svyby(~hivstatusfinal_recode, ~region_labs, phia_svyset, svymean)


# New way with the srvyr package that fits within tidyverse ---------------

# See here for the update on srvyr
# https://cran.r-project.org/web/packages/srvyr/vignettes/srvyr-vs-survey.html

uga_adults <- 
  adultbio_svy %>%
  filter(!is.na(varunit) & hiv_miss_flag == 0) %>% # exclude missing weights and flagged hiv
  #filter(!is.na(varunit)) %>% 
  as_survey_design(id = varunit, strata = varstrat, weights = btwt0, nest = TRUE) # declare data svyset

natl_ave <- uga_adults %>%
  summarise(hvi_prev = survey_mean(hivstatusfinal_recode, vartype = "ci")) %>%
  pull()

uga_adults %>%
  group_by(region) %>% # stratifying summary stats on region
  filter(gender == 2 & age >= 15 & age <= 49) %>%
  summarise(
    hvi_prev = survey_mean(hivstatusfinal_recode, vartype = "ci"),
    n = unweighted(n())
  )



# Look at the prevalence by male / female across large age swaths
hiv_prev <- 
  uga_adults %>%
  group_by(agecat, gender, region_labs, age_15_24) %>%
  summarise(
    hvi_prev = survey_mean(hivstatusfinal_recode, vartype = "ci"),
    n = unweighted(n())
  )

# Plot the results in a dot plot with ci's to show variation adn sample sizes
hiv_prev %>% filter(age_15_24 == "[15,24]") %>% 
  ggplot(., aes(x = hvi_prev, y = agecat, color = factor(gender), fill = factor(gender))) +
  geom_vline(xintercept = natl_ave, colour = llamar::grey10K, size = 2) +
  geom_segment(aes(x = hvi_prev_low, xend = hvi_prev_upp, y = agecat, yend = agecat)) +
  geom_point(size = 3, shape = 21, colour = "white", stroke = 1) +
  ggrepel::geom_text_repel(aes(label = n, colour = factor(gender)),
    size = 2,
    segment.color = NA, vjust = 0, force = 5
  ) +
  scale_colour_manual(
    values = sex_clrs, name = "",
    labels = sex_labels
  ) +
  scale_fill_manual(
    values = sex_clrs, name = "",
    labels = sex_labels
  ) +
  scale_x_continuous(labels = percent) +
  facet_wrap(~region_labs, nrow = 2) +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.justification = "left",
    panel.spacing = unit(2, "lines"),
    strip.text = element_text(hjust = 0)
  ) +
  labs(
    x = "", y = "",
    title = "15 - 24 year HIV prevalence for Uganda",
    subtitle = str_c("Gray line represents national average ~", round(natl_ave*100, 1), "%. Numbers below estimate indicate sample size."),
    caption = datasource
  )



