# Purpose: Verify Uganda DREAMS estimates
# Author: Tim Essam, USAID/OHA
# Date: 2020-02-10
# Notes:

library(tidytext)

# Linking to where files live that are needed for analysis ----------------

# Path where data live for now -- not synched for obvious reasons
PHIApath <- "~/Documents/Github/PHIA_data/UPHIA data/"
uga_geo <- st_read(file.path(PHIApath, "uga_admbnda_adm1_ubos_v2",
  "uga_admbnda_adm1_UBOS_v2.shp")) %>% 
  mutate(district = stringr::str_to_title(ADM1_EN))

dir(PHIApath, pattern = ".xls")


# Queury the names of sheets in workbook so we know what to load for analysis
uga_estimates <- read_excel(file.path(PHIApath, "UPHIA AGYW size estimation 8-2-2020.xlsx")) %>% 
  mutate(district = str_remove_all(District, " DISTRICT") %>% str_to_title(.)) %>% 
  janitor::clean_names()


excel_sheets(file.path(PHIApath, "agyw_analysis_CDC.xlsx"))
read_path <- file.path(PHIApath, "agyw_analysis_CDC.xlsx")

uga <- read_path %>%
  excel_sheets() %>% 
  # Use basename with the file_path_sans_ext to strip extra info; Not NEEDED
  # set_names(nm = (basename(.) %>% tools::file_path_sans_ext())) %>%
  set_names() %>%
  map(~read_excel(path = read_path, sheet = .x), .id = "sheet") 

uga_replicate <- uga$incidence %>% 
  select(District, incidence = Female) %>% 
  left_join(., uga$hiv_negative, by = c("District")) %>% 
  select(district = District, incidence, hiv_neg = Female) %>% 
  left_join(., uga$district, by = c("district" = "District")) %>% 
  mutate(incid_g = case_when(
    incidence <= 0.17967 ~ 1,
    incidence >  0.17967 & incidence <= 0.30825 ~ 2,
    incidence >  0.30825 ~ 3,
    TRUE ~ (NA_real_)
  ),
    hivneg_g = case_when(
      hiv_neg <= 19963.4 ~ 1,
      hiv_neg > 19963.4 & hiv_neg <= 29521.6 ~ 2,
      hiv_neg > 29521.6 ~ 3,
      TRUE ~ (NA_real_)
    ),
    inc_neg_seq = ((10 * incid_g) + hivneg_g),
    hiv_ranking = case_when(
      inc_neg_seq == 11 ~ "low-low",
      inc_neg_seq == 12 ~ "low-moderate",
      inc_neg_seq == 13 ~ "low-high",
      inc_neg_seq == 21 ~ "moderate-low",
      inc_neg_seq == 22 ~ "moderate-moderate",
      inc_neg_seq == 23 ~ "moderate-high",
      inc_neg_seq == 31 ~ "high-low",
      inc_neg_seq == 32 ~ "high-moderate",
      inc_neg_seq == 33 ~ "high-high",
      TRUE ~ (NA_character_)
    ),
    pop_at_risk = (hiv_neg * 0.45)
  ) %>% 
  janitor::clean_names()

uga_replicate %>% 
  filter(inc_neg_seq == 31) %>% 
  group_by(region, district, hiv_ranking, inc_neg_seq) %>% 
  summarise_at(vars(incidence, hiv_neg, pop_at_risk, incid_g, hivneg_g, ), mean) 


# Summarize the risk model results as they relate to the AGYW popu --------

uga_estimates %>% 
  group_by(region) %>% 
  mutate(tot_pop = sum(number_agyw_with_1_risk_factor)) %>% 
  ungroup() %>% 
  mutate(district_order = reorder_within(district_2, number_agyw_with_1_risk_factor, region),
    region_order = fct_reorder(region, tot_pop, .desc = TRUE)) %>% 
  group_by(region_order) %>% 
  ggplot(., aes(xmax = number_agyw_with_at_least_one_of_the_top_3_risk_factors, xmin = number_agyw_with_1_risk_factor, 
    y = district_order)) +
  geom_linerange(aes(color = age), position = position_dodge2(width = 0.5, padding = 1.5), size = 1) +
  scale_y_reordered() +
  facet_wrap(~region_order, scales = "free_y", nrow = 2) +
  theme_minimal() +
  scale_colour_brewer(palette = "Accent") +
  theme(axis.text = element_text(size = 8),
    panel.grid.major.y = element_blank(),
    strip.text = element_text(hjust = 0, size = 12),
    legend.position = "top") +
  labs(y = "", 
    title = "Range of AGYW estimates with 1 to 3 risk factors by age and region",
    subtitle = "The length of the line indicates the difference in population estimates using scenarios based on 1 to 3 risk factors",
    caption = "Source: Derived from 2016/17 Uganda Population-Based HIV Impact Survey (UPHIA) and UBOS population estimates.")

uga_estimates %>% 
  group_by(region) %>% 
  mutate(tot_pop = sum(percent_agyw_in_the_region_with_at_least_one_of_the_top_3_risk_factors)) %>% 
  ungroup() %>% 
  mutate(district_order = reorder_within(district_2, percent_agyw_with_1_risk_factor, region),
    region_order = fct_reorder(region, tot_pop, .desc = TRUE)) %>% 
  group_by(region_order) %>% 
  ggplot(., aes(xmax = percent_agyw_with_at_least_one_of_the_top_3_risk_factors, xmin = percent_agyw_with_1_risk_factor, 
    y = district_order)) +
  geom_linerange(aes(color = age), position = position_dodge2(width = 0.5, padding = 1.5), size = 1) +
  scale_y_reordered() +
  scale_x_continuous(labels = scales::percent) +
  facet_wrap(~region_order, scales = "free_y", nrow = 2) +
  theme_minimal() +
  scale_colour_brewer(palette = "Accent") +
  theme(axis.text = element_text(size = 8),
    panel.grid.major.y = element_blank(),
    strip.text = element_text(hjust = 0, size = 12),
    legend.position = "top") +
  labs(y = "", 
    title = "Range of % AGYW estimates with 1 to 3 risk factors by age and region",
    subtitle = "The length of the line indicates the difference in % AGYW estimates using scenarios based on 1 to 3 risk factors",
    caption = "Source: Derived from 2016/17 Uganda Population-Based HIV Impact Survey (UPHIA) and UBOS population estimates.")

