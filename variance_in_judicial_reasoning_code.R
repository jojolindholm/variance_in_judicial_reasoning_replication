# reproduction code for `variance in judicial reasoning'

library(tidyverse)
library(plm)
library(estimatr)
library(car)
library(jtools)


# load data --------------------------------------------------------------------
load("data/votes.RData")


# generate model for extra-legislative dimension -------------------------------
m_el_timearea <- lm_robust(sources_extra_legislative_percent ~
                             degree_university_LU + degree_university_UU +
                             degree_post_europe +
                             experience_academia + experience_ministry +
                             experience_courts + experience_private +
                             experience_public_service + experience_politics +
                             sap_appointee +   # control for appointing government
                             area_administrative + area_criminal +   # controls for area of law
                             area_bankruptcy + area_environmental +
                             area_procedural + area_real_property +
                             area_tort + area_agriculture +
                             area_arbitration + area_constitution +
                             area_competition + area_contracts +
                             area_corporate + area_educational +
                             area_tax + area_family +
                             area_intellectual_property + area_labor +
                             area_municipal + area_transportation +
                             area_migration + area_health +
                             area_procurement + area_social +
                             split95,   # control for time (before/after 1995)
                           data = votes,
                           clusters = case_num)   # clustered at case


# generate model for extra-national dimension ----------------------------------
m_en_timearea <- lm_robust(sources_extra_national_percent ~
                             degree_university_LU + degree_university_UU +
                             degree_post_europe +
                             experience_academia + experience_ministry +
                             experience_courts + experience_private +
                             experience_public_service + experience_politics +
                             sap_appointee +   # control for appointing government
                             area_administrative + area_criminal +   # controls for area of law
                             area_bankruptcy + area_environmental +
                             area_procedural + area_real_property +
                             area_tort + area_agriculture +
                             area_arbitration + area_constitution +
                             area_competition + area_contracts +
                             area_corporate + area_educational +
                             area_tax + area_family +
                             area_intellectual_property + area_labor +
                             area_municipal + area_transportation +
                             area_migration + area_health +
                             area_procurement + area_social +
                             split95,   # control for time (before/after 1995)
                           data = votes,
                           clusters = case_num)   # clustered at case

# output models ----------------------------------------------------------------

# define coefs and models
coefs = c("Lund University degree" = "degree_university_LU",
          "Uppsala University degree" = "degree_university_UU",
          "Post-Europeanization degree" = "degree_post_europe",
          "Academic background" = "experience_academia",
          "Private practise background" = "experience_private",
          "Ministry background" = "experience_ministry",
          "Public service background" = "experience_public_service",
          "Judicial background" = "experience_courts",
          "Political background" = "experience_politics")

model_names = c("Extra-legislative sources",
                "Extra-national sources")

# as figure
plot_coefs(m_el_timearea, m_en_timearea,
           coefs = coefs,
           model.names = model_names,
           inner_ci_level = 0.9,
           ci_level = 0.95) +
  labs(title = "",
       model.names = "") +
  theme(legend.position = "bottom",
        axis.text.y = element_text(hjust = 0))

# as table
export_summs(m_el_timearea, m_en_timearea,
             model.names = model_names,
             coefs = coefs)

# Test whether significant differences between LU and UU
linearHypothesis(m_el_timearea, "degree_university_LU - degree_university_UU = 0")
linearHypothesis(m_en_timearea, "degree_university_LU - degree_university_UU = 0")
