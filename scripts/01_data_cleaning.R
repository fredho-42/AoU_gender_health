library(arrow)
library(tidyverse)

dat_qf <- read_parquet('./data/cohort_20260213.parquet')

dat <- dat_qf %>%
  mutate(
    across(
      .cols = where(is.character), 
      .fns = ~ if_else(
        . %in% c("PMI: Skip", "I prefer not to answer", "Unsure", "Don't know"),
        NA_character_, .)
    )
  ) %>% 
  select(-status)

# Gender classification ----

dat <- dat %>% 
  mutate(
    gender_c = case_when(
      sex_at_birth == 'Female' & gender_identity == 'Female' ~ 'Cis woman', 
      sex_at_birth == 'Male' & gender_identity == 'Male' ~ 'Cis man', 
      
      # Fixed: Added 'Gender Identity: Transgender' 
      (sex_at_birth == 'Female' & 
         gender_identity %in% c('Male', 'Transgender', 'Gender Identity: Transgender')) | 
        (detailed_gender == 'Closer Gender Description: Trans Man') ~ 'Trans man', 
      (sex_at_birth == 'Male' & 
         gender_identity %in% c('Female', 'Transgender', 'Gender Identity: Transgender')) | 
        (detailed_gender == 'Closer Gender Description: Trans Woman') ~ 'Trans woman',       
      
      sex_at_birth == 'Female' & !is.na(gender_identity) ~ 'Diverse, female at birth', 
      sex_at_birth == 'Male' & !is.na(gender_identity) ~ 'Diverse, male at birth', 
      sex_at_birth %in% c('Intersex', 
                          'Sex At Birth: Sex At Birth None Of These') ~ 
        'Intersex / other at birth', 
      .default=NA
    ) %>% factor(levels=c('Cis woman', 'Cis man', 
                          'Trans woman', 'Trans man', 
                          'Diverse, female at birth', 
                          'Diverse, male at birth', 
                          'Intersex / other at birth')), 
    .after='detailed_gender'
  ) %>% 
  mutate(
    gender_c2 = case_when(
      # 1. Cisgender groups
      sex_at_birth == 'Female' & gender_identity == 'Female' ~ 'Cis woman', 
      sex_at_birth == 'Male' & gender_identity == 'Male' ~ 'Cis man', 
      
      # 2. Binary transgender groups
      (sex_at_birth == 'Female' & 
         gender_identity %in% c('Male', 'Transgender', 'Gender Identity: Transgender')) | 
        (detailed_gender == 'Closer Gender Description: Trans Man') ~ 'Trans man', 
      (sex_at_birth == 'Male' & 
         gender_identity %in% c('Female', 'Transgender', 'Gender Identity: Transgender')) | 
        (detailed_gender == 'Closer Gender Description: Trans Woman') ~ 'Trans woman',       
      
      # 3. Specific Gender Diverse categories (Non-binary split by sex at birth)
      sex_at_birth == 'Female' & gender_identity == 'Gender Identity: Non Binary' ~ 'Non-binary, female at birth',
      sex_at_birth == 'Male' & gender_identity == 'Gender Identity: Non Binary' ~ 'Non-binary, male at birth',
      
      detailed_gender == 'Closer Gender Description: Genderqueer' ~ 'Genderqueer',
      detailed_gender == 'Closer Gender Description: Genderfluid' ~ 'Genderfluid',
      detailed_gender == 'Closer Gender Description: Gender Variant' ~ 'Gender variant',
      detailed_gender == 'Closer Gender Description: Two Spirit' ~ 'Two-Spirit',
      
      # 4. Catch-all for remaining diverse identities 
      sex_at_birth == 'Female' & !is.na(gender_identity) ~ 'Other diverse, female at birth', 
      sex_at_birth == 'Male' & !is.na(gender_identity) ~ 'Other diverse, male at birth', 
      
      # 5. Intersex / Not listed at birth
      sex_at_birth %in% c('Intersex', 'Sex At Birth: Sex At Birth None Of These') ~ 'Intersex / other at birth', 
      
      .default = NA_character_
    ) %>% factor(levels = c('Cis woman', 'Cis man', 
                            'Trans woman', 'Trans man', 
                            'Non-binary, female at birth', 
                            'Non-binary, male at birth', 
                            'Genderqueer', 'Genderfluid', 
                            'Gender variant', 'Two-Spirit', 
                            'Other diverse, female at birth', 
                            'Other diverse, male at birth', 
                            'Intersex / other at birth')), 
    .after = 'gender_c'
  )

# Variable transformation ----

dat <- dat %>% 
  mutate( 
    age_c = case_when(
      age_at_survey >= 18 & age_at_survey <= 34 ~ "18-34",
      age_at_survey >= 35 & age_at_survey <= 50 ~ "35-50",
      age_at_survey >= 51 & age_at_survey <= 64 ~ "51-64",
      age_at_survey >= 65             ~ "65+"
    ) %>% factor(), 
    race_ethnicity=case_when(
      race_ethnicity %in% c('Middle Eastern or North African', 
                            'Native Hawaiian or Other Pacific Islander', 
                            'None Indicated', 
                            'None of these') ~ 'Other', 
      .default=race_ethnicity
    ) %>% factor(levels=c(
      'White', 'Black or African American', 
      'Asian', 'American Indian or Alaska Native', 
      'More than one population', 'Other'
    )), 
    .after=age_at_survey
  ) %>% 
  mutate(
    employment_status = fct_collapse(
      employment_status,
      "Employed"          = c("Employed", "Self-employed"),
      "Unemployed"        = c("Employment Status: Out Of Work Less Than One", 
                              "Employment Status: Out Of Work One Or More"),
      "Not in labor force" = c("Homemaker", "Student", "Retired", "Disabled"),
    ) %>% fct_relevel("Employed")) %>%
  mutate(
    education = case_when(
      education %in% c("Never attended/kindergarten only", 
                       "Received elementary school education", 
                       "Educated to junior high school level", 
                       "Highest Grade: Nine Through Eleven") ~ "Less than high school",
      education == "Educated to high school level" ~ "High school",
      education %in% c("Received higher education college education", 
                       "Received vocational training") ~ "Some college / vocational",
      education == "Received university education" ~ "College or higher"
    ) %>% factor(levels=c('Less than high school', 'High school', 
                          'Some college / vocational', 'College or higher')), 
  ) %>% 
  mutate(
    education_b = ifelse(education == "College or higher", 1, 0) %>% 
      factor(), 
    .after='education'
  ) %>% 
  mutate(
    income = case_when(
      income == "Annual Income: less 10k"  ~ 5000,
      income == "Annual Income: 10k 25k"   ~ 17500,
      income == "Annual Income: 25k 35k"   ~ 30000,
      income == "Annual Income: 35k 50k"   ~ 42500,
      income == "Annual Income: 50k 75k"   ~ 62500,
      income == "Annual Income: 75k 100k"  ~ 87500,
      income == "Annual Income: 100k 150k" ~ 125000,
      income == "Annual Income: 150k 200k" ~ 175000,
      income == "Annual Income: more 200k" ~ 250000,
    ), 
  ) %>% 
  mutate(
    deprivation_quintile = ifelse(
      (race_ethnicity == 'American Indian or Alaska Native') | 
        (race_ethnicity == 'More than one population' & is.na(deprivation_index)), 
      0, ntile(deprivation_index, 5)
    ), 
    deprivation_quintile = factor(deprivation_quintile, 
                                  levels = 0:5,
                                  labels = c("Not applicable (AI/AN)", 
                                             "Q1 (Least Deprived)", 
                                             "Q2", "Q3", "Q4", 
                                             "Q5 (Most Deprived)")
    ),
    .after=deprivation_index) %>% 
  mutate(census_division = case_when(
    census_division == "Unknown"     ~ NA, 
    census_division == "Territories" ~ 'South Atlantic', 
    .default=census_division
  )) %>% 
  
## PROMIS-10 Recoding & T-Score Generation (Official Guidelines) ----
mutate(across(
  c(general_health, quality_of_life, physical_health, 
    mental_health, social_satisfaction, social_roles),
  ~case_when(
    .x == "Excellent" ~ 5,
    .x == "Very Good" ~ 4,
    .x == "Good"      ~ 3,
    .x == "Fair"      ~ 2,
    .x == "Poor"      ~ 1,
    TRUE              ~ NA_real_
  )
)) %>%
  mutate(
    fatigue = case_when(
      fatigue == "None"        ~ 5,
      fatigue == "Mild"        ~ 4,
      fatigue == "Moderate"    ~ 3,
      fatigue == "Severe"      ~ 2,
      fatigue == "Very severe" ~ 1,
      TRUE                     ~ NA_real_
    ),
    emotional_problems = case_when(
      emotional_problems == "Never"     ~ 5,
      emotional_problems == "Rarely"    ~ 4,
      emotional_problems == "Sometimes" ~ 3,
      emotional_problems == "Often"     ~ 2,
      emotional_problems == "Always"    ~ 1,
      TRUE                              ~ NA_real_
    ),
    daily_activities = case_when(
      daily_activities == "Completely" ~ 5,
      daily_activities == "Mostly"     ~ 4,
      daily_activities == "Moderately" ~ 3,
      daily_activities == "A little"   ~ 2,
      daily_activities == "Not at all" ~ 1,
      TRUE                             ~ NA_real_
    ),
    # Map raw 0-10 pain to 1-5 scale for GPH scoring
    pain_recode = case_when(
      pain_avg == 0 ~ 5,
      pain_avg >= 1 & pain_avg <= 3 ~ 4,
      pain_avg >= 4 & pain_avg <= 6 ~ 3,
      pain_avg >= 7 & pain_avg <= 9 ~ 2,
      pain_avg == 10 ~ 1,
      TRUE ~ NA_real_
    )
  ) %>% 
  # Calculate Raw Sums with Pro-rating (min 3 items required)
  mutate(
    n_gmh = rowSums(!is.na(cbind(quality_of_life, mental_health, social_satisfaction, emotional_problems))),
    sum_gmh = rowSums(cbind(quality_of_life, mental_health, social_satisfaction, emotional_problems), na.rm = TRUE),
    gmh_raw = case_when(
      n_gmh == 4 ~ sum_gmh,
      n_gmh == 3 ~ round(sum_gmh * 4 / 3),
      TRUE ~ NA_real_
    ),
    
    n_gph = rowSums(!is.na(cbind(physical_health, daily_activities, pain_recode, fatigue))),
    sum_gph = rowSums(cbind(physical_health, daily_activities, pain_recode, fatigue), na.rm = TRUE),
    gph_raw = case_when(
      n_gph == 4 ~ sum_gph,
      n_gph == 3 ~ round(sum_gph * 4 / 3),
      TRUE ~ NA_real_
    )
  ) %>%
  # Map Raw Sums to Official PROMIS T-Scores
  mutate(
    gmh_t = case_when(
      gmh_raw == 4 ~ 21.2, gmh_raw == 5 ~ 25.1, gmh_raw == 6 ~ 28.4, gmh_raw == 7 ~ 31.3,
      gmh_raw == 8 ~ 33.8, gmh_raw == 9 ~ 36.3, gmh_raw == 10 ~ 38.8, gmh_raw == 11 ~ 41.1,
      gmh_raw == 12 ~ 43.5, gmh_raw == 13 ~ 45.8, gmh_raw == 14 ~ 48.3, gmh_raw == 15 ~ 50.8,
      gmh_raw == 16 ~ 53.3, gmh_raw == 17 ~ 56.0, gmh_raw == 18 ~ 59.0, gmh_raw == 19 ~ 62.5,
      gmh_raw == 20 ~ 67.6, TRUE ~ NA_real_
    ),
    gph_t = case_when(
      gph_raw == 4 ~ 16.2, gph_raw == 5 ~ 19.9, gph_raw == 6 ~ 23.5, gph_raw == 7 ~ 26.7,
      gph_raw == 8 ~ 29.6, gph_raw == 9 ~ 32.4, gph_raw == 10 ~ 34.9, gph_raw == 11 ~ 37.4,
      gph_raw == 12 ~ 39.8, gph_raw == 13 ~ 42.3, gph_raw == 14 ~ 44.9, gph_raw == 15 ~ 47.7,
      gph_raw == 16 ~ 50.8, gph_raw == 17 ~ 54.1, gph_raw == 18 ~ 57.7, gph_raw == 19 ~ 61.9,
      gph_raw == 20 ~ 67.7, TRUE ~ NA_real_
    ), 
    .before='general_health'
  ) %>%
  select(-n_gmh, -sum_gmh, -n_gph, -sum_gph, -pain_recode, -gmh_raw, -gph_raw, 
         -quality_of_life, -mental_health, -social_satisfaction, -emotional_problems, 
         -physical_health, -daily_activities, -pain_avg, -fatigue) %>%

mutate( # Waist-height ratio
  whtr = waist_cm / height_cm,
  whr  = waist_cm / hip_cm, 
  .after=waist_cm
) %>% 
  mutate( # Smoking and vaping
    smoking = case_when(
      smoke_ever=='No' ~ 'Never', 
      smoke_ever=='Yes' & smoke_frequency=='Not at all' ~ 'Former', 
      smoke_ever=='Yes' & smoke_frequency %in% c('Every day', 'Some days') ~ 'Current', 
    ),     
    smoking_b = ifelse(smoking %in% c('Current', 'Former'), 1, 0), 
    vaping = case_when(
      vape_ever=='No' ~ 'Never', 
      vape_ever=='Yes' & vape_frequency=='Not at all' ~ 'Former', 
      vape_ever=='Yes' & vape_frequency %in% c('Every day', 'Some days') ~ 'Current', 
    ), 
    vaping_b  = ifelse(vaping  %in% c('Current', 'Former'), 1, 0),
    .before=smoke_ever
  ) %>% 
  mutate(
    alcohol_frequency = case_when(
      alcohol_frequency == "Never"                  ~ 0,
      alcohol_frequency == "Monthly or less"        ~ 0.5 / 4.33, # ~0.115 days/wk
      alcohol_frequency == "2-4 times a month"      ~ 3.0 / 4.33, # ~0.693 days/wk
      alcohol_frequency == "2-3 times a week"       ~ 2.5,
      alcohol_frequency == "4 or more times a week" ~ 5.5,        # Midpoint of 4-7
      TRUE ~ NA_real_
    ),
    alcohol_units = case_when(
      alcohol_units == "1 or 2"     ~ 1.5,
      alcohol_units == "3 or 4"     ~ 3.5,
      alcohol_units == "5 or 6"     ~ 5.5,
      alcohol_units == "7 to 9"     ~ 8.0,
      alcohol_units == "10 or more" ~ 11.0, # Standard right-censor assumption
      TRUE ~ NA_real_
    ),
    drinks_per_week = alcohol_frequency * alcohol_units,
    drinks_per_week = if_else(alcohol_frequency == "Never", 0, drinks_per_week), 
    
    .before='alcohol_ever', 
  ) %>% 
  mutate(
    bmi_c = case_when(
      bmi < 18.5 ~ "Underweight",
      bmi >= 18.5 & bmi < 25 ~ "Normal weight",
      bmi >= 25 & bmi < 30 ~ "Overweight",
      bmi >= 30 & bmi < 35 ~ "Obesity I",
      bmi >= 35 & bmi < 40 ~ "Obesity II",
      bmi >= 40 ~ "Obesity III",
    ) %>% factor(levels = c("Normal weight", "Underweight", "Overweight", 
                            "Obesity I", "Obesity II", "Obesity III")),
    bmi_b = ifelse(bmi >= 30, 1, 0),
    .after=bmi
  ) %>% 
  mutate(
    whtr_b = ifelse(whtr > 0.5, 1, 0), 
    whr_b  = case_when(
      sex_at_birth == 'Female' & whr > 0.85 ~ 1, 
      sex_at_birth == 'Male'   & whr > 0.90 ~ 1, 
      !is.na(sex_at_birth) & !is.na(whr)    ~ 0
    ), 
    .after=whr
  ) %>% 
  mutate(
    hyt = case_when(
      sbp >= 140 | dbp >= 90 ~ 'Hypertension', 
      sbp >= 120 | dbp >= 80 ~ 'Prehypertension', 
      !is.na(sbp) & !is.na(dbp) ~ 'Normal'
    ) %>% factor(levels=c('Normal', 'Prehypertension', 'Hypertension')), 
    hyt_b = ifelse(hyt=='Hypertension', 1, 0), 
    hyt_undetected   = case_when(
      hyt_b==1 & hypertension==0 ~ 1, 
      hyt_b==0 & hypertension==0 ~ 0, 
      .default=NA), 
    hyt_uncontrolled = case_when(
      hyt_b==1 & hypertension==1 ~ 1, 
      hyt_b==0 & hypertension==1 ~ 0, 
      .default=NA), 
    .after=dbp
  ) %>%
  mutate(
    mm = ifelse(mm_count >= 2, 1, 0), 
    .before=mm_count
  )

# Cohort selection ----
# n = 633547

# n = 623585
dat <- dat %>% filter(!is.na(gender_c)) 

# n = 623166
dat <- dat %>% 
  filter(gender_c!='Intersex / other at birth') %>% 
  mutate(
    gender_c=droplevels(gender_c), 
    gender_c2=droplevels(gender_c2)
  )

# n = 588126
dat <- dat %>% 
  drop_na(c(age_c, race_ethnicity, census_division, 
             deprivation_quintile, education_b, employment_status))

# Export ----

write_parquet(dat, "~/data/cohort_cleaned_20260226.parquet")



