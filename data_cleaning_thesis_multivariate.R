###############################################################################
########### Data clean-Up Script for Thesis Project ###########################
############last modified: 22/08/22 by Jan Grashoff ###########################
###############################################################################
#### delete global environment ####
rm(list = ls())
###############################################################################
#### Installing and loading packages, WD settings ####
# Package names
packages <- c("tidyverse", "car", "raster", "readr", "psych", "lme4", "nlme") # required packages for this script

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

#get and set WD
getwd()
setwd("C:/Users/jangr/OneDrive/Dokumente/Uni Research Master PP/Internship/Thesis/Analysis")
wd_dude <- toString(getwd())

###############################################################################
#### Create and set up data frame ####
df <- read.csv2("dataset_thesis_all.csv", header = TRUE, sep = ";")
###############################################################################
#### convert variables ####
# compute lagged school value -> doesnt work yet, not necessary
#df1 <- df %>% 
 #group_by(record_id) %>% 
  #mutate(school_lag = lag(school, n = 1, default = NA)) %>% 
  #relocate(school_lag, .after = school)

# select t0 and t1 and create new time var
df1 <- df %>% 
  filter(str_detect(redcap_event_name, "t0") | #filter for t0 and t1
         str_detect(redcap_event_name, "t1")) %>% 
  mutate(time = case_when(str_detect(redcap_event_name, "t0") == T ~ 0, #create new time var and set values
                          str_detect(redcap_event_name, "t0") == F ~ 1)) %>% 
  relocate(time, .before = redcap_event_name) # relocate in data frame

# age conversion
df2 <- df1 %>%  # compute age from vars
  mutate(ageMonths = 12 - demo_born_month, 
         demo_born_month = NULL,
         ageMonths_2 = ageMonths / 12,
         ageYears = 2021 - demo_born_year,
         age = ageYears + ageMonths_2) %>%
  relocate(age, .before = demo_school) %>% #relocate in data frame
  mutate(age = case_when(time == 0 ~ age,
                         time == 1 ~ replace_na(age, mean(age, na.rm = T)))) # replace NAs with age mean

  
###############################################################################         
#### rename (and revalue) variables, pool treatment group ####
# ID & demographics
df3 <- df2 %>% 
  rename(subject = record_id, # rename subject identifier
         grade = demo_class,   # rename grade var
         gender = demo_sex, # rename gender var
         consent = einwill_y_n, # rename consent var 
         therapy = demo_psych_thrpy, #rename psychotherapy var  
         school_r = demo_school) # rename school var
  
df3$gender <- recode(df3$gender, "1 = 0; 2 = 1; 3 = 2") # recode gender (0 = f, 1 = m, 3 = nonb)


# conditions
df4 <- df3 %>% 
  rename(condition = redcap_event_name) %>% # rename condition var
  mutate(condition = as.factor(case_when(str_detect(condition, "_1") == T ~ 1, # revalue condition var depending on condition
                               str_detect(condition, "_2") == T ~ 2,
                               str_detect(condition, "_3") == T ~ 3))) %>% 
  relocate(condition, .before = probandeninformation_timestamp) # relocate in df


# pool the 2 intervention groups
df4$condition <- recode(df4$condition, "2 = 1; 3 = 0") # 0=control, 1= treatment1/2


# scales
df5 <- df4 %>% 
  rename(fas_sum = fas_ges, # FAS
         macArthur_scale = ses_1, # MacArthur
         ders_awareness = ders_klarheit, # DERS
         ders_acceptance = ders_akzeptanz,
         ders_goal = ders_ziele,
         ders_strategies = ders_strategien,
         ders_impulsivity = ders_impulsivitaet,
         ders_sum = ders_ges,
         dshi_check = dshi_1, #DSHI
         dshi_binary = dshi_9,
         fev_action = fev_sum_action, #FEVER
         fev_contemplation = fev_sum_contemplation,
         fev_precontemplation = fev_sum_precontemplation)

###############################################################################
#### select variables and cases for later analysis; dshi score transformation ####
# select required vars
df6 <- df5 %>% 
  mutate(consent = case_when(consent == 1 | time == 1 ~ 1,
                             consent == 0 ~ 0)) %>% 
  dplyr::select(subject, 
                time,
                condition,
                gender,
                consent,
                age,
                grade,
                school_r,
                therapy,
                fas_sum,
                macArthur_scale,
                rki_sum,
                ders_awareness:ders_sum,
                dshi_check,
                dshi_binary,
                fev_action:fev_precontemplation)

df6$dshi_binary <- recode(df6$dshi_binary, "2 = 1; 3 = 0; 4 = 0; 5 = 0") # dshi recode: 1 & 2 = 1, else = 0

###############################################################################
#### long to wide format transformation for covariate and change score approach ####
# long to wide format
df_wide <- reshape(df6, timevar = "time",idvar = c("subject"), direction = "wide") %>% #long to wide
  rename_with(~ gsub(".0", "_pre", .x, fixed = T)) %>% # fix column name to pre/post suffix
  rename_with(~ gsub(".1", "_post", .x, fixed = T)) %>% 
  mutate(age_pre = case_when(!is.na(age_pre) ~ age_pre, # fix NAs age computation
                             is.na(age_pre) ~ age_post),
         therapy_pre = ifelse(is.na(therapy_pre), 0, therapy_pre), # recode NA values in therapy to "0" for "no therapy"
         consent_pre = case_when(!is.na(consent_pre) ~ consent_pre, # existent t1 data represents consent for NAs at t0
                                 is.na(consent_pre) ~ consent_post))


df_wide_1 <- df_wide %>% 
  mutate(condition = case_when(is.na(condition_pre) ~ condition_post, # selects NA in incomplete cases with corresponding condition value form post test
                               !is.na(condition_pre) ~ condition_pre)) %>% 
  relocate(condition, .before = condition_pre) %>% # relocates condition var in df
  subset(select = -c(condition_pre, condition_post)) # delete redundant condition pre/post columns

# dshi binary outcome 0/NA treatment
df_wide_2 <- df_wide_1 %>% 
  mutate(dshi_binary_pre = case_when(dshi_check_pre == 0 ~ 0, # check for screening item (if 0, it must be 0 also in the binary outcome)
                                     dshi_check_pre != 0 ~ dshi_binary_pre)) %>% # if != 0, keep original value
  mutate(dshi_binary_post = case_when(dshi_check_post == 0 ~ 0,
                                     dshi_check_post != 0 ~ dshi_binary_post))


###############################################################################
#### delete unused post variables & rename demo vars ####
# select only necessary pre and post vars
df_wide_3 <- df_wide_2 %>% 
  dplyr::select(subject:fev_precontemplation_pre, # select only required pre and post cases
                rki_sum_post:dshi_binary_post) %>% 
  rename(gender = gender_pre,# rename demographic vars for easier interpretation
         consent = consent_pre,
         age = age_pre,
         grade = grade_pre,
         school = school_r_pre,
         therapy = therapy_pre)
###############################################################################
#### compute change/sum scores ####
df_wide_4 <- df_wide_3 %>% 
  mutate(dshi_change = dshi_binary_post - dshi_binary_pre, # dshi change outcome
         ders_change = ders_sum_post - ders_sum_pre, # ders change mediator
         fever_sum = fev_action_pre + fev_contemplation_pre - fev_precontemplation_pre) # fever sum score moderator
df_wide_4$dshi_change <- recode(df_wide_4$dshi_change, "-1 = 0") #dshi change score dichotomization, -1 becomes 0, indicating either prevention effect (=0) or improvement (-1), to be used in complementary analysis

###############################################################################
##### check inclusion criteria and perform listwise deletion #####
df_final <- df_wide_4

df_final <- df_final[complete.cases(df_final), ] # select only complete cases

df_final <- df_final %>% 
  filter((consent == 1)) # check consent
df_final <- df_final %>% 
  filter((therapy == 0)) # check therapy status
df_final <- df_final %>% 
  filter((age < 15 & age >= 11)) %>%  # check age requirement
  mutate(age = as.integer(age)) # set age to integer to avoid error
df_final <- df_final %>% # filter for male and females as nonb only small frequency
  filter(gender == 0 | gender == 1)

###############################################################################
#### export data set for subsequent inferential analysis ####
write.csv(df_final, "C:/Users/jangr/OneDrive/Dokumente/Uni Research Master PP/Internship/Thesis/Analysis/data_thesis_complete_cases_final.csv", row.names = F)
###############################################################################