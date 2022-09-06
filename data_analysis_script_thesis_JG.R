###############################################################################
########### Data analysis Script for Thesis Project ###########################
############last modified: 27/08/22 by Jan Grashoff ###########################
###############################################################################
#### Installing and loading packages, WD settings ####
# Package names
packages <- c("tidyverse", "car", "raster", "readr", "psych", "lme4", "nlme", "lessR", "lmerTest", "mediation") # required packages for this script !(simultaneous use of "lmerTest" and "mediation" not possible)!

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
df_all_cases <- read.csv2("data_thesis_complete_cases_final.csv", header = T, sep = ",")

###############################################################################
#### outlier detection and handling ####
# get boxplots for visual inspection
df_all_cases$age <- as.integer(df_all_cases$age)
boxplot(df_all_cases$ders_sum_pre, main = "Boxplot") # ders pre boxplot
boxplot(df_all_cases$ders_sum_post, main = "Boxplot") # ders post boxplot
boxplot(df_all_cases$rki_sum_pre, main = "Boxplot") # rki boxplot
boxplot(df_all_cases$fas_sum_pre, main = "Boxplot") # fas boxplot
boxplot(df_all_cases$fever_sum, main = "Boxplot") # fever boxplot

# sum up total number of outliers according to IQR criterion
df_all_cases <- df_all_cases %>% 
  mutate(outlier_flag = ifelse(ders_sum_pre < quantile(ders_sum_pre, p = 0.25) - 1.5 * IQR(ders_sum_pre) | # create variable that is 1 if subject has at least on suspicious value based on IQR
                                 ders_sum_post < quantile(ders_sum_post, p = 0.25) - 1.5 * IQR(ders_sum_post) |
                                 rki_sum_pre < quantile(rki_sum_pre, p = 0.25) - 1.5 * IQR(rki_sum_pre) |
                                 fas_sum_pre < quantile(fas_sum_pre, p = 0.25) - 1.5 * IQR(fas_sum_pre) |
                                 fever_sum < quantile(fever_sum, p = 0.25) - 1.5 * IQR(fever_sum) |
                                 ders_sum_pre > quantile(ders_sum_pre, p = 0.75) + 1.5 * IQR(ders_sum_pre) |
                                 ders_sum_post > quantile(ders_sum_post, p = 0.75) + 1.5 * IQR(ders_sum_post) |
                                 rki_sum_pre > quantile(rki_sum_pre, p = 0.75) + 1.5 * IQR(rki_sum_pre) |
                                 fas_sum_pre > quantile(fas_sum_pre, p = 0.75) + 1.5 * IQR(fas_sum_pre) |
                                 fever_sum > quantile(fever_sum, p = 0.75) + 1.5 * IQR(fever_sum),
                               1, 0))

sum(df_all_cases$outlier_flag) # sum up outliers
                                 
###############################################################################
#### descriptive statistics ####
# variable overview (m, sd, se, etc.)
descriptive_all <- describe(df_all_cases)
descriptive_by_condition <- describeBy(df_all_cases, df_all_cases$condition,)
descriptive_all # descriptives for entire sample
descriptive_by_condition # descriptives for each condition

# gender ratios
table(df_all_cases$gender) # gender ratio full sample
table(df_all_cases$condition, df_all_cases$gender) # gender ratio per condition

table_NSSI_pre <- xtabs(~gender + condition + dshi_binary_pre, data = df_all_cases) # frequency of NSSI in each group per gender at T0
ftable(table_NSSI_pre)

table_NSSI_post <- xtabs(~gender + condition + dshi_binary_post, data = df_all_cases) # frequency of NSSI in each group per gender at T1
ftable(table_NSSI_post)

table(df_all_cases$gender, df_all_cases$dshi_binary_post) # frequency of NSSI per gender for entire sample at T1

table(df_all_cases$dshi_binary_pre) # total frequency of NSSI at T0

###############################################################################
#### grand mean center mediator and moderator + simple slope analysis preparation####
# center variables around grand mean
df_all_cases <- df_all_cases %>% 
  mutate(ders_c = (ders_change - mean(ders_change)), # centering mediator emotion regulation change score
         ders_sum_pre_c = (ders_sum_pre - mean(ders_sum_pre)), # centering emotion regulation pre score
         ders_sum_post_c = (ders_sum_post - mean(ders_sum_post)), # centering emotion regulation post score
         fever_c = (fever_sum - mean(fever_sum))) # centering moderator readiness to change pre score

# create new moderator variables for simple slope analysis
df_all_cases <- df_all_cases %>% 
  mutate(fever_m1SD = fever_c - sd(fever_c), # indicates being high on RTC
         fever_p1SD = fever_c + sd(fever_c)) # indicates being low on RTC

# save data set file to directory
write.csv(df_all_cases, "C:/Users/jangr/OneDrive/Dokumente/Uni Research Master PP/Internship/Thesis/Analysis/data_thesis_mediation.csv", row.names = F)

###############################################################################
#### mediation analysis with pretest covariate ####
# set up null models
## path a (x -> m)
fit.path.a.null <- lmer(ders_sum_post_c ~ (1|school), # build null model for ders post score
                        data = df_all_cases, 
                        REML = F) 
summary(fit.path.a.null)

## path c' (x -> y)
fit.path.c.null <- glmer(dshi_binary_post ~ (1|school), # build null model for dshi outcome
                         data = df_all_cases, 
                         control = glmerControl(check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4)),
                         family = binomial(link = "logit"))
summary(fit.path.c.null)

## calculate ICC for models
performance::icc(fit.path.a.null)
performance::icc(fit.path.c.null)

# add fixed effects, moderation/interaction and covariates 
fit.path.cprime <- glmer(dshi_binary_post ~ condition*fever_c + dshi_binary_pre + gender + fas_sum_pre + rki_sum_pre + (1|school), # c' path model
                         data = df_all_cases,
                         control = glmerControl(check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4)),
                         family = binomial(link = "logit"))

fit.mediator.mod <- lmer(ders_sum_post_c ~ condition*fever_c + ders_sum_pre_c + gender + fas_sum_pre + rki_sum_pre + (1|school), # mediation path model with moderator (a path)
                         data = df_all_cases,
                         REML = F)

fit.total.mod <- glmer(dshi_binary_post ~ condition*fever_c + dshi_binary_pre + ders_sum_pre_c + ders_sum_post_c + gender + fas_sum_pre + rki_sum_pre + (1|school), # total effect model with moderator (for b and c path)
                       data = df_all_cases,
                       control = glmerControl(check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4)),
                       family = binomial(link = "logit"))

summary(fit.path.cprime)
confint(fit.path.cprime, parm="beta_",method="Wald")

summary(fit.mediator.mod)
confint(fit.mediator.mod)

summary(fit.total.mod)
confint(fit.total.mod ,parm="beta_",method="Wald")

fit.mediator.mod.test <- lmer(ders_sum_pre_c ~ condition*fever_c + gender + fas_sum_pre + rki_sum_pre + (1|school), # mediation path model with moderator (a path)
                         data = df_all_cases,
                         REML = F)
summary(fit.mediator.mod.test)

## simple slope analysis
fit.mediator.lowfever <- lmer(ders_sum_post_c ~ condition*fever_p1SD + ders_sum_pre_c + gender + fas_sum_pre + rki_sum_pre + (1|school), # effect of condition for low levels of RTC
                              data = df_all_cases,
                              REML = F)
fit.mediator.highfever <- lmer(ders_sum_post_c ~ condition*fever_m1SD + ders_sum_pre_c + gender + fas_sum_pre + rki_sum_pre + (1|school), # effect of condition for high levels of RTC
                               data = df_all_cases,
                               REML = F)

summary(fit.mediator.lowfever)
confint(fit.mediator.lowfever)

summary(fit.mediator.highfever)
confint(fit.mediator.highfever)

# estimate moderated mediation model with ::mediate
med.fever.high <- mediate(fit.mediator.mod, fit.total.mod, # mediation for high levels of RTC
                          treat = "condition",
                          mediator = "ders_sum_post_c",
                          covariates = list(df_all_cases$fever_m1SD))

med.fever.low <- mediate(fit.mediator.mod, fit.total.mod, # mediation for low levels of RTC
                         treat = "condition",
                         mediator = "ders_sum_post_c",
                         covariates = list(df_all_cases$fever_p1SD))

med.fever.average <- mediate(fit.mediator.mod, fit.total.mod, # mediation for average levels of RTC
                             treat = "condition",
                             mediator = "ders_sum_post_c",
                             covariates = list(df_all_cases$fever_c))

summary(med.fever.high)
summary(med.fever.low)
summary(med.fever.average)

## build simple mediation model because of no significant moderation
fit.mediator.simple <- lmer(ders_sum_post_c ~ condition + ders_sum_pre_c + gender + fas_sum_pre + rki_sum_pre + (1|school), # mediation path without moderator
                     data = df_all_cases,
                     REML = F)

fit.total.simple <- glmer(dshi_binary_post ~ condition + dshi_binary_pre + ders_sum_pre_c + ders_sum_post_c + gender + fas_sum_pre + rki_sum_pre + (1|school), # direct effect model without moderator
                   family = binomial(link = "logit"),
                   control = glmerControl(check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4)),
                   data = df_all_cases)

## estimate mediation without moderator with ::mediate
mediation.simple <- mediate(fit.mediator.simple, fit.total.simple,
                             treat = "condition",
                             mediator = "ders_sum_post_c",
                             boot = F,
                             sims = 500)
summary(mediation.simple)


# check assumptions of each model separately
## heteroscedasticity
plot(fit.mediator.mod)

## normality of residuals
hist(resid(fit.mediator.mod))
qqnorm(resid(fit.mediator.mod))
qqline(resid(fit.mediator.mod))

## multicollinearity
vif(fit.path.cprime)
vif(fit.mediator.mod)
vif(fit.total.mod)

###############################################################################
#### mediation analysis with change scores ####
# set up null models
## path a
fit.path.a.change <- lmer(ders_c ~ (1|school),
                          data = df_all_cases,
                          REML = F)
summary(fit.path.a.change)

## path c prime
fit.path.c.change <- glmer(dshi_change ~ (1|school),
                           data = df_all_cases,
                           family = binomial(link = "logit"))
summary(fit.path.c.change)

## calculate ICC for models
performance::icc(fit.path.a.change)
performance::icc(fit.path.c.change)

# add fixed effects and covariates to models
## for moderated mediation
fit.path.cprime_change <- glmer(dshi_change ~ condition*fever_c + gender + fas_sum_pre + rki_sum_pre + (1|school), # c prime path model
                                data = df_all_cases,
                                family = binomial(link = "logit"))

fit.mediator.mod.change <- lmer(ders_c ~ condition*fever_c + gender + fas_sum_pre + rki_sum_pre + (1|school), # mediation path model with moderator
                                data = df_all_cases,
                                REML = F)

fit.direct.mod.change <- glmer(dshi_change ~ condition*fever_c + ders_c + gender + fas_sum_pre + rki_sum_pre + (1|school), # direct effect model with moderator
                               family = binomial(link = "logit"),
                               data = df_all_cases)

summary(fit.path.cprime_change)
confint(fit.path.cprime_change,parm="beta_",method="Wald")

summary(fit.mediator.mod.change)
confint(fit.mediator.mod.change)

summary(fit.direct.mod.change)
confint(fit.direct.mod.change,parm="beta_",method="Wald")

##  simple slope analysis
fit.mediator.lowfever.change <- lmer(ders_c ~ condition*fever_p1SD + gender + fas_sum_pre + rki_sum_pre + (1|school), # condition effect for low levels of RTC
                                     data = df_all_cases,
                                     REML = F)
fit.mediator.highfever.change <- lmer(ders_c ~ condition*fever_m1SD + gender + fas_sum_pre + rki_sum_pre + (1|school), # condition effect for high levels of RTC
                                      data = df_all_cases,
                                      REML = F)
fit.mediator.averagefever.change <- lmer(ders_c ~ condition*fever_c + gender + fas_sum_pre + rki_sum_pre + (1|school), # condition effect for average levels of RTC
                                         data = df_all_cases,
                                         REML = F)

summary(fit.mediator.lowfever)
summary(fit.mediator.highfever)
summary(fit.mediator.averagefever.change)

# estimate moderated mediation model with ::mediate
med.change.fever.high <- mediate(fit.mediator.mod.change, fit.direct.mod.change, # mediation for high levels of RTC
                          treat = "condition",
                          mediator = "ders_c",
                          covariates = list(df_all_cases$fever_m1SD))

med.change.fever.low <- mediate(fit.mediator.mod.change, fit.direct.mod.change, # mediation for low levels of RTC
                         treat = "condition",
                         mediator = "ders_c",
                         covariates = list(df_all_cases$fever_p1SD))

med.change.fever.average <- mediate(fit.mediator.mod, fit.direct.mod.change, # mediation for average levels of RTC
                             treat = "condition",
                             mediator = "ders_c",
                             covariates = list(df_all_cases$fever_c))

summary(med.change.fever.high)
summary(med.change.fever.low)
summary(med.change.fever.average)

## build simple mediation model because of no significant moderation
fit.mediator.simple.change <- lmer(ders_c ~ condition + ders_sum_pre_c + gender + fas_sum_pre + rki_sum_pre + (1|school), # mediation path without moderator
                                   data = df_all_cases,
                                   REML = F)

fit.total.simple.change <- glmer(dshi_binary_post ~ condition + dshi_binary_pre + ders_sum_pre_c + ders_sum_post_c + gender + fas_sum_pre + rki_sum_pre + (1|school), # direct effect model without moderator
                          family = binomial(link = "logit"),
                          control = glmerControl(check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4)),
                          data = df_all_cases)

## estimate mediation without moderator with ::mediate
mediation.simple.change <- mediate(fit.mediator.simple.change, fit.total.simple.change,
                            treat = "condition",
                            mediator = "ders_c",
                            boot = F,
                            sims = 500)
summary(mediation.simple.change)