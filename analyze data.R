#source('load data.R')

library(cowplot)
library(dplyr)
library(survival)
load('2016-05-17.RData')

## Filter down to complete cases
df = df[complete.cases(df), ]
## Filter down to non-smokers between 50 and 85
df = df %>% filter(!smoker, age.months >= 50*12, age.months <= 85*12)

## ----------
## Survival analysis
## Define the survival object
survobj = with(df, Surv(age.months, mort.status == 'deceased'))
## Curves by BMI category at examination
fit = survfit(survobj~bmi.cat, data = df)
survdiff(survobj~bmi.cat, data = df)

## Cox proportional hazards models
## Set this option to get desired behavior w/ ordered factors
options(contrasts = c("contr.treatment", "contr.treatment"))
## Breaking BMI and max BMI into categories
coxfit.cat = coxph(survobj ~ sex + race.ethnicity + 
				   	bmi.cat + bmi.max.cat,
				   data = df)
summary(coxfit.cat)
## Continuous BMI and max BMI
coxfit.cont = coxph(survobj ~ sex + race.ethnicity + bmi * bmi.max, data = df)
summary(coxfit.cont)


## ----------
## LOESS analysis
# ggplot(data = df, aes(x = bmi, y = as.numeric(mort.status) - 1, 
# 					  color = race.ethnicity, 
# 					  shape = sex, linetype = sex)) +
# 	geom_point(position = position_jitter(height = .25), alpha = .1) +
# 	stat_smooth(alpha = .2) +
# 	geom_vline(xintercept = bmi_breaks, color = 'grey') +
# 	scale_y_continuous(name = 'mortality', breaks = c(0,1), 
# 					   labels = c('alive', 'deceased')) +
# 	scale_x_continuous(limits = c(NA, 50)) +
# 	facet_wrap(~ race.ethnicity)

## ----------
## Binomial regression
# fit1 = glm(mort.status ~ bmi,
# 		   family = binomial,
# 		   data = df)
# summary(fit1)
# 
# fit2 = glm(mort.status ~ bmi + sex + age.months + race.ethnicity, 
# 		   family = binomial, 
# 		   data = df)
# summary(fit2)
# 
# fit3 = glm(mort.status ~ bmi + bmi.max + sex + age.months + race.ethnicity, 
# 		   family = binomial, 
# 		   data = df)
# summary(fit3)
# exp(fit3$coefficients)
