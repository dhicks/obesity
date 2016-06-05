#source('load data.R')

library(cowplot)
library(dplyr)
library(mgcv)
#library(survival)
library(survey)
load('2016-05-24.RData')

## TODO: df -> dataf
## TODO: predictions

## Drop one entry with NA id
df = df %>% filter(!is.na(id))
## Move to a survey design object
design_unfltd = svydesign(id = ~ psu, strata = ~ stratum, weights = ~ sample.weight, 
				   nest = TRUE, 
				   data = df)
## Filtered dataset: nonsmokers, 50-84 years old, not underweight
design = subset(design_unfltd, (!smoker) & (age.months >= 50*12) & (age.months < 85*12) 
					 & (bmi.cat != 'underweight')
					 )
## Build interaction term
design = update(design, bmi.cat.inter = interaction(bmi.cat, bmi.max.cat, drop = TRUE))

## TODO: summary statistics

## ----------
## Weighted survival analysis, categorical BMI
## Fit the model
coxfit.cat = svycoxph(Surv(age.years, mort.status == 'deceased') ~ 
				  			sex + race.ethnicity + education +
				  			bmi.cat.inter, 
						   design = design)
summary(coxfit.cat)

## Weighted survivial analysis, continuous BMI
## Fit the model
coxfit.cont = svycoxph(Surv(age.years, mort.status == 'deceased') ~
					   	sex + race.ethnicity + education + 
					   	bmi * bmi.max, 
					   design = design)
summary(coxfit.cont)

# ## ----------
# ## Unweighted survival analysis
# ## Define the survival object
# survobj = with(df, Surv(age.months, mort.status == 'deceased'))
# ## Curves by BMI category at examination
# # fit = survfit(survobj~bmi.cat, data = df)
# # survdiff(survobj~bmi.cat, data = df)
# 
# ## Cox proportional hazards models
# ## Set this option to get desired behavior w/ ordered factors
# #options(contrasts = c("contr.treatment", "contr.poly"))
# ## Breaking BMI and max BMI into categories
# coxfit.cat = coxph(survobj ~ sex + race.ethnicity + education +
# 				   bmi.cat + bmi.max.cat,
# 				   data = df)
# summary(coxfit.cat)
# ## Continuous BMI and max BMI
# coxfit.cont = coxph(survobj ~ sex + race.ethnicity + education + 
# 						bmi + bmi.max, 
# 					data = df)
# summary(coxfit.cont)


## ----------
## LOESS
# ggplot(data = df, aes(x = bmi, y = as.numeric(mort.status) - 1,
# 					  color = race.ethnicity,
# 					  shape = sex, linetype = sex)) +
# 	geom_point(position = position_jitter(height = .25), alpha = .1) +
# 	stat_smooth(alpha = .2) +
# 	#geom_vline(xintercept = bmi_breaks, color = 'grey') +
# 	scale_y_continuous(name = 'mortality', breaks = c(0,1),
# 					   labels = c('alive', 'deceased')) +
# 	scale_x_continuous(limits = c(NA, 50)) +
# 	facet_wrap(~ race.ethnicity)

## ----------
## GAM
## TODO: try something like this: http://stackoverflow.com/questions/16569299/r-svyglm-prediction-with-spline-function

# gamfit = gam(mort.status ~ bmi.cat * bmi.max.cat + 
# 			 	sex + age.months + race.ethnicity + education, 
# 			 data = df)

## ----------
## Binomial regression
## Categorical BMI
binomfit.cat = svyglm(mort.status ~ bmi.cat.inter + 
				   	sex + age.years + race.ethnicity + education, 
				  family = quasibinomial(), 
				  design = design)
summary(binomfit.cat)

## Continuous BMI
binomfit.cont = svyglm(mort.status ~ bmi * bmi.max + 
						sex + age.years + race.ethnicity + education, 
					family = quasibinomial(), 
					design = design)
summary(binomfit.cont)

