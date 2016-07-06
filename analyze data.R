library(cowplot)
library(dplyr)
library(mgcv)
library(survey)
load('2016-07-06.RData')

## TODO: predictions

bmi_breaks = c(18.5, 25, 30, 35, Inf)
names(bmi_breaks) = c('underweight', 'normal', 'overweight', 'obese I', 'obese II')

## Drop one entry with NA id
dataf = dataf %>% filter(!is.na(id))
## Move to a survey design object
design_unfltd = svydesign(id = ~ psu, strata = ~ stratum, weights = ~ sample.weight, 
				   nest = TRUE, 
				   data = dataf)
## Filtered dataset: nonsmokers, 50-84 years old, not underweight, 
##   mortality followup as of December 31, 2006
design = subset(design_unfltd, (!smoker) & (age.months >= 50*12) & 
					(age.months < 85*12) & 
					(bmi.cat != 'underweight') & 
					(mort.followup.date.est <= '2006-12-31')
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
## Make predictions
coxfit.cat.pred = predict(coxfit.cat, design$variables, type = 'risk', se.fit = TRUE)
design = update(design, coxfit.cat.fit = coxfit.cat.pred$fit,
				coxfit.cat.se = coxfit.cat.pred$se.fit)
## Plot predictions
ggplot(data = design$variables, aes(bmi, coxfit.cat.fit, color = sex, fill = sex)) + 
	#geom_ribbon(aes(ymin = coxfit.cont.fit - 2*coxfit.cont.se, ymax = coxfit.cont.fit + 2*coxfit.cont.se), alpha = .25) +
	geom_segment(aes(x = bmi, xend = bmi, 
					 y = coxfit.cat.fit + qnorm(.025) * coxfit.cat.se, 
					 yend = coxfit.cat.fit + qnorm(.975) * coxfit.cat.se), 
				 alpha = .5) +
	#geom_point(alpha = .25) + 
	geom_vline(xintercept = bmi_breaks, color = 'grey') +
	geom_hline(yintercept = 1) +
	facet_grid(race.ethnicity ~ bmi.max.cat) + 
	ylab('proportional hazard') + 
	coord_cartesian(xlim = c(18.5, 50), ylim = c(0, 4))

## TODO: plot

## Weighted survivial analysis, continuous BMI
## Fit the model
coxfit.cont = svycoxph(Surv(age.years, mort.status == 'deceased') ~
					   	sex + race.ethnicity + education + 
					   	bmi * bmi.max, 
					   design = design)
summary(coxfit.cont)
## Make predictions
coxfit.cont.pred = predict(coxfit.cont, design$variables, type = 'risk', se.fit = TRUE)
design = update(design, coxfit.cont.fit = coxfit.cont.pred$fit, 
				coxfit.cont.se = coxfit.cont.pred$se.fit)
## Plot predictions
ggplot(data = design$variables, aes(bmi, coxfit.cont.fit, color = sex, fill = sex)) + 
	#geom_ribbon(aes(ymin = coxfit.cont.fit - 2*coxfit.cont.se, ymax = coxfit.cont.fit + 2*coxfit.cont.se), alpha = .25) +
	geom_segment(aes(x = bmi, xend = bmi, 
					 y = coxfit.cont.fit + qnorm(.025) * coxfit.cont.se, 
					 yend = coxfit.cont.fit + qnorm(.975) * coxfit.cont.se), 
				 alpha = .5) +
	#geom_point(alpha = .25) + 
	geom_vline(xintercept = bmi_breaks, color = 'grey') +
	geom_hline(yintercept = 1) +
	facet_wrap( ~ race.ethnicity) + 
	ylab('proportional hazard') + 
	coord_cartesian(xlim = c(18.5, 50), ylim = c(0, 4))



# ## ----------
# ## Unweighted survival analysis
# ## Define the survival object
# survobj = with(dataf, Surv(age.months, mort.status == 'deceased'))
# ## Curves by BMI category at examination
# # fit = survfit(survobj~bmi.cat, data = dataf)
# # survdiff(survobj~bmi.cat, data = dataf)
# 
# ## Cox proportional hazards models
# ## Set this option to get desired behavior w/ ordered factors
# #options(contrasts = c("contr.treatment", "contr.poly"))
# ## Breaking BMI and max BMI into categories
# coxfit.cat = coxph(survobj ~ sex + race.ethnicity + education +
# 				   bmi.cat + bmi.max.cat,
# 				   data = dataf)
# summary(coxfit.cat)
# ## Continuous BMI and max BMI
# coxfit.cont = coxph(survobj ~ sex + race.ethnicity + education + 
# 						bmi + bmi.max, 
# 					data = dataf)
# summary(coxfit.cont)


## ----------
## LOESS
# ggplot(data = dataf, aes(x = bmi, y = as.numeric(mort.status) - 1,
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
# 			 data = dataf)

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

