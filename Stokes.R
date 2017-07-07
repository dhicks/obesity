library(broom)
library(cowplot)
library(dplyr)
library(knitr)
library(mgcv)
library(survey)
load('2016-09-20.RData')

# bmi_breaks = c(18.5, 25, 30, 35, Inf)
# names(bmi_breaks) = c('underweight', 'normal', 'overweight', 'obese I', 'obese II')

dataf = dataf %>% 
		## Drop entries with NA id or 0 sample weight
		filter(!is.na(id), sample.weight > 0) %>%
		mutate(
			## Censor mortality at the 85-years-old cutoff: 
			##  actual age at followup
			age.follow.2006 = age.months + follow.months.2006,
			age.follow.2011 = age.months + follow.months.2011,
			##  censor at 85 years old
			age.follow.2006.c = pmin(age.follow.2006, 85*12-1),
			age.follow.2011.c = pmin(age.follow.2011, 85*12-1),
			##  censored followup months
			follow.months.2006.c = age.follow.2006.c - age.months,
			follow.months.2011.c = age.follow.2011.c - age.months,
			## censored mortality
			mort.2006.c = ifelse(age.follow.2006 < 85*12, mort.2006, 1), 
			mort.2011.c = ifelse(age.follow.2011 < 85*12, mort.2011, 1),
			## Interaction term between categorical BMI variables
			bmi.cat.inter = interaction(bmi.max.cat, bmi.cat, drop = TRUE), 
			## For cross-referencing, number the rows in the master dataset
			row.num = row_number())
dataf.2006 = subset(dataf, (!smoker) & (age.months >= 50*12) & 
					  	(age.months < 85*12) & 
					  	(follow.months.2006 > 0) &
					  	(bmi.cat != 'underweight') & 
					  	(bmi < 75) & (bmi.max < 75) &
					  	!is.na(bmi) & !is.na(bmi.max) & 
					  	!is.na(education) & !is.na(mort.2006))
dataf.2011 = subset(dataf, (!smoker) & (age.months >= 50*12) & 
						(age.months < 85*12) & 
						(follow.months.2011 > 0) &
						(bmi.cat != 'underweight') & 
						(bmi < 75) & (bmi.max < 75) &
						!is.na(bmi) & !is.na(bmi.max) & 
						!is.na(education) & !is.na(mort.2011))
## These samples are exactly the same individuals
# table(dataf.2006$row.num == dataf.2011$row.num)
## There are some individuals who are in the 2006 followup but not in 2011
## But they were all 17-22 years old and in NHANES 3
# dataf %>% filter(!is.na(mort.2006), is.na(mort.2011)) %>% .$age.years %>% summary

dataf.2011 %>% 
	select(age.years, education, race.ethnicity, bmi, bmi.max, 
		   bmi.cat, bmi.max.cat, bmi.cat.inter,
		   mort.2006.c, mort.2011.c) %>% 
	summary
## Move to a survey design object
design_unfltd = svydesign(id = ~ psu, strata = ~ stratum, 
						  weights = ~ sample.weight, 
				   nest = TRUE, 
				   data = dataf)
## Filtered datasets
design.2006 = subset(design_unfltd, row.num %in% dataf.2006$row.num)
design.2011 = subset(design_unfltd, row.num %in% dataf.2011$row.num)

## Build interaction term
design.2006 = update(design.2006, bmi.cat.inter = interaction(bmi.max.cat, bmi.cat, drop = TRUE))
design.2011 = update(design.2011, bmi.cat.inter = interaction(bmi.max.cat, bmi.cat, drop = TRUE))

## ----------
## For some whole-population plots
dataf.adult = dataf %>% 
	filter(age.years >= 18, nhanes.cycle > 0)
design.adult = subset(design_unfltd, row.num %in% dataf.adult$row.num)
## KDE of BMI
ggplot(data = {svysmooth( ~ bmi, design.adult)$bmi %>% as.data.frame}, 
	   aes(x, y)) + 
	# geom_line(color = 'blue') +
	geom_ribbon(aes(ymin = 0, ymax = y), fill = 'blue', alpha = .5) +
	geom_vline(xintercept = bmi_breaks, alpha = .5) +
	xlab('BMI') + scale_y_continuous(breaks = NULL, name = '')

## 1 - CDF at some BMI values of interest
cdf = svycdf(~ bmi, design.adult)$bmi
1 - cdf(c(38, 45))
cdf(bmi_breaks) %>% diff()

## ----------
## Stokes' results
stokes = data.frame(
	term = c('overweight.normal', 'overweight.overweight', 
			 'obese I.normal', 'obese I.overweight', 'obese I.obese I', 
			 'obese II.normal', 'obese II.overweight', 'obese II.obese I', 
			 	'obese II.obese II'),
	bmi = c('normal', 'overweight', 
			'normal', 'overweight', 'obese I', 
			'normal', 'overweight', 'obese I', 'obese II'),
	bmi.max = c('overweight', 'overweight', 
				'obese I', 'obese I', 'obese I', 
				'obese II', 'obese II', 'obese II', 'obese II'), 
	estimate = c(1.69, 1.10, 
				 2.69, 1.76, 1.48, 
				 4.97, 3.06, 2.28, 1.85), 
	conf.low = c(1.12, .76, 
				 1.67, 1.16, .98, 
				 2.01, 1.72, 1.54, 1.18), 
	conf.high = c(2.56, 1.60, 
				  4.33, 2.66, 2.24, 
				  12.27, 5.44, 3.36, 2.89),
	model = 'Stokes',
	mort.data = '2006',
	model.data = 'Stokes'
)

## ----------
## Set up data frame for grid-based predictions
predictions = expand.grid(
	bmi = 19:45, 
	bmi.max = 19:45, 
	age.years = svymean(~ age.years, design.2011)[1],
	sex = factor('female', levels = levels(dataf.2011$sex)),
	race.ethnicity = factor('Non-Hispanic White', 
							levels = levels(dataf.2011$race.ethnicity)), 
	education = factor('High School', 
					   levels = levels(dataf.2011$education))) %>%
	filter(bmi <= bmi.max)

bmi_classify = function (bmi) {
	return(names(which(bmi <= bmi_breaks))[1])
}

predictions$bmi.cat = sapply(predictions$bmi, bmi_classify) %>% 
	factor(levels = names(bmi_breaks), ordered = FALSE) %>%
	C(treatment, base = 2)
predictions$bmi.max.cat = sapply(predictions$bmi.max, bmi_classify) %>% 
	factor(levels = names(bmi_breaks), ordered = FALSE) %>%
	C(treatment, base = 2)
predictions$bmi.cat.inter = with(predictions, 
								 interaction(bmi.max.cat, bmi.cat, drop = TRUE))


## ----------
## Weighted Cox PH model, categorical BMI
## Fit the model
coxfit.cat.2006 = svycoxph(Surv(age.years, mort.2006.c) ~ 
				  			sex + race.ethnicity + education +
				  			bmi.cat.inter, 
						   design = design.2006)
coxfit.cat.2011 = svycoxph(Surv(age.years, mort.2011.c) ~
						   	sex + race.ethnicity + education + 
						   	bmi.cat.inter,
						   design = design.2011)
summary(coxfit.cat.2006)
summary(coxfit.cat.2011)
## Extract coefficient estimates
estimates = list(coxfit.cat.2006 = coxfit.cat.2006, 
	 coxfit.cat.2011 = coxfit.cat.2011) %>%
	lapply(tidy) %>%
	do.call('rbind', .) %>% 
	mutate(model = 'cox.cat', mort.data = c(rep('2006', nrow(.)/2), 
											rep('2011', nrow(.)/2))) %>% 
	filter(grepl('bmi.cat.inter', term)) %>%
	mutate(term = gsub('bmi.cat.inter', '', term)) %>%
	mutate(bmi.max = stringr::str_match(term, '(^[^\\.]*)\\.')[,2], 
		   bmi = stringr::str_match(term, '\\.([^\\.]*)$')[,2]) %>%
	transmute(term = term, 
			  bmi = factor(bmi, levels = names(bmi_breaks), ordered = TRUE),
			  bmi.max = factor(bmi.max, levels = names(bmi_breaks), ordered = TRUE),
			  estimate = exp(estimate), 
			  conf.low = exp(conf.low), 
			  conf.high = exp(conf.high), 
			  model = 'coxfit.cat', 
			  mort.data = mort.data, 
			  model.data = paste(model, mort.data))
estimates = rbind(estimates, stokes)

## Tables and plots of coefficient estimates
# TODO: fix these
# coxfit.cat.sum %>% select(bmi:conf.high) %>%
# 	kable(digits = 2)
# ggplot(coxfit.cat.sum, aes(bmi.max, bmi, fill = estimate)) + 
# 	geom_tile() +
# 	scale_fill_gradient2(limits = c(0, NA), midpoint = 1, 
# 						 name = 'prop. hazard') + 
# 	geom_text(aes(label = paste(format(estimate, digits = 2), '\n',
# 								'(', format(conf.low, digits = 2), 
# 									'-', format(conf.high, digits = 2), ')',
# 								sep = '')))

## Compare w/ Stokes' estimates
ggplot(estimates,
	   aes(x = bmi, y = estimate, 
	   	ymin = conf.low, ymax = conf.high, 
	   	color = model.data, 
	   	group = model.data)) + 
	#geom_linerange(position = 'dodge') + geom_point(position = 'dodge') + 
	#geom_errorbar(width = .25, position = position_dodge(width = .25)) + 
	geom_linerange(position = position_dodge(width = .25), size = 1) +
	geom_point(position = position_dodge(width = .25)) +
	geom_line(position = position_dodge(width = .25)) +
	geom_hline(yintercept = 1) +
	scale_color_brewer(palette = 'Set1', name = 'model x data') +
	coord_flip(ylim = c(0, 5)) + 
	facet_grid( ~ bmi.max)

## Make predictions
coxfit.cat.2006.pred = predict(coxfit.cat.2006, predictions, type = 'risk', se.fit = TRUE)
predictions = predictions %>% mutate(
	coxfit.cat.2006.fit = coxfit.cat.2006.pred$fit,
	coxfit.cat.2006.se = coxfit.cat.2006.pred$se.fit)
## Plot predictions
ggplot(data = predictions, aes(bmi, coxfit.cat.2006.fit, fill = bmi.max.cat)) + 
	geom_ribbon(aes(ymin = coxfit.cat.2006.fit - 2*coxfit.cat.2006.se, ymax = coxfit.cat.2006.fit + 2*coxfit.cat.2006.se), alpha = .1) +
	# geom_segment(aes(x = bmi, xend = bmi,
	# 				 y = coxfit.cat.2006.fit + qnorm(.025) * coxfit.cat.se,
	# 				 yend = coxfit.cat.2006.fit + qnorm(.975) * coxfit.cat.se),
	# 			 alpha = .5) +
	geom_line(aes(color = bmi.max.cat), alpha = 1) +
	geom_vline(xintercept = bmi_breaks, color = 'grey') +
	geom_hline(yintercept = 1) +
	ylab('proportional hazard') + 
	coord_flip(xlim = c(18.5, 50), ylim = c(0, 3))


# coxfit.cat.pred = predict(coxfit.cat, design$variables, type = 'risk', se.fit = TRUE)
# design = update(design, coxfit.cat.fit = coxfit.cat.pred$fit,
# 				coxfit.cat.se = coxfit.cat.pred$se.fit)
# ## Plot predictions
# ggplot(data = design$variables, aes(bmi, coxfit.cat.fit, color = sex, fill = sex)) + 
# 	#geom_ribbon(aes(ymin = coxfit.cont.fit - 2*coxfit.cont.se, ymax = coxfit.cont.fit + 2*coxfit.cont.se), alpha = .25) +
# 	geom_segment(aes(x = bmi, xend = bmi, 
# 					 y = coxfit.cat.fit + qnorm(.025) * coxfit.cat.se, 
# 					 yend = coxfit.cat.fit + qnorm(.975) * coxfit.cat.se), 
# 				 alpha = .5) +
# 	#geom_point(alpha = .25) + 
# 	geom_vline(xintercept = bmi_breaks, color = 'grey') +
# 	geom_hline(yintercept = 1) +
# 	facet_grid(race.ethnicity ~ bmi.max.cat) + 
# 	ylab('proportional hazard') + 
# 	coord_cartesian(xlim = c(18.5, 50), ylim = c(0, 4))
# 
# ggplot(data = design$variables, aes(x = bmi, y = bmi.max, z = coxfit.cat.fit)) +
# 	geom_vline(xintercept = bmi_breaks, alpha = .1) +
# 	geom_hline(yintercept = bmi_breaks, alpha = .1) +
# 	#geom_point(aes(color = coxfit.cont.fit)) +
# 	#scale_color_gradient(limits = c(0, 4), low = 'blue', high = 'red') +
# 	stat_summary_hex(fun = mean, bins = 70) +
# 	scale_fill_gradient(limits = c(0,4), low = 'blue', high = 'red') +
# 	facet_grid(education ~ race.ethnicity) +
# 	coord_cartesian(xlim = c(18.5, 50), ylim = c(18.5, 50))

## ----------
## Weighted Cox PH model, continuous BMI
## Fit the model
coxfit.cont = svycoxph(Surv(age.years, mort.2006.c) ~
					   	sex + race.ethnicity + education + 
					   	bmi * bmi.max,
					   	#I(bmi - 18) * I(bmi.max - 18), 
					   design = design.2006)
summary(coxfit.cont)

km.obs = svykm(Surv(age.years, mort.2006.c) ~ 1, 
			   design = design.2006, se = FALSE)
km.model = survfit(coxfit.cont, se.fit = TRUE)
kms = data.frame(
		age = km.obs$time[-1],
		obs = km.obs$surv[-1],
		model.pe = km.model$surv,
		model.upper = km.model$upper, 
		model.lower = km.model$lower)
ggplot(kms) + 
	geom_line(aes(age, obs), color = 'black') +
	geom_line(aes(age, model.pe), color = 'red') +
	geom_ribbon(aes(age, ymin = model.lower, ymax = model.upper), fill = 'red', 
				alpha = .25)

		
	
## Make predictions
coxfit.cont.pred = predict(coxfit.cont, predictions, type = 'lp', se.fit = TRUE)
## Set the first entry (19 BMI, 19 max BMI) to 1
coxfit.cont.ref = coxfit.cont.pred$fit[1]
predictions = predictions %>% mutate(
	coxfit.cont.fit = exp(coxfit.cont.pred$fit - coxfit.cont.ref), 
	coxfit.cont.025 = coxfit.cont.fit / exp(1.96 * coxfit.cont.pred$se),
	coxfit.cont.975 = coxfit.cont.fit * exp(1.96 * coxfit.cont.pred$se))

## Plot predictions
## Points at bmi x bmi.max; PH by color w/ contours and labels
plot = ggplot(data = predictions, aes(bmi, bmi.max, z = coxfit.cont.fit)) + 
	geom_contour(aes(color = ..level..)) +
	geom_point(aes(color = coxfit.cont.fit), alpha = .1) +
	#geom_raster(aes(color = coxfit.cont.fit)) +
	# scale_fill_gradient(limits = c(0, 3), low = 'yellow', high = 'red', 
	# 					name = 'relative hazard') +
	scale_color_gradient(limits = c(0, 2), low = 'blue', high = 'red',
						 name = 'relative hazard') +
	geom_vline(xintercept = bmi_breaks, color = 'grey') +
	geom_hline(yintercept = bmi_breaks, color = 'grey') +
	stat_function(fun = function (x) {x}, color = 'grey') +
	coord_cartesian(xlim = c(18.5, 45), ylim = c(18.5, 45))
directlabels::direct.label(plot, method = 'bottom.pieces', debug = FALSE)

## PH x bmi; bmi.max by colored curves; w/ confidence intervals
{ggplot(data = filter(predictions, bmi.max <= 45),
		aes(bmi, coxfit.cont.fit)) + 
	## Prediction uncertainty ribbon
	geom_ribbon(aes(ymin = coxfit.cont.025,
					ymax = coxfit.cont.975,
					fill = bmi.max, group = bmi.max), alpha = .02) +
	geom_line(aes(color = bmi.max, group = bmi.max), size = 1, alpha = .5) + 
	geom_vline(xintercept = bmi_breaks, alpha = .25) +
	geom_hline(yintercept = 1) +
	scale_color_continuous(low = 'blue', high = 'red',
						   limits = c(18.5, 45), breaks = c(18.5, 25, 30, 35)) +
	scale_fill_continuous(low = 'blue', high = 'red',
						  limits = c(18.5, 45), breaks = c(18.5, 25, 30, 35)) +
	ylab('relative risk') #+
	#coord_flip(ylim = c(.5, 2))
	} %>%
	directlabels::direct.label('last.points', debug = FALSE)

## ----------
## Weighted Cox PH model, continuous BMI, with splines on the BMI terms
## Fit the model
coxfit.spline = svycoxph(Surv(age.years, mort.2006.c) ~
					   	sex + race.ethnicity + education + 
					   	pspline(bmi * bmi.max, nterm = 4),
					   #I(bmi - 18) * I(bmi.max - 18), 
					   design = design.2006)
summary(coxfit.spline)

## Make predictions
coxfit.spline.pred = predict(coxfit.spline, predictions, type = 'risk', se.fit = TRUE)
## Set the first entry (19 BMI, 19 max BMI) to 1
coxfit.spline.ref = coxfit.spline.pred$fit[1]
predictions = predictions %>% mutate(
	coxfit.spline.fit = exp(coxfit.spline.pred$fit - coxfit.spline.ref), 
	coxfit.spline.025 = coxfit.spline.fit / exp(1.96 * coxfit.spline.pred$se),
	coxfit.spline.975 = coxfit.spline.fit * exp(1.96 * coxfit.spline.pred$se))

## Plot predictions
## Points at bmi x bmi.max; PH by color w/ contours and labels
plot = ggplot(data = predictions, aes(bmi, bmi.max, z = coxfit.spline.fit)) + 
	geom_contour(aes(color = ..level..)) +
	geom_point(aes(color = coxfit.spline.fit), alpha = .1) +
	#geom_raster(aes(color = coxfit.spline.fit)) +
	# scale_fill_gradient(limits = c(0, 3), low = 'yellow', high = 'red', 
	# 					name = 'relative hazard') +
	scale_color_gradient(limits = c(0, 2), low = 'blue', high = 'red',
						 name = 'relative hazard') +
	geom_vline(xintercept = bmi_breaks, color = 'grey') +
	geom_hline(yintercept = bmi_breaks, color = 'grey') +
	stat_function(fun = function (x) {x}, color = 'grey') +
	coord_cartesian(xlim = c(18.5, 45), ylim = c(18.5, 45))
directlabels::direct.label(plot, method = 'bottom.pieces', debug = FALSE)

## PH x bmi; bmi.max by colored curves; w/ confidence intervals
{ggplot(data = filter(predictions, bmi.max <= 45),
		aes(bmi, coxfit.spline.fit)) + 
	## Prediction uncertainty ribbon
	geom_ribbon(aes(ymin = coxfit.spline.025,
					ymax = coxfit.spline.975,
					fill = bmi.max, group = bmi.max), alpha = .02) +
	geom_line(aes(color = bmi.max, group = bmi.max), size = 1, alpha = .5) + 
	geom_vline(xintercept = bmi_breaks, alpha = .25) +
	geom_hline(yintercept = 1) +
	scale_color_continuous(low = 'blue', high = 'red',
						   limits = c(18.5, 45), breaks = c(18.5, 25, 30, 35)) +
	scale_fill_continuous(low = 'blue', high = 'red',
						  limits = c(18.5, 45), breaks = c(18.5, 25, 30, 35)) +
	ylab('relative risk') +
	coord_cartesian(ylim = c(0.5,2))
	} %>%
	directlabels::direct.label('last.points', debug = FALSE)

## Compare continuous and continuous-spline predictions
ggplot(predictions, aes(coxfit.cont.fit, coxfit.spline.fit)) + 
	geom_point(aes(color = bmi)) + 
	# geom_smooth(method = 'lm', color = 'red') +
	stat_function(fun = function (x) x)

## ----------
## Compare categorical and continuous Cox PH predictions
# ggplot(data = predictions, aes(x = bmi, y = bmi.max, 
# 									z = coxfit.cont.fit - coxfit.cat.fit)) +
# 	geom_vline(xintercept = bmi_breaks, alpha = .1) +
# 	geom_hline(yintercept = bmi_breaks, alpha = .1) +
# 	#geom_point(aes(color = coxfit.cont.fit)) +
# 	#scale_color_gradient(limits = c(0, 4), low = 'blue', high = 'red') +
# 	stat_summary_2d(fun = mean, bins = 70) +
# 	scale_fill_gradient2(limits = c(-3, 3), low = 'blue', mid = 'yellow', high = 'red') +
# 	# facet_grid(education ~ race.ethnicity) +
# 	coord_cartesian(xlim = c(18.5, 50), ylim = c(18.5, 50))


## ----------
## Weighted binomial regression, continuous BMI
binomfit.cont = svyglm(I(mort.2006.c - 1) ~ age.years + sex + race.ethnicity + education +
					   	bmi * bmi.max, 
					   design = design.2006, 
					   family = quasibinomial())
summary(binomfit.cont)
## Make predictions
binomfit.cont.pred = predict(binomfit.cont, predictions, type = 'response', se.fit = TRUE)
predictions = predictions %>% mutate(
	binomfit.cont.fit = as.vector(binomfit.cont.pred),
	binomfit.cont.se = attr(binomfit.cont.pred, 'var'))

## Plot predictions
{ggplot(data = filter(predictions, bmi.max <= 45),
		aes(bmi, binomfit.cont.fit)) + 
	## Prediction uncertainty ribbon
	geom_ribbon(aes(ymin = binomfit.cont.fit - 2*binomfit.cont.se,
					ymax = binomfit.cont.fit + 2*binomfit.cont.se,
					fill = bmi.max, group = bmi.max), alpha = .25) +
	geom_line(aes(color = bmi.max, group = bmi.max), size = 1) + 
	geom_vline(xintercept = bmi_breaks, alpha = .25) +
	#geom_hline(yintercept = 1) +
	scale_color_continuous(low = 'blue', high = 'red',
						   limits = c(18.5, 45), breaks = c(18.5, 25, 30, 35)) +
	scale_fill_continuous(low = 'blue', high = 'red',
						  limits = c(18.5, 45), breaks = c(18.5, 25, 30, 35)) +
	ylab('mortality probability') +
	coord_flip()} %>%
	directlabels::direct.label('top.points', debug = FALSE)


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
# binomfit.cat = svyglm(mort.censored ~ bmi.cat.inter + 
# 				   	sex + age.years + race.ethnicity + education, 
# 				  family = quasibinomial(), 
# 				  design = design)
# summary(binomfit.cat)
# 
# ## Continuous BMI
# binomfit.cont = svyglm(mort.censored ~ bmi * bmi.max + 
# 						sex + age.years + race.ethnicity + education, 
# 					family = quasibinomial(), 
# 					design = design)
# summary(binomfit.cont)

