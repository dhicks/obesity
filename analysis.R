#' ---
#' 	title: "Analysis of BMI and All-Cause Morality in NHANES III and NHANES 1999-2004"
#' ---
#' 	
#' 	# Setup and Data #
#' 	
#' We first load several required `R` packages and previously cleaned data.  Data come from NHANES III and continuous NHANES (cycles 1-3, 1999-2004), and followup mortality data from 2006 and 2011.  The model below uses only 2006 mortality data.  

#+ setup, message=FALSE, warning=FALSE
## tidyverse
library(tidyverse)
library(cowplot)
library(broom)
library(stringr)
library(purrr)
## complex survey designs + survival analysis
library(survival)
library(survey)
## spline models
library(splines)

load('2016-10-06.RData')
## TODO: why does't bmi_breaks come with names?
names(bmi_breaks) = c('NA', 'underweight', 'normal', 
					  'overweight', 'obese I', 'obese II')

age_cutoff = 85

## Age variables are measured in months

dataf_unfltd = dataf %>% 
	## Drop entries with NA id or 0 sample weight
	filter(!is.na(id), sample.weight > 0) %>%
	mutate(
		## Censor mortality at the 70-years-old cutoff: 
		##  actual age at followup
		age.follow = age.months + follow.months.2011,
		##  censor at 70 years old
		age.follow.c = pmin(age.follow, age_cutoff*12+11),
		##  censored followup months
		follow.months.c = age.follow.c - age.months,
		## censored mortality
		## 0 = alive/censored, 1 = deceased
		mort.c = ifelse(age.follow <= age_cutoff*12+11, 
						as.integer(mort.2011) - 1, 0),
		## For cross-referencing, number the rows in the master dataset
		row.num = row_number())

#+ subset_data
## The subset of data to work with
dataf = subset(dataf_unfltd, (!smoker) & (age.months >= 50*12 + 12) & 
						(age.months < age_cutoff*12 + 12) & 
						(follow.months.2011 > 0) &
						# (bmi.cat != 'underweight') &
						(bmi < 75) & 
						!is.na(bmi) &
						!is.na(education) & !is.na(mort.c))

## descriptive statistics for each variable in the dataset
summary(dataf)

## Move to a survey design object
design_unfltd = svydesign(id = ~ psu, strata = ~ stratum, 
						  weights = ~ sample.weight, 
						  nest = TRUE, 
						  data = dataf_unfltd)
design = subset(design_unfltd, row.num %in% dataf$row.num)

#' # Models #
#' We examine three key decisions in constructing and evaluating models for these data:  
#' 
#' - *BMI*: (1) conventionally binned as "normal," "overweight," "obese I," and "obese II"; (2) as a continuous variable; (3) as a continuous variable, with a four-spline B-spline basis; (4) as a continuous variable, with a six-spline B-spline basis [see documentation for `survival::pspline`]
#' - *Model specification*: (1) a Cox proportional hazards model; (2) a generalized linear regression, using a logistic link function; (3) a generalized linear regression, using a complementary log (cloglog) link function.  
#' - *Model selection* or goodness-of-fit evaluation using (1) deviance, (2) accuracy, (3) F1, and (4) AUC. 
#' 
#' The first two sets of decisions give us a total of 12 different models.  

#+ fit_models
fit_model = function (design, variable, specification) {
	var = switch(variable, 
				 'binned' = 'bmi.cat',
				 'continuous' = 'bmi',
				 '4-spline' = 'ns(bmi, df = 5)', 
				 '6-spline' = 'ns(bmi, df = 7)')
	if (specification == 'cox') {
		rhs = str_c(var, ' + sex + race.ethnicity + education')
	} else {
		rhs = str_c(var, ' + age.follow.c + sex + race.ethnicity + education')
	}
	
	
	response = switch(specification, 
					  'cox' = 'Surv(age.follow.c, mort.c)',
					  'logistic' = 'mort.c',
					  'cloglog' = 'mort.c')
	form = str_c(response, ' ~ ', rhs)
	
	expr = switch(specification, 
				  'cox' = as.expression(str_c(
				  	'svycoxph(', form, ', ',
				  		'design = design)')),
				  'logistic' = as.expression(str_c(
				  	'svyglm(', form, ', ',
				  		'design = design, ', 
				  		'family = quasibinomial(link = "logit"))')),
				  'cloglog' = as.expression(str_c(
				  	'svyglm(', form, ', ',
				  	'design = design, ', 
				  	'family = quasibinomial(link = "cloglog"))')))
	
	return(parse(text = expr))
	# eval(parse(text = expr))
}

models_df = cross_d(list('variable' = c('binned', 'continuous', 
							'4-spline', '6-spline'),
	   'specification' = c('cox', 'logistic', 'cloglog'))) %>%
	mutate(model_id = row_number()) %>%
	select(model_id, everything()) %>%
	by_row(function (df) fit_model(design, df$variable, df$specification), 
		   .to = 'model_expr')

models = models_df$model_expr %>%
	map(eval)

#+ analyze_models
coefs = models %>%
	map(tidy, conf.int = TRUE) %>%
	bind_rows(.id = 'model_id') %>%
	mutate(model_id = as.integer(model_id)) %>%
	left_join(models_df, .) %>%
	mutate(conf.low.calc = estimate - qnorm(.975)*std.error, 
		   conf.high.calc = estimate + qnorm(.975)*std.error)

coefs %>%
	filter(str_detect(term, 'bmi')) %>%
	ggplot(aes(term, estimate, color = variable, 
			   linetype = specification, shape = specification)) + 
	# geom_point(position = position_dodge(width = .25)) +
	geom_pointrange(aes(ymin = conf.low.calc, ymax = conf.high.calc), 
					position = position_dodge(width = .5)) +
	coord_flip()

coefs %>%
	filter(!str_detect(term, 'bmi')) %>%
	ggplot(aes(term, estimate, color = variable, 
			   linetype = specification, shape = specification)) +
	geom_pointrange(aes(ymin = conf.low.calc, ymax = conf.high.calc),
					position = position_dodge(width = .5)) +
	coord_flip()

## ----------------------------------------
#' ## Model Accuracy ##
## TODO: 
## `augment.coxph` doesn't return mort.c, but instead a numeric 
## corresponding to age.years, which doesn't actually have the 
## survival data
aug = models %>%
	map(augment, type.predict = 'expected', type = 'response') %>%
	bind_rows(.id = 'model_id') %>% 
	mutate(model_id = as.integer(model_id)) %>%
	right_join(models_df, .) %>%
	## For Cox models, survival probability = exp(-expected)
	mutate(prob = ifelse(specification != 'cox', 
						 .fitted, 
						 1 - exp(-.fitted)))

ggplot(aug, aes(prob)) + 
	stat_ecdf() + 
	geom_hline(yintercept = 1 - sum(dataf$mort.c)/nrow(dataf)) +
	facet_grid(variable ~ specification)

aug %>%
	group_by(model_id, variable, specification) %>%
	summarize(q = quantile(prob, probs = 1 - sum(mort.c)/n()))

## TODO: 
## Maybe try the AUC package to quickly generate AUC statistics? 
aug %>%
	mutate(prediction = ifelse(prob > .3, 1, 0)) %>%
	group_by(variable, specification) %>%
	by_slice(function (df) table(df$prediction, df$mort.c)) %>%
	.$.out

## ----------------------------------------
#' ## Predictions ##
## Initialize a data frame to hold the predictions
predictions = expand.grid(
	bmi = 16:45, 
	age.follow.c = svymean(~ age.follow.c, design)[1],
	sex = factor('female', levels = levels(dataf$sex)),
	race.ethnicity = factor('Non-Hispanic White', 
							levels = levels(dataf$race.ethnicity)), 
	education = factor('High School', 
					   levels = levels(dataf$education)))

bmi_classify = function (bmi) {
	return(names(which(bmi <= bmi_breaks))[1])
}
bmi_classify = Vectorize(bmi_classify)

predictions = predictions %>%
	mutate(bmi.cat = {bmi %>% bmi_classify %>%
			factor(levels = names(bmi_breaks), ordered = FALSE) %>%
			C(treatment, base = 2)})
predictions = models %>%
	map_if(function (x) 'svycoxph' %in% class(x), 
		   predict, predictions, type = 'risk', se = TRUE) %>%
	map_if(function (x) 'svyglm' %in% class(x),
		   predict, predictions, type = 'response') %>%
	map(as_tibble) %>%
	map(`names<-`, c('fit', 'se')) %>%
	map(bind_cols, predictions) %>%
	bind_rows(.id = 'model_id') %>%
	mutate(model_id = as.integer(model_id)) %>%
	left_join(models_df)

## Rescale predictions to baseline BMI = 22
predictions = predictions %>%
	group_by(model_id) %>%
	filter(bmi == 22) %>%
	select(model_id, baseline = fit) %>%
	right_join(predictions) %>%
	mutate(risk_rel = fit / baseline)

## Attach confidence intervals
predictions = predictions %>%
	mutate(conf.high = risk_rel * exp(qnorm(.975)*se), 
		   conf.low = risk_rel / exp(qnorm(.975)*se))

## Plot predictions
ggplot(predictions, aes(bmi, risk_rel, 
						fill = variable,
						linetype = specification)) + 
	geom_hline(yintercept = 1) +
	geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
				alpha = .25) +
	geom_line(aes(color = variable), size = 1) +
	geom_point(x = mean(c(bmi_breaks['normal'], bmi_breaks['underweight'])), 
			   y = 1) +
	scale_x_continuous(breaks = bmi_breaks, 
					   labels = bmi_breaks) +
	geom_vline(xintercept = bmi_breaks[2:5], alpha = .1) +
	coord_cartesian(ylim = c(0, 2)) +
	facet_grid(~ variable)

## ----------------------------------------
## Old stuff below here
#+ eval = FALSE
coxfit.cat = svycoxph(Surv(age.years, mort.c) ~ 
					  	sex +
					  	race.ethnicity +
					  	education +
					  	bmi.cat, 
					  design = design)
summary(coxfit.cat)

## ------------------------------------------------------------------------
## Observed Kaplan-Meyer curve
km.obs = svykm(Surv(age.years, mort.2006.c) ~ 1, 
			   design = design.2006, se = FALSE)
## Kaplan-Meyer curve predicted from the model
km.model = survfit(coxfit.cat, se.fit = TRUE)
## Combine the two and plot
kms = data.frame(
	age = km.obs$time[-1],
	obs = km.obs$surv[-1],
	model.pe = km.model$surv,
	model.upper = km.model$upper, 
	model.lower = km.model$lower)
kms.plot = ggplot(kms) + 
	geom_line(aes(age, obs), color = 'black') +
	geom_line(aes(age, model.pe), color = 'red') +
	geom_ribbon(aes(age, ymin = model.lower, ymax = model.upper), fill = 'red', 
				alpha = .25)
kms.plot

## ------------------------------------------------------------------------
res.df = data.frame(
	age = coxfit.cat$model$`Surv(age.years, mort.2006.c)`[,1],
	prediction = coxfit.cat$linear.predictors,
	residual = residuals(coxfit.cat, type = 'deviance')
	)
ggplot(res.df, aes(prediction, residual)) + geom_point(position = 'jitter') +
	geom_smooth()
ggplot(res.df, aes(age, residual)) + geom_point() + geom_smooth()

## ------------------------------------------------------------------------
coeffs = tidy(coxfit.cat) %>%
	transmute(term = term, 
			  estimate = exp(estimate),
			  conf.low = exp(conf.low),
			  conf.high = exp(conf.high)) %>%
	mutate(term = str_replace(term, 'sex', ''),
		   term = str_replace(term, 'race.ethnicity', ''),
		   term = str_replace(term, 'education', ''),
		   term = str_replace(term, 'bmi.cat1', 'underweight'),
		   term = str_replace(term, 'bmi.cat3', 'overweight'),
		   term = str_replace(term, 'bmi.cat4', 'obese I'), 
		   term = str_replace(term, 'bmi.cat5', 'obese II')) %>%
	mutate(term = factor(term, levels = term, ordered = TRUE), 
		   group = c('sex', 
		   		  rep('race.ethnicity', 3), 
		   		  rep('education', 4), 
		   		  rep('bmi', 4)))

ggplot(coeffs, aes(term, estimate, color = group)) + 
	geom_hline(yintercept = 1) +
	geom_point() + 
	geom_linerange(aes(ymin = conf.low, ymax = conf.high)) +
	coord_flip()

