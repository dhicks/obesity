#' ---
#' title: "Analysis of BMI and All-Cause Morality in NHANES III and NHANES 1999-2004"
#' author: "Daniel J. Hicks and Catherine Womack"
#' email: hicks.daniel.j@gmail.com
#' ---
#'     
#' # Setup and Data #
#'     
#' We first load several required `R` packages and previously cleaned data.  Data come from NHANES III and continuous NHANES (cycles 1-3, 1999-2004), and followup mortality data from 2006 and 2011.  The models constructed below use only 2011 mortality data.  

## ----------------------------------------
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
library(AUC)
## sortable table in HTML
library(DT)

load('2016-10-06.RData')
## TODO: why does't bmi_breaks come with names?
names(bmi_breaks) = c('NA', 'underweight', 'normal', 
                      'overweight', 'obese I', 'obese II')

age_cutoff = 85
## NB Age variables are measured in months

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
        ## For cross-referencing, number the rows in the unfiltered dataset
        row.num = row_number())

#+ subset_data
## The subset of data that actually feeds into the models
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


## ----------------------------------------
#' # Model Fitting #
#' We examine three key decisions in constructing and evaluating models for these data:  
#' 
#' - *BMI*: (1) conventionally binned as "normal," "overweight," "obese I," and "obese II"; (2) as a continuous variable; (3) as a continuous variable with four cubic splines; (4) as a continuous variable with six cubic splines [see documentation for `splines::bs`]
#' - *Model specification*: (1) a Cox proportional hazards model; two binomial regressions, using (2) a logistic link function; (3) a complementary log (cloglog) link function; and (4) an "ordinary" or "vanilla" linear regression, often called ordinary least-squares (OLS), with an identity link function. 
#' - *Model selection* or goodness-of-fit evaluation using (1) deviance, (2) accuracy, (3) F1, and (4) AUC. 
#' 
#' The first two sets of decisions give us a total of 16 different models.  
#' 
#' The following function constructs unevaluated expressions for each model, based on `variable` (how BMI is represented) and `specification` (which model specification is used).  

#+ build_expr
build_expr = function (design, variable, specification) {
    knots_4 = svyquantile(~ bmi, design, 1/5*1:4) %>%
        round(digits = 2) %>%
        str_c(collapse = ', ') %>%
        str_c('c(', ., ')')
    knots_6 = svyquantile(~ bmi, design, 1/7*1:6) %>%
        round(digits = 2) %>%
        str_c(collapse = ', ') %>%
        str_c('c(', ., ')')
    var = switch(variable, 
                 'binned' = 'bmi.cat',
                 'continuous' = 'bmi',
                 '4-spline' = str_c('bs(bmi, knots = ', 
                                    knots_4, ')'), 
                 '6-spline' = str_c('bs(bmi, knots = ', 
                                    knots_6, ')'))
    if (specification == 'cox') {
        rhs = str_c(var, ' + sex + race.ethnicity + education')
    } else {
        rhs = str_c(var, ' + age.follow.c + sex + race.ethnicity + education')
    }
    
    response = switch(specification, 
                      'cox' = 'Surv(age.follow.c, mort.c)',
                      'linear' = 'mort.c',
                      'poisson' = 'mort.c',
                      'logistic' = 'mort.c',
                      'cloglog' = 'mort.c')
    form = str_c(response, ' ~ ', rhs)
    
    expr = switch(specification, 
                  'cox' = as.expression(str_c(
                      'svycoxph(', form, ', ',
                      'design = design)')),
                  'linear' = as.expression(str_c(
                      'svyglm(', form, ', ',
                      'design = design, ',
                      'family = gaussian(link = "identity"))')),
                  'logistic' = as.expression(str_c(
                      'svyglm(', form, ', ',
                      'design = design, ', 
                      'family = quasibinomial(link = "logit"))')),
                  'poisson' = as.expression(str_c(
                      'svyglm(', form, ', ',
                      'design = design, ',
                      'family = quasipoisson(link = "log"))')),
                  'cloglog' = as.expression(str_c(
                      'svyglm(', form, ', ',
                      'design = design, ', 
                      'family = quasibinomial(link = "cloglog"))'))
    )
    
    return(parse(text = expr))
}

#' The calls are kept with the model metadata, which is useful, e.g., for confirming that `build_expr` works as expected. 
models_df = cross_d(list('variable' = c('binned', 'continuous', 
                                        '4-spline', '6-spline'),
                         'specification' = c('cox', 'logistic', 
                                             'poisson', 'linear'))) %>%
    mutate(model_id = row_number()) %>%
    select(model_id, everything()) %>%
    by_row(function (df) build_expr(design, df$variable, 
                                    df$specification), 
           .to = 'model_expr')
str(models_df, max.level = 1)

#' While the fitted models can be kept (as a list variable) in the models dataframe, this causes problems with `View` and `str`.  Instead we keep them in a parallel variable.  
models = map(models_df$model_expr, eval)


## ----------------------------------------
#' # Model Analysis #
#' We analyze the models in three ways:  extracting coefficients, calculating various "accuracy" or "model selection" statistics, and generating relative risk predictions.  
#' 
#' ## Model Coefficients ##
#' For our purposes, the models' coefficients can be arranged into three groups:  coefficients related to BMI, intercept terms, and other covariates (assigned sex, assigned race-ethnicity, etc.).  The different models involve different variables, and hence incommensurable coefficients.  
#' 
#' ### BMI ###
#' Each of the four ways of representing BMI produces a distinct set of BMI variables/coefficients.  Continuous BMI involves a single coefficient, representing the ceteris paribus effect of a one-point increase in BMI.  Categorical BMI includes a coefficient for each BMI category *except* "normal," which is used as a baseline or default.  These estimates are, in effect, completely independent of each other.  The two spline representations each involve a series of coefficients, one more than the total number of splines.  These coefficients correspond to parameter values for cubic functions on overlapping subsets of the BMI values.  Because different numbers of splines involve different overlapping subsets, the values from the 4-spline models cannot be compared to the values from the 6-spline models.  Indeed, more generally, the coefficients on BMI terms are incommensurable between different representations of BMI.  
#' 
#' ### Intercepts ###
#' The two GLM models both include intercept terms, which represent constant or baseline effects.  However, the Cox model does not include an intercept, because it estimates proportional or relative risk, that is, risk relative to the baseline case.  This baseline case is not modeled explicitly, but instead stipulated to have "relative risk" of 1.0.  This is both a strength and limitation of the Cox model.  It is a strength because it means that the model does not depend on substantive assumptions about risk in the baseline case. But it is also a limitation, because it means that the model cannot estimate "absolute," non-relative risk. 
#' 
#' ### Other Covariates ###
#' The model specifications also disagree on the status of age (`age.follow.c`).  For the GLM models, age is a covariate on the right-hand side of the model specification formula:  age is an independent variable, and its value is used to predict whether or not death will occur.  For the Cox model, by contrast, age is on the left-hand side of the model:  age is part of the dependent variable, and the aim of the model is predict the age at which death will occur.  
#'  
#+ coefficients
coefs = models %>%
    map(tidy, conf.int = .95) %>%
    bind_rows(.id = 'model_id') %>%
    mutate(model_id = as.integer(model_id)) %>%
    left_join(models_df, .) %>%
    ## The next two lines calculate CIs manually
    # mutate(conf.low.calc = estimate - qnorm(.975)*std.error, 
    #        conf.high.calc = estimate + qnorm(.975)*std.error) %>%
    mutate(term_group = ifelse(str_detect(term, 'bmi'),
                               'bmi', 
                               ifelse(str_detect(term, 'Intercept'), 
                                      'intercept',
                                      'covariates')))

coefs %>%
    split(.$term_group) %>%
    map(~
            ggplot(.x, aes(term, estimate, color = variable, 
                           linetype = specification, 
                           shape = specification)) + 
            # geom_point(position = position_dodge(width = .25)) +
            geom_pointrange(aes(ymin = conf.low, 
                                ymax = conf.high), 
                            position = position_dodge(width = .75)) +
            # facet_grid(term_group ~ ., scales = 'free') +
            coord_flip()
    )

## ----------------------------------------
#' ## Model Accuracy ##
#' Again, we are interested in model evaluation/selection using 4 different statistics:  (1) AIC, (2) accuracy, (3) F1, and (4) AUC. 

## `survey` doesn't include an AIC calculation for Cox models
getAnywhere('extractAIC.svycoxph')
## So we'll define one
setMethod('extractAIC', 'svycoxph', 
          function (fit) {2*length(coef(fit)) - 2*fit$ll[2]})
## Then calculate AICs
models_df = models %>% 
    map(extractAIC) %>%
    map_if(function(x) length(x) > 1, ~ .x[['AIC']]) %>%
    combine() %>%
    tibble(model_id = 1:length(.), 
           AIC = .) %>%
    full_join(models_df, .)

## Use `augment` to generate predictions
## `augment.coxph` doesn't return mort.c, but instead a Surv variable that doesn't play nicely with the dplyr functions. To get mort.c, we need to join it with `dataf`. But this creates a bunch of non-overlapping variables (e.g., bmi and bmi.cat are NA when they're not included in a given model). So then we need to rename and remove the gappy variables. 
aug = models %>%
    ## `augment` generates the predictions
    map(augment, type.predict = 'expected', type = 'response') %>%
    bind_rows(.id = 'model_id') %>%
    ## Remove some problem variables
    select(-matches('Surv|bs.bmi')) %>%
    ## Join with `dataf`
    mutate(row.num = as.integer(.rownames)) %>%
    inner_join(dataf, by = c('row.num', 'sex', 'race.ethnicity', 
                             'education')) %>%
    ## Remove gappy bmi.x, bmi.cat.x, mort.c.x, and age.follow.c.x
    rename(bmi = bmi.y, bmi.cat = bmi.cat.y, 
           mort.c = mort.c.y, age.follow.c = age.follow.c.y) %>%
    select(-contains('.x')) %>%
    ## Join with `models_df`
    mutate(model_id = as.integer(model_id)) %>%
    right_join(models_df, .) %>%
    ## For Cox models, survival probability = exp(-expected)
    mutate(prob = ifelse(specification != 'cox', 
                         .fitted, 
                         1 - exp(-.fitted)))

## Plot the distribution of probabilities; 
## use these to select an appropriate threshold
# ggplot(aug, aes(prob)) + 
#     stat_ecdf() + 
#     geom_hline(yintercept = 1 - sum(dataf$mort.c)/nrow(dataf)) +
#     facet_grid(variable ~ specification)
aug %>%
    group_by(model_id, variable, specification) %>%
    summarize(q = quantile(prob, probs = 1 - sum(mort.c)/n()))

## Calculate evaluation statistics
models_df = aug %>%
    filter(!is.na(mort.c)) %>%
    mutate(prediction = ifelse(prob > .3, 1, 0)) %>% 
    group_by(variable, specification) %>%
    summarize(accuracy = mean(prediction == mort.c), 
              precision = sum(prediction * mort.c)/sum(mort.c), 
              recall = sum(prediction * mort.c)/sum(prediction), 
              f1 = 2*1/(1/precision + 1/recall), 
              auroc = {roc(prob, as.factor(mort.c)) %>% auc()}) %>%
    ungroup() %>%
    full_join(models_df, .)

## Sortable table FTW
models_df %>%
    select(variable, specification, 
           AIC, accuracy, f1, auroc) %>%
    datatable()

## Plot of evaluation statistics
models_df %>%
    select(variable, specification, 
           AIC, accuracy, f1, auroc) %>%
    mutate(AIC = -AIC) %>%
    gather(statistic, score, AIC:auroc) %>%
    ggplot(aes(1, score, 
               color = variable, shape = specification)) + 
    geom_point(position = position_dodge(width = .5)) + 
    scale_x_continuous(breaks = NULL, limits = c(0,2), name = '') +
    facet_wrap(~ statistic, scales = 'free')

#' TODO: what do we conclude? 

## ----------------------------------------
#' ## Predictions ##
#' TODO: write some things! 
## Initialize a data frame to hold the predictions
predictions = expand.grid(
    bmi = seq(16, 45, .1),
    age.follow.c = svymean(~ age.follow.c, design)[1],
    sex = factor('female', levels = levels(dataf$sex)),
    race.ethnicity = factor('Non-Hispanic White', 
                            levels = levels(dataf$race.ethnicity)), 
    education = factor('High School', 
                       levels = levels(dataf$education)))

## Get binned labels from the continuous BMI values
bmi_classify = function (bmi) {
    return(names(which(bmi <= bmi_breaks))[1])
}
bmi_classify = Vectorize(bmi_classify)
predictions = predictions %>%
    mutate(bmi.cat = {bmi %>% bmi_classify %>%
            factor(levels = names(bmi_breaks), ordered = FALSE) %>%
            C(treatment, base = 2)})

## Generate the predictions and arrange in a single dataframe
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
pred_plot = ggplot(predictions, aes(bmi, risk_rel, 
                                    fill = variable,
                                    linetype = specification)) + 
    geom_hline(yintercept = 1) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
                alpha = .25) +
    geom_step(aes(color = variable), size = 1) +
    geom_point(x = 22, y = 1, inherit.aes = FALSE) +
    scale_x_continuous(breaks = bmi_breaks, 
                       labels = bmi_breaks) +
    geom_vline(xintercept = bmi_breaks[2:5], alpha = .1) +
    scale_linetype_manual(values = c('solid', 'dashed', 'dotted', 'dotdash')) +
    coord_cartesian(ylim = c(.5, 2)) +
    ylab('relative risk')
pred_plot + facet_grid(~ variable)
pred_plot + facet_grid(~ specification)


#+ eval = FALSE, echo = FALSE
# ## ----------------------------------------
# ## Old stuff below here
# coxfit.cat = svycoxph(Surv(age.years, mort.c) ~ 
#                           sex +
#                           race.ethnicity +
#                           education +
#                           bmi.cat, 
#                       design = design)
# summary(coxfit.cat)
# 
# ## ------------------------------------------------------------------------
# ## Observed Kaplan-Meyer curve
# km.obs = svykm(Surv(age.years, mort.2006.c) ~ 1, 
#                design = design.2006, se = FALSE)
# ## Kaplan-Meyer curve predicted from the model
# km.model = survfit(coxfit.cat, se.fit = TRUE)
# ## Combine the two and plot
# kms = data.frame(
#     age = km.obs$time[-1],
#     obs = km.obs$surv[-1],
#     model.pe = km.model$surv,
#     model.upper = km.model$upper, 
#     model.lower = km.model$lower)
# kms.plot = ggplot(kms) + 
#     geom_line(aes(age, obs), color = 'black') +
#     geom_line(aes(age, model.pe), color = 'red') +
#     geom_ribbon(aes(age, ymin = model.lower, ymax = model.upper), fill = 'red', 
#                 alpha = .25)
# kms.plot
# 
# ## ------------------------------------------------------------------------
# res.df = data.frame(
#     age = coxfit.cat$model$`Surv(age.years, mort.2006.c)`[,1],
#     prediction = coxfit.cat$linear.predictors,
#     residual = residuals(coxfit.cat, type = 'deviance')
#     )
# ggplot(res.df, aes(prediction, residual)) + geom_point(position = 'jitter') +
#     geom_smooth()
# ggplot(res.df, aes(age, residual)) + geom_point() + geom_smooth()
# 
# ## ------------------------------------------------------------------------
# coeffs = tidy(coxfit.cat) %>%
#     transmute(term = term, 
#               estimate = exp(estimate),
#               conf.low = exp(conf.low),
#               conf.high = exp(conf.high)) %>%
#     mutate(term = str_replace(term, 'sex', ''),
#            term = str_replace(term, 'race.ethnicity', ''),
#            term = str_replace(term, 'education', ''),
#            term = str_replace(term, 'bmi.cat1', 'underweight'),
#            term = str_replace(term, 'bmi.cat3', 'overweight'),
#            term = str_replace(term, 'bmi.cat4', 'obese I'), 
#            term = str_replace(term, 'bmi.cat5', 'obese II')) %>%
#     mutate(term = factor(term, levels = term, ordered = TRUE), 
#            group = c('sex', 
#                      rep('race.ethnicity', 3), 
#                      rep('education', 4), 
#                      rep('bmi', 4)))
# 
# ggplot(coeffs, aes(term, estimate, color = group)) + 
#     geom_hline(yintercept = 1) +
#     geom_point() + 
#     geom_linerange(aes(ymin = conf.low, ymax = conf.high)) +
#     coord_flip()
# 
