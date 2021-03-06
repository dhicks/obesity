#' ---
#' title: "Analysis of BMI and All-Cause Morality in NHANES III and NHANES 1999-2004"
#' author: "Daniel J. Hicks and Catherine Womack"
#' email: hicks.daniel.j@gmail.com
#' ---
#'     
#' # Setup and Data #
#'     
#' We first load several required R packages and previously cleaned data.  Data come from NHANES III and continuous NHANES (cycles 1-3, 1999-2004), and followup mortality data from 2006 and 2011.  The models constructed below use only 2011 mortality data.  

## ----------------------------------------
#+ setup, message=FALSE, warning=FALSE
## tidyverse
    ## Changes to bind_rows() in dplyr 0.6.0 break the code used to extract predictions from the models:  
    ##  https://github.com/tidyverse/dplyr/issues/3134  
    ## The next few lines install dplyr 0.5.0, and load it before loading tidyverse.  
# withr::with_libpaths(new = paste0(getwd(), '/legacy_packages'), 
#                      code = devtools::install_version('dplyr', 
#                                                       version = '0.5.0', 
#                                                       repos = 'http://cran.us.r-project.org'))

withr::with_libpaths(new = paste0(getwd(), '/legacy_packages'), 
                     code = {library('dplyr')})

library(tidyverse)
library(cowplot)
library(broom)
library(stringr)
library(purrr)
library(purrrlyr)
## complex survey designs + survival analysis
library(survival)
library(survey)  ## NB version >= 3.32-2
    ## As of 2017-10-04, this needs to be installed from dev:  
    ## install.packages("survey", repos = "http://R-Forge.R-project.org")
## spline models
library(splines)
library(AUC)
## sortable table in HTML
library(DT)

load('2016-10-06.RData')
## TODO: why does't bmi_breaks come with names?
names(bmi_breaks) = c('NA', 'underweight', 'normal', 
                      'overweight', 'obese I', 'obese II')

age_cutoffs = c(50, 85) * 12 + 12
window = 5 * 12  # age window for cohort sample
## NB Age variables are measured in months

dataf_unfltd = dataf %>% 
    ## Drop entries with NA id or 0 sample weight
    filter(!is.na(id), sample.weight > 0) %>%
    mutate(
        ## Censor mortality at the upper cutoff: 
        ##  actual age at followup
        age.follow = age.months + follow.months.2011,
        age.follow.years = age.follow %/% 12,
        ##  censor at upper cutoff
        age.follow.c = pmin(age.follow, age_cutoffs[2]),
        age.follow.c.years = age.follow.c %/% 12,
        ##  censored followup months
        follow.months.c = age.follow.c - age.months,
        ## censored mortality
        ## 0 = alive/censored, 1 = deceased
        mort.c = ifelse(age.follow < age_cutoffs[2], 
                        as.integer(mort.2011) - 1, 0),
        ## For cross-referencing, number the rows in the unfiltered dataset
        row.num = row_number())
rm(dataf)

#+ subset_data
## The data subsets used by the models
## 1. Censored dataset, designed for survival analysis
dataf_censored = subset(dataf_unfltd, (!smoker) & 
                   (age.months >= age_cutoffs[1]) & 
                   (age.months < age_cutoffs[2]) & 
                   (follow.months.2011 > 0) &
                   # (bmi.cat != 'underweight') &
                   (bmi < 75) & 
                   !is.na(bmi) &
                   !is.na(education) & !is.na(mort.c))

## 2. "Cohort" dataset, the kind you might try to use w/ GLM
dataf_cohort = subset(dataf_unfltd, (!smoker) & 
                          (nhanes.cycle == 0) &
                          ## Individuals w/ in the age window for the cohort
                          (age.months >= age_cutoffs[1]) & 
                          (age.months < age_cutoffs[1] + window) &
                          ## Who either aged our successfully or died
                          ((age.follow.c == age_cutoffs[2]) | (mort.c == 1)) &
                          (follow.months.2011 > 0) &
                          (bmi < 75) & 
                          !is.na(bmi) & 
                          !is.na(education) & !is.na(mort.c))

## descriptive statistics for each variable in the dataset
str(dataf_censored)
summary(dataf_censored)

str(dataf_cohort)
summary(dataf_cohort)

## NB With cutoffs of 50; 85 and a 5-year window, the cohort has only 64 participants
## We can increase the cohort using a lower upper cutoff, eg, 50; 65 gives 386 participants
## But this is still too small to build a good regression across all of our covariates

## Split into training and validation sets
train_frac = .7
set.seed(42)
dataf_censored_train = dataf_censored %>%
    mutate(psu_ = str_c(stratum, '_', psu)) %>%
    group_by(psu_) %>%
    sample_frac(size = train_frac, weight = 1/sample.weight)
dataf_censored_valid = dataf_censored %>%
    filter(!(row.num %in% dataf_censored_train$row.num))

## Move to survey design objects
design_unfltd = svydesign(id = ~ psu, strata = ~ stratum, 
                          weights = ~ sample.weight, 
                          nest = TRUE, 
                          data = dataf_unfltd)
design_censored = subset(design_unfltd, row.num %in% dataf_censored$row.num)
## NB Two routes to test and validation sets; both yield the same sample weights
# design_censored_train1 = subset(design_unfltd,
#                                 row.num %in% dataf_censored_train$row.num)
# design_censored_train2 = subset(design_censored,
#                                 row.num %in% dataf_censored_train$row.num)
# tibble(train1 = design_censored_train1$variables$sample.weight,
#        train2 = design_censored_train2$variables$sample.weight,
#        df = dataf_censored_train$sample.weight) %>%
#     mutate(same = train1 == train2) %>%
#     summarize(frac_same = sum(same) / n())
design_censored_train = subset(design_unfltd, 
                               row.num %in% dataf_censored_train$row.num)
design_censored_valid = subset(design_unfltd, 
                               row.num %in% dataf_censored_valid$row.num)

design_cohort = subset(design_unfltd, row.num %in% dataf_cohort$row.num)


## ----------------------------------------
#' # Model Fitting #
#' We examine three key decisions in constructing and evaluating models for these data:  
#' 
#' - *BMI*: (1) conventionally binned as "normal," "overweight," "obese I," and "obese II"; (2) as a continuous variable; (3) as a continuous variable with cubic splines on four knot points; (4) as a continuous variable with cubic splines on six knot points [see documentation for `splines::bs`]
#' - *Model specification*: (1) a Cox proportional hazards model; (2) a binomial regression with a logistic link function; (3) a Poisson regression, with a log link; and (4) an "ordinary" or "vanilla" linear regression, often called ordinary least-squares (OLS), with an identity link function. 
#' - *Model selection* or goodness-of-fit evaluation using (1) deviance, (2) accuracy, (3) F1, and (4) AUC. 
#' 
#' The first two sets of decisions give us a total of 16 different models.  
#' 
#' The following function constructs unevaluated expressions for each model, based on `variable` (how BMI is represented) and `specification` (which model specification is used).  

#+ build_expr
build_expr = function (dataset, variable, specification) {
    dataset = as.character(dataset)
    variable = as.character(variable)
    specification = as.character(specification)

    design = switch(dataset, 
                    'censored' = 'design_censored_train',
                    'cohort' = 'design_cohort')
        
    knots_4 = svyquantile(~ bmi, eval(parse(text = design)), 1/5*1:4) %>%
        round(digits = 2) %>%
        str_c(collapse = ', ') %>%
        str_c('c(', ., ')')
    knots_6 = svyquantile(~ bmi, eval(parse(text = design)), 1/7*1:6) %>%
        round(digits = 2) %>%
        str_c(collapse = ', ') %>%
        str_c('c(', ., ')')
    
    var = switch(variable, 
                 'binned' = 'bmi.cat',
                 'continuous' = 'bmi',
                 'square' = 'bmi + I(bmi^2)',
                 'cube' = 'bmi + I(bmi^2) + I(bmi^3)',
                 '4-spline' = str_c('bs(bmi, knots = ', 
                                    knots_4, ')'), 
                 '6-spline' = str_c('bs(bmi, knots = ', 
                                    knots_6, ')'))
    if (specification == 'cox') {
        rhs = str_c(var, ' + sex + race.ethnicity + education')
    } else {
        rhs = str_c(var, ' + sex + race.ethnicity + education')
    }
    
    response = switch(specification, 
                      'cox' = 'Surv(age.follow.c.years, mort.c)',
                      'linear' = 'mort.c',
                      'poisson' = 'mort.c',
                      'logistic' = 'mort.c',
                      'cloglog' = 'mort.c')
    form = str_c(response, ' ~ ', rhs)
    
    expr = switch(specification, 
                  'cox' = as.expression(str_c(
                      'svycoxph(', form, ', ',
                      'design = ', design, ')')),
                  'linear' = as.expression(str_c(
                      'svyglm(', form, ', ',
                      'design = ', design, ', ',
                      'family = gaussian(link = "identity"))')),
                  'logistic' = as.expression(str_c(
                      'svyglm(', form, ', ',
                      'design = ', design, ', ',
                      'family = quasibinomial(link = "logit"))')),
                  'poisson' = as.expression(str_c(
                      'svyglm(', form, ', ',
                      'design = ', design, ', ',
                      'family = quasipoisson(link = "log"))')),
                  'cloglog' = as.expression(str_c(
                      'svyglm(', form, ', ',
                      'design = ', design, ', ',
                      'family = quasibinomial(link = "cloglog"))'))
    )
    
    return(parse(text = expr))
}

#' The calls are kept with the model metadata, which is useful, e.g., for confirming that `build_expr` works as expected. 
## NB The next line uses only the `censored` dataset.  If other datasets are used, modifications will be needed when OOS accuracy statistics are calculated
models_df = cross_df(list('dataset' = forcats::as_factor(c('censored')),
                          'covariate' = forcats::as_factor(
                                            c('binned', 
                                              'continuous', 
                                              'square',
                                              '4-spline')),
                         'model' = forcats::as_factor(
                                            c('linear', 
                                              'logistic', 
                                              'poisson', 
                                              'cox')))) %>%
    # filter((specification != 'cox') | (dataset == 'censored')) %>%
    mutate(model_id = row_number()) %>%
    select(model_id, everything()) %>%
    by_row(function (df) build_expr(df$dataset, 
                                    df$covariate, 
                                    df$model), 
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
            ggplot(.x, aes(term, estimate, color = covariate, 
                           linetype = model, 
                           shape = model)) + 
            # geom_point(position = position_dodge(width = .25)) +
            geom_pointrange(aes(ymin = conf.low, 
                                ymax = conf.high), 
                            position = position_dodge(width = .75)) +
            facet_grid(dataset ~ ., scales = 'free') +
            coord_flip()
    )

#' The BMI plot shows the incommensurability of the different BMI variables.  The estimate for continuous BMI is the estimated effect of a one-unit increase of BMI.  The categorical estimates are relative to the reference level, in this case "normal" weight (`bmi.cat2`).  The spline variables each have $n+k$ parameters, where $n$ is the number of knots and $k$ is the dimension of the basis functions (here $k=3$).  (B-spline bases are usually presented in terms of $n+k+1$ parameters, where the $+1$ corresponds to the constant or intercept.  In our models the intercept is handled by the intercept term of the regression.)  The values of these parameters do not appear to have a simple physical interpretation.  Further, it also does not make sense to compare these parameters across two different spline bases, i.e., between the 4- and 6-knot bases.  
#' 
#' By contrast, the coefficient and intercept plots may appear to be commensurable.  But this is also a mistake.  The coefficients do represent the additive contribution of the regressors to the right-hand-side of the regression equation.  But the link between the response and regression is different in each case.  The linear model is mathematically simplest, and we assume already familiar to our audience:  each unit increase in the regressor corresponds to an additive change of the response, by the coefficient.  In the logistic regression, the response is the probability of the event occuring (namely, the participant dies), and this is linked to the regressors using the logit or log-odds function:  
#' \[ p(y = 1) = logit^{-1}(\beta_0 + \beta_1 x_1 + \cdots \beta_n x_n), \]
#' where 
#' \[logit (x) = \log\frac{x}{1-x}.\]
#' Thus, in a logistic regression, a one-unit change in the regressor corresponds to a *multiplicative* change in the *log odds* of the response.  
#' 
#' In Poisson regression, the response is simply the response (i.e., not a probability), but it is linked to the regressors using a logarithm:  
#' \[ y = e^{\beta_0 + \beta_1 x_1 + \cdots + \beta_n x_n}.\]
#' Here a one-unit change in the regressor corresponds to a multiplicative change — as with the logistic regression — but now in the response itself.  
#' 
#' Finally, in a Cox proportional hazards model the response is a hazard function $h(t)$, giving the probability of an event occurring (i.e., participant death) after $t$ amount of time has passed.  The simplest hazard regression attempts to model this using a "baseline" hazard function and multiplicative change:  
#' \[ h(t) = h_0(t) e^{\beta_1 x_1 + \cdots + \beta_n x_n}.\]
#' Note that (1) when $x_i = 0$ for all of the regressors $x_i$, $h(t) = h_0(t)$; and (2) there is no intercept term $\beta_0$, since this is incorporated into the baseline $h_0(t)$.  However, estimating the baseline hazard function can be difficult.  Cox's innovation was to estimate the proportional hazard instead:  
#' \[ \frac{h(t)}{h_0(t)} = e^{\beta_1 x_1 + \cdots + \beta_n x_n}.\]
#' The right-hand side of this regression equation is similar to that of the Poisson regression.  And, in both cases, a one-unit change in a regressor corresponds to a multiplicative change in the response.  But the response variables on the left-hand side are different. So the coefficients from these two models are also incommensurable.  

## ----------------------------------------
#' ## Model Accuracy ##
#' Again, we are interested in model evaluation/selection using 4 different statistics:  (1) AIC, (2) accuracy, (3) F1, and (4) AUC. 

## `survey` 3.32-2 now includes an `extractAIC` method for Cox PH
## Then calculate AICs
models_df = models %>% 
    map(extractAIC) %>%
    map(~ .x[['AIC']]) %>%
    combine() %>%
    tibble(model_id = 1:length(.), 
           AIC = .) %>%
    full_join(models_df, .)

## Use `augment` to generate predictions
aug = models %>%
    ## `augment.lm` has problems w/ newdata; tries to bind it to original dataframe?
    map2(ifelse(map(models, is, 'glm'), 'response', 'expected'), 
         ~ predict(.x, newdata = design_censored_valid$variables,
                   type = .y)) %>%
    map(~ bind_cols(dataf_censored_valid, tibble(.fitted = .x))) %>%
    bind_rows(.id = 'model_id') %>%
    ## Join with `models_df`
    mutate(model_id = as.integer(model_id)) %>%
    right_join(models_df, .) %>%
    ## For Cox models, survival probability = exp(-expected)
    mutate(prob = ifelse(model != 'cox', 
                         .fitted, 
                         1 - exp(-.fitted)))

## Mortality rates estimated from training and validation sets
mort_rate_train = with(design_censored_train$variables, 
     sum(sample.weight * mort.c) / sum(sample.weight))
mort_rate_valid = with(design_censored_valid$variables, 
     sum(sample.weight * mort.c) / sum(sample.weight))
## Accuracy, precision, recall, and F1 for null model
accuracy_null = mort_rate_train * mort_rate_valid + 
    (1 - mort_rate_train) * (1 - mort_rate_valid)
precision_null = mort_rate_train
recall_null = mort_rate_valid
f1_null = 2*1/(1/precision_null + 1/recall_null)
    

## Plot the distribution of probabilities; 
## use these to select an appropriate threshold
# ggplot(aug, aes(prob)) + 
#     stat_ecdf() + 
#     geom_hline(yintercept = 1 - sum(dataf$mort.c)/nrow(dataf)) +
#     facet_grid(variable ~ specification)
threshold = aug %>%
    group_by(model_id, dataset, covariate, model) %>%
    summarize(threshold = quantile(prob, 
                                   probs = 1 - mort_rate_train))

## Calculate evaluation statistics
## Accuracy, precision, recall, and F1 are calculated using the Horvitz-Thompson estimator (see Lumley 2010, 17ff, 22)
models_df = aug %>%
    filter(!is.na(mort.c)) %>%
    left_join(threshold) %>%
    mutate(prediction = ifelse(prob > threshold, 1, 0)) %>% 
    group_by(dataset, covariate, model) %>%
    summarize(n_hat = sum(sample.weight), 
              accuracy = 1/n_hat * 
                  sum((prediction == mort.c) * sample.weight), 
              accuracy_rel = accuracy - accuracy_null,
              precision = sum(prediction * mort.c * sample.weight) /
                  sum(mort.c * sample.weight), 
              precision_rel = precision - precision_null,
              recall = sum(prediction * mort.c * sample.weight) / 
                  sum(prediction * sample.weight), 
              recall_rel = recall - recall_null,
              f1 = 2*1/(1/precision + 1/recall),
              f1_rel = f1 - f1_null,
              roc_curve = list(roc(prob, as.factor(mort.c))),
              auroc = {map(roc_curve, auc) %>% unlist()}) %>%
    ungroup() %>%
    full_join(models_df, .)

## Plot ROC curves
roc_curves = models_df$roc_curve %>%
    map(tidy) %>%
    bind_rows(.id = 'model_id') %>%
    as_tibble() %>%
    mutate(model_id = as.integer(model_id)) %>%
    left_join(select(models_df, model_id, 
                     dataset, covariate, model))
ggplot(roc_curves, aes(fpr, tpr, 
                       group = interaction(dataset, 
                                           covariate, 
                                           model), 
                       color = model)) + 
    geom_line() +
    geom_segment(x = 0, y = 0, xend = 1, yend = 1,
                 linetype = 'dashed',
                 inherit.aes = FALSE) +
    facet_wrap(~ dataset)

## Sortable table FTW
models_df %>%
    select(dataset, covariate, model, 
           AIC,accuracy, f1, auroc) %>%
    ## 3 digits of precision
    mutate_at(c('AIC', 'accuracy', 'f1', 'auroc'), 
              funs(signif(., digits = 3))) %>%
    datatable(filter = 'top')

## Plot of evaluation statistics
pred_fid_plot = models_df %>%
    select(dataset, covariate, model, 
           AIC, accuracy, precision, recall, f1, auroc) %>%
    gather(statistic, score, AIC, precision, recall,
           accuracy:auroc) %>%
    ggplot(aes(dataset, score, 
               color = covariate, shape = model)) + 
    geom_point(position = position_dodge(width = .5)) + 
    # scale_x_continuous(breaks = NULL, limits = c(0,2), name = '') +
    scale_color_brewer(palette = 'Set1') +
    facet_wrap(~ statistic, scales = 'free')
pred_fid_plot

save_plot('01_pred_fit.png', pred_fid_plot, 
          base_width = 8, base_height = 4)

## Accuracy statistics relative to null model
rel_pred_fid_plot = pred_fid_plot %+% 
    {models_df %>%
        select(dataset, covariate, model, 
               accuracy_rel, precision_rel, recall_rel, f1_rel) %>%
        gather(statistic, score, accuracy_rel:f1_rel)} +
    geom_hline(yintercept = 0, linetype = 'dashed') +
    facet_wrap(~ statistic, scales = 'free', 
               labeller = as_labeller(function (x) {
                   x %>%
                       str_extract('[a-z]+1?') %>%
                       str_c('relative ', .)
               }))

save_plot('02_rel_pred_fit.png', rel_pred_fid_plot, 
          base_width = 6, base_height = 4)

#' The Cox model performs worst under all four metrics.  For the two accuracy statistics (accuracy itself and AUROC for the accuracy curve), the other three models are comparable; variable seems to be more important than model specification, with continuous BMI performing slightly worse by AUROC than the other variables across all three specifications.  F1 generally finds continuous variables and linear models to perform worse than alternatives; logistic and Poisson models with spline variables seem to perform the best, presumably because these models are more flexible for fitting non-linear trends. 
#' 
#' AIC should be interpreted with care.  For the generalized regression models, the response variable is whether or not the survey responded died.  On the other hand, for the Cox model, the response is the amount of time the respondent survived until either dying, being lost to followup, or aging out of the study.  So the AIC of the Cox model probably shouldn't be compared to the AIC of the other three models.  AICs for the three GLMs should be comparable.  
#' 
#' The distribution of AICs for the three GLMs contrast sharply with those of the other statistics.  Variable seems to matter little or not at all; the plot gives the impression that the AIC values are the same for a given specification, but the table indicates small differences.  Specifically, linear models dominate the other specifications (in the game-theoretic sense) according to AIC.  By contrast, linear models had roughly the same accuracy and AUROC scores, and tended to have the second-worst F1 scores.  
#' 
#' All together, these statistics underdetermine the choice of model; and incommensurability appears again, insofar as AIC cannot be calculated for the Cox models.  

## ----------------------------------------
#' ## Predictions ##
#' TODO: write some things! 
## Initialize a data frame to hold the predictions
predictions = expand.grid(
    bmi = seq(16, 45, .1),
    age.follow.c.years = svymean(~ age.follow.c.years, design_censored)[1],
    sex = factor('female', levels = levels(dataf_censored$sex)),
    race.ethnicity = factor('Non-Hispanic White', 
                            levels = levels(dataf_censored$race.ethnicity)), 
    education = factor('High School', 
                       levels = levels(dataf_censored$education)), 
    mort.c = 0)

## Get binned labels from the continuous BMI values
bmi_classify = function (bmi) {
    return(names(which(bmi <= bmi_breaks))[1])
}
bmi_classify = Vectorize(bmi_classify)
predictions = predictions %>%
    mutate(bmi.cat = {bmi %>% bmi_classify %>%
            factor(levels = names(bmi_breaks), ordered = FALSE) %>%
            C(treatment, base = 2)})

## NB There are small differences between Cox "risk" and "expected" predictions
## possibly standard errors are different? that's more complicated to check
# thing = tibble(risk = predict(models[[20]], predictions, type = 'risk'),
#                prob = 1 - exp(- predict(models[[20]], predictions, type = 'expected'))) %>%
#     bind_cols(predictions, .)
# 
# baselines = thing %>%
#     filter(bmi == 22) %>%
#     select(baseline_risk = risk, baseline_prob = prob)
# thing$baseline_risk = baselines$baseline_risk
# thing$baseline_prob = baselines$baseline_prob
# 
# thing %>%
#     mutate(risk_rel = risk / baseline_risk,
#            prob_rel = prob / baseline_prob) %>%
#     ggplot(aes(risk_rel, prob_rel)) + geom_point() +
#     stat_function(fun = function (x) x, color = 'red')

## Generate the predictions and arrange in a single dataframe
predictions = models %>%
    map_if(function (x) is(x, 'svycoxph'), 
           predict, predictions, type = 'risk', se = TRUE) %>%
    map_if(function (x) is(x, 'svyglm'),
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
                                    fill = covariate,
                                    linetype = model)) + 
    geom_hline(yintercept = 1) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
                alpha = .1) +
    geom_step(aes(color = covariate), size = 1) +
    geom_point(x = 22, y = 1, inherit.aes = FALSE) +
    scale_x_continuous(breaks = bmi_breaks, 
                       labels = bmi_breaks) +
    geom_vline(xintercept = bmi_breaks[2:5], alpha = .1) +
    scale_linetype_manual(values = c('solid', 'dashed', 'dotted', 'dotdash')) +
    coord_cartesian(ylim = c(.5, 2)) +
    ylab('relative risk')
pred_plot + facet_grid(dataset ~ covariate)
save_plot('03_rr_preds.png', pred_plot + facet_grid(dataset ~ covariate), 
          base_width = 10, base_height = 5)
pred_plot + facet_grid(dataset ~ model)
save_plot('04_rr_preds.png', pred_plot + facet_grid(dataset ~ model), 
          base_width = 10, base_height = 5)

#' Both plots show the same set of 16 predictions.  The first plot groups the predictions by variable, allowing us to compare different model specifications.  Above the "normal" BMI range, the Cox model generally produces much higher risk estimates than the other three.  For the spline variables, all of the models see a dramatic increase at the left and right edges of the plot; this is due in part to the basis vectors, which act as simple cubics as we approach the edges of the data.  
#' 
#' Every model — except the four continuous-variable models — is consistent with reduced risk across the "overweight" range.  The non-Cox spline models extend this reduced risk until well into the "obese II' range.  The non-Cox models of the binned and continuous variables show an elevated — but arguably relatively small — risk in "obese I."  Many of the individual curves find evidence to support the "obesity paradox."  
#' 
#' The second plot groups models by specification, allowing us to compare different representations of BMI.  Two patterns stand out here.  First, the two spline variable curves are generally quite close within each specification, even though they different quite a bit from the other variables (especially at the edges of the data).  Second, the Cox model is clearly the most consistent across the four variables, especially at greater BMI.  


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

sessionInfo()
