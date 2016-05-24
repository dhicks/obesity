library(dplyr)
library(survey)
library(survival)

load('2016-05-24.Rdata')

df = df[complete.cases(df),]

## ----------
## W/o sampling weights
df.working = df %>% filter(!smoker, age.months >= 50*12, age.months < 85*12)
coxfit.unweighted = coxph(Surv(age.months, mort.status == 'deceased') ~ 
						  	bmi.cat * bmi.max.cat,
				   data = df.working)
summary(coxfit.unweighted)


## ----------
## W/ sampling weights
## cf the svydesign call in the docs for survey::nhanes
design = svydesign(id = ~ psu, strata = ~ stratum, weights = ~ sample.weight, 
				   nest = TRUE, 
				   data = {df %>% filter(!smoker, age.months >= 50*12, age.months < 85*12)})
coxfit.weighted = svycoxph(Surv(age.months, mort.status == 'deceased') ~ 
						   	bmi.cat * bmi.max.cat, 
				 design = design)

